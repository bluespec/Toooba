// Copyright (c) 2019-2020 Bluespec, Inc.

package MMIO_AXI4_Adapter;

// ================================================================
// This is an adapter to connect MIT's RISCY-OOO to an AXI4 fabric in
// Bluespec's Toooba setup. All IO traffic to the fabric flows through
// this.  Note: a few IO addresses (e.g., MTIME, MTIMECMP, MSIP,
// TOHOST, FROMHOST are intercepted and handled before they reach this
// adapter).

// ================================================================
// BSV lib imports

import Assert       :: *;
import ConfigReg    :: *;
import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;
import Vector       :: *;

// ----------------
// BSV additional libs

import GetPut_Aux     :: *;
import Cur_Cycle      :: *;
import Semi_FIFOF     :: *;
import CreditCounter  :: *;

// ================================================================
// Project imports

// ----------------
// From MIT RISCY-OOO

import Types :: *;
import ProcTypes :: *;

// ----------------
// From Bluespec Pipes

import AXI4 :: *;
import SourceSink :: *;
import Fabric_Defs  :: *;
import SoC_Map      :: *;

// ================================================================

interface MMIO_AXI4_Adapter_IFC;
   method Action reset;

   interface Server #(MMIOCRq, MMIODataPRs) core_side;

   // Fabric master interface for IO
   interface AXI4_Master #(Wd_MId_2x3, Wd_Addr, Wd_Data,
                           Wd_AW_User, Wd_W_User, Wd_B_User,
                           Wd_AR_User, Wd_R_User) mmio_master;
endinterface

// ================================================================

module mkMMIO_AXI4_Adapter (MMIO_AXI4_Adapter_IFC);

   // Verbosity: 0: quiet; 1: transactions
   Integer verbosity = 1;
   Reg #(Bit #(4)) cfg_verbosity <- mkConfigReg (fromInteger (verbosity));

   // ================================================================
   // Requests from and responses to core

   FIFOF #(MMIOCRq)     f_reqs_from_core <- mkFIFOF;
   FIFOF #(MMIODataPRs) f_rsps_to_core   <- mkFIFOF;
   Reg#(Fabric_Addr)    read_req_addr    <- mkRegU;

   SoC_Map_IFC  soc_map <- mkSoC_Map;    // for m_is_IO_addr

   // ================================================================
   // Fabric request/response

   let master_shim <- mkAXI4ShimFF;

   // For discarding write-responses
   CreditCounter_IFC #(4) ctr_wr_rsps_pending <- mkCreditCounter; // Max 15 writes outstanding

   // ================================================================
   // Handle read requests and responses.
   // Don't do a read while a write is outstanding.
   // This is just an adapter from MMIOCRq/MMIODataPRs to AXI4
   Reg #(Bit #(1)) rg_rd_rsp_beat <- mkReg (0);
   Reg #(MemTaggedData) rspData <- mkReg (unpack(0));

   rule rl_handle_read_req (f_reqs_from_core.first.func matches Ld
                            &&& (ctr_wr_rsps_pending.value == 0));
      let req <- pop (f_reqs_from_core);

      if (cfg_verbosity > 0) begin
         $display ("%0d: %m.rl_handle_read_req: Ld request", cur_cycle);
         $display ("    ", fshow (req));
      end

      Vector #(2, Bit #(8)) line_strb = unpack(pack(req.byteEn));
      Bool burst = line_strb[0]!=0 && line_strb[1]!=0;

      // Technically the following check for legal IO addrs is not
      // necessary; the AXI4 fabric should return a DECERR for illegal
      // addrs; but not all AXI4 fabrics do the right thing.
      if (soc_map.m_is_IO_addr (req.addr)) begin
         AXI4_Size size = 8;
         let mem_req_rd_addr = AXI4_ARFlit {arid:     fabric_2x3_default_mid,
                                            araddr:   req.addr,
                                            arlen:    (burst) ? 1:0,           // burst len = arlen+1
                                            arsize:   size,
                                            arburst:  fabric_default_burst,
                                            arlock:   fabric_default_lock,
                                            arcache:  fabric_default_arcache,
                                            arprot:   fabric_default_prot,
                                            arqos:    fabric_default_qos,
                                            arregion: fabric_default_region,
                                            aruser:   fabric_default_aruser};

         master_shim.slave.ar.put(mem_req_rd_addr);
         read_req_addr <= req.addr;

          // Debugging
         if (cfg_verbosity > 0) begin
            $display ("    ", fshow (mem_req_rd_addr));
         end
      end else begin
         let rsp = MMIODataPRs {valid: False,
                                data: toMemTaggedData(req.addr)};    // For debugging convenience only
         f_rsps_to_core.enq (rsp);
         if (cfg_verbosity > 0) begin
            $display ("%0d: %m.rl_handle_read_req: unmapped IO address; returning error response",
                      cur_cycle);
            $display ("    ", fshow (req));
         end
      end
   endrule

   // ----------------

   rule rl_handle_read_rsps;
      let mem_rsp <- get(master_shim.slave.r);

      if (cfg_verbosity > 0) begin
         $display ("%0d: %m.rl_handle_read_rsps ", cur_cycle);
         $display ("    ", fshow (mem_rsp));
      end

      if ((cfg_verbosity > 0) && (mem_rsp.rresp != OKAY)) begin
         $display ("%0d: %m.rl_handle_read_rsp: fabric response error", cur_cycle);
         $display ("    ", fshow (mem_rsp));
      end

      let newData = rspData;
      newData.data[read_req_addr[3]+rg_rd_rsp_beat] = mem_rsp.rdata;
      let rsp = MMIODataPRs {valid: (mem_rsp.rresp == OKAY),
                             data: newData };
      if (mem_rsp.rlast) begin
        f_rsps_to_core.enq (rsp);
        if (cfg_verbosity > 0)
           $display ("    Response MMIO to core: ", fshow (rsp));
        rg_rd_rsp_beat <= 0;
        rspData <= unpack(0);
      end else begin
        rg_rd_rsp_beat <= rg_rd_rsp_beat + 1;
        rspData <= newData;
      end
   endrule

   // ================================================================
   // Handle write requests and responses

   // Each 128b word takes 2 beats, each handling 64 bits
   Reg #(Bit #(1)) rg_wr_req_beat <- mkReg (0);

   rule rl_handle_write_req (f_reqs_from_core.first.func matches St);
      let req =  f_reqs_from_core.first;

      if (cfg_verbosity > 0) begin
         $display ("%d: %m.rl_handle_write_req: St request:", cur_cycle);
         $display ("    ", fshow (req));
      end

      Vector #(2, Bit #(8)) line_strb = unpack(pack(req.byteEn));
      Bool burst = line_strb[0]!=0 && line_strb[1]!=0;
      Bit#(1) whichHalf = rg_wr_req_beat;
      if (!burst) whichHalf = (line_strb[0]!=0) ? 0:1;
      Bool first = (burst) ? whichHalf==0:True;
      Bool last = (burst) ? whichHalf==1:True;

      // Technically the following check for legal IO addrs is not
      // necessary; the AXI4 fabric should return a DECERR for illegal
      // addrs; but not all AXI4 fabrics do the right thing.
      if (soc_map.m_is_IO_addr (req.addr)) begin
         //fa_fabric_send_write_req (req.addr, truncate(pack(req.byteEn)), fromMemTaggedData(req.data));
         // on first flit...
         // ================
         if (first) begin
            AXI4_Size  size = 8;
            AXI4_AWFlit #(Wd_MId_2x3, Wd_Addr, Wd_AW_User)
                mem_req_wr_addr = AXI4_AWFlit {awid:     fabric_2x3_default_mid,
                                               awaddr:   req.addr,
                                               awlen:    (burst) ? 1:0,           // burst len = awlen+1
                                               awsize:   size,
                                               awburst:  fabric_default_burst,
                                               awlock:   fabric_default_lock,
                                               awcache:  fabric_default_awcache,
                                               awprot:   fabric_default_prot,
                                               awqos:    fabric_default_qos,
                                               awregion: fabric_default_region,
                                               awuser:   0};
`ifdef FABRIC64
            // Work-around for a misbehavior on Xilinx UART and its
            // Xilinx AXI4 adapter. On 64-bit fabrics, for a write where
            // axsize says '8 bytes' but wstrb is for <= 4 bytes, the
            // adapter converts it two 32-bit writes, one of which has
            // wstrb=4'b0000. The Xilinx UART, in turn ignores wstrb and
            // therefore performs a spurious write.  This workaround
            // changes axsize for such writes to '4 bytes', avoiding this
            // problem.

            if (countOnes(pack(req.byteEn)) <= 4)
               mem_req_wr_addr.awsize = 4;
`endif
            master_shim.slave.aw.put (mem_req_wr_addr);
            if (cfg_verbosity > 0) begin
               $display ("%d: %m.rl_handle_write_req: sent aw flit:", cur_cycle);
               $display ("    ", fshow (mem_req_wr_addr));
            end
            // Expect a fabric response
            ctr_wr_rsps_pending.incr;
         end

         // on last flit...
         // ===============
         if (last) begin
            f_reqs_from_core.deq;
            rg_wr_req_beat <= 0;
         end else // increment flit counter
            rg_wr_req_beat <= rg_wr_req_beat + 1;

        // on each flit...
        // ===============
        AXI4_WFlit #(Wd_Data, Wd_W_User)
            wflit = AXI4_WFlit {wdata:  req.data.data[whichHalf],
                                wstrb:  line_strb[whichHalf],
                                wlast:  last,
                                wuser:  0};
        master_shim.slave.w.put (wflit);
        if (cfg_verbosity > 0) begin
           $display ("%d: %m.rl_handle_write_req: sent w flit:", cur_cycle);
           $display ("    ", fshow (wflit), "first: %d, last: %d, whichHalf: %d", first, last, whichHalf);
        end
      end else begin
         let rsp = MMIODataPRs {valid: False,
                                data: toMemTaggedData(req.addr)};    // For debugging convenience only
         f_reqs_from_core.deq;
         f_rsps_to_core.enq (rsp);
         if (cfg_verbosity > 0) begin
            $display ("%0d: %m.rl_handle_write_req: unmapped IO address; returning error response",
                      cur_cycle);
            $display ("    ", fshow (req));
         end
      end
   endrule

   // ----------------
   // Discard write-responses from the fabric

   rule rl_discard_write_rsp;
      let wr_resp <- get(master_shim.slave.b);

      if (cfg_verbosity > 0) begin
         $display ("%0d: %m.rl_discard_write_rsp", cur_cycle);
         $display ("    ", fshow (wr_resp));
      end

      if (ctr_wr_rsps_pending.value == 0) begin
         $display ("%0d:%m.rl_discard_write_rsp: ERROR:unexpected Wr response (ctr_wr_rsps_pending.value == 0)",
                   cur_cycle);
         $display ("    ", fshow (wr_resp));
         $finish (1);    // Assertion failure
      end

      ctr_wr_rsps_pending.decr;

      if (wr_resp.bresp != OKAY) begin
         // TODO: need to raise a non-maskable interrupt (NMI) here
         $display ("%0d:%m.rl_discard_write_rsp: ERROR: fabric response error: exit.", cur_cycle);
         $display ("    ", fshow (wr_resp));
         $finish (1);
      end
      f_rsps_to_core.enq (MMIODataPRs {valid: wr_resp.bresp == OKAY, data: 0});
   endrule

   // ================================================================
   // This adapter should only receive Ld/St requests, no Inst or AMO reqs.

   function Bool fn_is_Ld_or_St (MMIOCRq  req);
      return case (req.func) matches
                Ld     : True;
                St     : True;
                default: False;
             endcase;
   endfunction

   rule rl_handle_non_Ld_St (! fn_is_Ld_or_St (f_reqs_from_core.first));
      let req <- pop (f_reqs_from_core);

      $display ("%0d:%m.rl_handle_non_Ld_St: ERROR: neither Ld nor St? exit.", cur_cycle);
      $display ("    ", fshow (req));
      $finish (1);    // Assertion failure
   endrule

   // ================================================================
   // INTERFACE

   method Action reset;
      ctr_wr_rsps_pending.clear;
   endmethod

   interface Server core_side = toGPServer (f_reqs_from_core, f_rsps_to_core);

   // Fabric master interface for IO
   interface mmio_master = master_shim.master;
endmodule

// ================================================================

endpackage
