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
   interface AXI4_Master_Synth #(Wd_MId_2x3, Wd_Addr, Wd_Data,
                                 Wd_AW_User, Wd_W_User, Wd_B_User,
                                 Wd_AR_User, Wd_R_User) mmio_master;
endinterface

// ================================================================

module mkMMIO_AXI4_Adapter (MMIO_AXI4_Adapter_IFC);

   // Verbosity: 0: quiet; 1: transactions
   Integer verbosity = 0;
   Reg #(Bit #(4)) cfg_verbosity <- mkConfigReg (fromInteger (verbosity));

   // ================================================================
   // Requests from and responses to core

   FIFOF #(MMIOCRq)     f_reqs_from_core <- mkFIFOF;
   FIFOF #(MMIODataPRs) f_rsps_to_core   <- mkFIFOF;

   SoC_Map_IFC  soc_map <- mkSoC_Map;    // for m_is_IO_addr

   // ================================================================
   // Fabric request/response

   let master_xactor <- mkAXI4_Master_Xactor;

   // For discarding write-responses
   CreditCounter_IFC #(4) ctr_wr_rsps_pending <- mkCreditCounter; // Max 15 writes outstanding

   // ================================================================
   // Functions to interact with the fabric

   // Send a read-request into the fabric
   function Action fa_fabric_send_read_req (Fabric_Addr  addr);
      action
         AXI4_Size  size = 8;
         let mem_req_rd_addr = AXI4_ARFlit {arid:     fabric_2x3_default_mid,
                                             araddr:   addr,
                                             arlen:    0,           // burst len = arlen+1
                                             arsize:   size,
                                             arburst:  fabric_default_burst,
                                             arlock:   fabric_default_lock,
                                             arcache:  fabric_default_arcache,
                                             arprot:   fabric_default_prot,
                                             arqos:    fabric_default_qos,
                                             arregion: fabric_default_region,
                                             aruser:   fabric_default_aruser};

         master_xactor.slave.ar.put(mem_req_rd_addr);

         // Debugging
         if (cfg_verbosity > 0) begin
            $display ("    ", fshow (mem_req_rd_addr));
         end
      endaction
   endfunction

   // Send a write-request into the fabric
   function Action fa_fabric_send_write_req (Fabric_Addr  addr, Fabric_Strb  strb, Bit #(64)  st_val);
      action
         AXI4_Size  size = 8;
         let mem_req_wr_addr = AXI4_AWFlit {awid:     fabric_2x3_default_mid,
                                             awaddr:   addr,
                                             awlen:    0,           // burst len = awlen+1
                                             awsize:   size,
                                             awburst:  fabric_default_burst,
                                             awlock:   fabric_default_lock,
                                             awcache:  fabric_default_awcache,
                                             awprot:   fabric_default_prot,
                                             awqos:    fabric_default_qos,
                                             awregion: fabric_default_region,
                                             awuser:   0};

         let mem_req_wr_data = AXI4_WFlit {wdata:  st_val,
                                             wstrb:  strb,
                                             wlast:  True,
                                             wuser:  0};

`ifdef FABRIC64
         // Work-around for a misbehavior on Xilinx UART and its
         // Xilinx AXI4 adapter. On 64-bit fabrics, for a write where
         // axsize says '8 bytes' but wstrb is for <= 4 bytes, the
         // adapter converts it two 32-bit writes, one of which has
         // wstrb=4'b0000. The Xilinx UART, in turn ignores wstrb and
         // therefore performs a spurious write.  This workaround
         // changes axsize for such writes to '4 bytes', avoiding this
         // problem.

         if (strb [7:4] == 0 || strb [3:0] == 0) begin
            mem_req_wr_addr.awsize = 4;
         end
`endif

   master_xactor.slave.aw.put (mem_req_wr_addr);
   master_xactor.slave.w.put (mem_req_wr_data);

         // Expect a fabric response
         ctr_wr_rsps_pending.incr;

         // Debugging
         /*
         if (cfg_verbosity > 0) begin
            $display ("    To fabric: ", fshow (mem_req_wr_addr));
            $display ("               ", fshow (mem_req_wr_data));
         end
         */
      endaction
   endfunction

   // ================================================================
   // Handle read requests and responses.
   // Don't do a read while a write is outstanding.
   // This is just an adapter from MMIOCRq/MMIODataPRs to AXI4

   rule rl_handle_read_req (f_reqs_from_core.first.func matches Ld
                            &&& (ctr_wr_rsps_pending.value == 0));
      let req <- pop (f_reqs_from_core);

      if (cfg_verbosity > 0) begin
         $display ("%0d: %m.rl_handle_read_req: Ld request", cur_cycle);
         $display ("    ", fshow (req));
      end

      // Technically the following check for legal IO addrs is not
      // necessary; the AXI4 fabric should return a DECERR for illegal
      // addrs; but not all AXI4 fabrics do the right thing.
      if (soc_map.m_is_IO_addr (req.addr))
         fa_fabric_send_read_req (req.addr);
      else begin
         let rsp = MMIODataPRs {valid: False,
                                data: req.addr};    // For debugging convenience only
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
      let mem_rsp <- get(master_xactor.slave.r);

      if (cfg_verbosity > 0) begin
         $display ("%0d: %m.rl_handle_read_rsps ", cur_cycle);
         $display ("    ", fshow (mem_rsp));
      end

      if ((cfg_verbosity > 0) && (mem_rsp.rresp != OKAY)) begin
         $display ("%0d: %m.rl_handle_read_rsp: fabric response error", cur_cycle);
         $display ("    ", fshow (mem_rsp));
      end

      let rsp = MMIODataPRs {valid: (mem_rsp.rresp == OKAY),
                             data: mem_rsp.rdata};
      f_rsps_to_core.enq (rsp);

      if (cfg_verbosity > 0)
         $display ("    Response MMIO to core: ", fshow (rsp));
   endrule

   // ================================================================
   // Handle write requests and responses

   rule rl_handle_write_req (f_reqs_from_core.first.func matches St);
      let req <- pop (f_reqs_from_core);

      if (cfg_verbosity > 0) begin
         $display ("%d: %m.rl_handle_write_req: St request:", cur_cycle);
         $display ("    ", fshow (req));
      end

      // Technically the following check for legal IO addrs is not
      // necessary; the AXI4 fabric should return a DECERR for illegal
      // addrs; but not all AXI4 fabrics do the right thing.
      if (soc_map.m_is_IO_addr (req.addr))
         fa_fabric_send_write_req (req.addr, pack (req.byteEn), req.data);
      else begin
         let rsp = MMIODataPRs {valid: False,
                                data: req.addr};    // For debugging convenience only
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
      let wr_resp <- get(master_xactor.slave.b);

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
      else begin
         let rsp = MMIODataPRs {valid: True, data: 0};
         f_rsps_to_core.enq (rsp);
      end
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
   interface mmio_master = master_xactor.masterSynth;
endmodule

// ================================================================

endpackage
