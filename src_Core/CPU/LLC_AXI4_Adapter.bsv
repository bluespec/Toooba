//-
// RVFI_DII + CHERI modifications:
//     Copyright (c) 2020 Alexandre Joannou
//     Copyright (c) 2020 Jonathan Woodruff
//     All rights reserved.
//
//     This software was developed by SRI International and the University of
//     Cambridge Computer Laboratory (Department of Computer Science and
//     Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
//     DARPA SSITH research programme.
//
//     This work was supported by NCSC programme grant 4212611/RFA 15971 ("SafeBet").
//-

package LLC_AXI4_Adapter;

// ================================================================
// BSV lib imports

import ConfigReg :: *;
import Assert    :: *;
import FIFOF     :: *;
import Vector    :: *;

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

import Types       :: *;
import CacheUtils  :: *;
import CCTypes     :: *;

// ----------------
// From Bluespec Pipes

import AXI4 :: *;
import SourceSink :: *;
import Fabric_Defs  :: *;
import SoC_Map      :: *;

// ================================================================

interface LLC_AXI4_Adapter_IFC;
   method Action reset;

   // Fabric master interface for memory
   interface AXI4_Master #(Wd_MId, Wd_Addr, Wd_Data,
                           Wd_AW_User, Wd_W_User, Wd_B_User,
                           Wd_AR_User, Wd_R_User) mem_master;
endinterface

// ================================================================

module mkLLC_AXi4_Adapter #(MemFifoClient #(idT, childT) llc)
                          (LLC_AXI4_Adapter_IFC)
   provisos(Bits#(idT, a__),
            Bits#(childT, b__),
            FShow#(ToMemMsg#(idT, childT)),
            FShow#(MemRsMsg#(idT, childT)),
            Add#(SizeOf#(Line), 0, TAdd#(512, 4))); // assert Line sz = 512 + 4 tags

   // Verbosity: 0: quiet; 1: LLC transactions; 2: loop detail
   Integer verbosity = 2;
   Reg #(Bit #(4)) cfg_verbosity <- mkConfigReg (fromInteger (verbosity));

   // ================================================================
   // Fabric request/response

   let masterPortShim <- mkAXI4ShimFF;

   // For discarding write-responses
   CreditCounter_IFC #(4) ctr_wr_rsps_pending <- mkCreditCounter; // Max 15 writes outstanding

   // ================================================================
   // Functions to interact with the fabric

   // Send a read-request into the fabric
   function Action fa_fabric_send_read_req (Fabric_Addr  addr);
      action
         let mem_req_rd_addr = AXI4_ARFlit {arid:     fabric_default_mid,
                                            araddr:   addr,
                                            arlen:    0,           // burst len = arlen+1
                                            arsize:   64,
                                            arburst:  INCR,
                                            arlock:   fabric_default_lock,
                                            arcache:  fabric_default_arcache,
                                            arprot:   fabric_default_prot,
                                            arqos:    fabric_default_qos,
                                            arregion: fabric_default_region,
                                            aruser:   fabric_default_aruser};

         masterPortShim.slave.ar.put(mem_req_rd_addr);

         // Debugging
         if (cfg_verbosity > 1) begin
            $display ("    ", fshow (mem_req_rd_addr));
         end
      endaction
   endfunction

   // ================================================================
   // Handle read requests and responses
   // Don't do reads while writes are outstanding.

   // Each 512b cache line takes 8 beats, each handling 64 bits
   Reg #(Bit #(3)) rg_rd_rsp_beat <- mkReg (0);

   FIFOF #(LdMemRq #(idT, childT)) f_pending_reads <- mkFIFOF;
   Reg #(CLine) rg_cline <- mkRegU;

   rule rl_handle_read_req (llc.toM.first matches tagged Ld .ld
                            &&& (ctr_wr_rsps_pending.value == 0));
      if ((cfg_verbosity > 0)) begin
         $display ("%0d: LLC_AXI4_Adapter.rl_handle_read_req: Ld request from LLC to memory",
                   cur_cycle);
         $display ("    ", fshow (ld));
      end

      Addr  line_addr = {ld.addr [63:6], 6'h0 };                      // Addr of containing cache line
      fa_fabric_send_read_req (line_addr);
      f_pending_reads.enq (ld);
      llc.toM.deq;
   endrule

   rule rl_handle_read_rsps;
      let mem_rsp <- get(masterPortShim.slave.r);
      if (cfg_verbosity > 1) begin
         $display ("%0d: LLC_AXI4_Adapter.rl_handle_read_rsps: beat %0d ", cur_cycle, rg_rd_rsp_beat);
         $display ("    ", fshow (mem_rsp));
      end
      if (mem_rsp.rresp != OKAY) begin
         // TODO: need to raise a non-maskable interrupt (NMI) here
         $display ("%0d: LLC_AXI4_Adapter.rl_handle_read_rsp: fabric response error; exit", cur_cycle);
         $display ("    ", fshow (mem_rsp));
         $finish (1);
      end
      let new_cline = CLine { tag: unpack(mem_rsp.ruser)
                            , data: unpack(mem_rsp.rdata) };
      let ldreq <- pop (f_pending_reads);
      MemRsMsg #(idT, childT) resp = MemRsMsg {data:  new_cline,
                                              child: ldreq.child,
                                              id:    ldreq.id};
      llc.rsFromM.enq (resp);
      if (cfg_verbosity > 1)
        $display ("    Response to LLC: ", fshow (resp));
   endrule

   // ================================================================
   // Handle write requests and responses

   rule rl_handle_write_req (llc.toM.first matches tagged Wb .wb);
      if (cfg_verbosity > 0) begin
         $display ("%d: LLC_AXI4_Adapter.rl_handle_write_req: Wb request from LLC to memory:", cur_cycle);
         $display ("    ", fshow (wb));
      end

      // send AXI4 AW flit
      masterPortShim.slave.aw.put (AXI4_AWFlit {
        awid:     fabric_default_mid,
        awaddr:   { wb.addr [63:6], 6'h0 },
        awlen:    0, // burst len = awlen+1
        awsize:   64,
        awburst:  INCR,
        awlock:   fabric_default_lock,
        awcache:  fabric_default_awcache,
        awprot:   fabric_default_prot,
        awqos:    fabric_default_qos,
        awregion: fabric_default_region,
        awuser:   0});
      // Expect a fabric response
      ctr_wr_rsps_pending.incr;
      llc.toM.deq;


      // on each flit ...
      // ================
      Vector #(8, Bit #(8)) line_strb = unpack(pack(wb.byteEn));
      Vector #(4, MemTaggedData) line_data = clineToMemTaggedDataVector(wb.data);
      // send AXI4 W flit
      masterPortShim.slave.w.put(AXI4_WFlit {
        wdata:  pack(wb.data.data),
        wstrb:  pack(wb.byteEn),
        wlast:  True,
        wuser:  pack(wb.data.tag)});
   endrule

   // ----------------
   // Discard write-responses from the fabric

   rule rl_discard_write_rsp;
      let wr_resp <- get(masterPortShim.slave.b);

      if (ctr_wr_rsps_pending.value == 0) begin
         $display ("%0d: ERROR: LLC_AXI4_Adapter.rl_discard_write_rsp: unexpected Wr response (ctr_wr_rsps_pending.value == 0)",
                   cur_cycle);
         $display ("    ", fshow (wr_resp));
         $finish (1);    // Assertion failure
      end

      ctr_wr_rsps_pending.decr;

      if (wr_resp.bresp != OKAY) begin
         // TODO: need to raise a non-maskable interrupt (NMI) here
         $display ("%0d: LLC_AXI4_Adapter.rl_discard_write_rsp: fabric response error: exit", cur_cycle);
         $display ("    ", fshow (wr_resp));
         $finish (1);
      end
   endrule

   // ================================================================
   // INTERFACE

   method Action reset;
      ctr_wr_rsps_pending.clear;
   endmethod

   // Fabric interface for memory
   interface mem_master = masterPortShim.master;
endmodule

// ================================================================

endpackage
