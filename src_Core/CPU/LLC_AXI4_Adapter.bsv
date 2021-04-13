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

import AXI4_Types   :: *;
import Fabric_Defs  :: *;

// ================================================================

interface LLC_AXI4_Adapter_IFC;
   method Action reset;

   // Fabric master interface for memory
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) mem_master;
endinterface

// ================================================================

module mkLLC_AXi4_Adapter #(MemFifoClient #(idT, childT) llc)
                          (LLC_AXI4_Adapter_IFC)
   provisos(Bits#(idT, a__),
	    Bits#(childT, b__),
	    FShow#(ToMemMsg#(idT, childT)),
	    FShow#(MemRsMsg#(idT, childT)),
	    Add#(SizeOf#(Line), 0, 512)); // assert Line sz = 512

   // Verbosity: 0: quiet; 1: LLC transactions; 2: loop detail
   Integer verbosity = 0;
   Reg #(Bit #(4)) cfg_verbosity <- mkConfigReg (fromInteger (verbosity));

   // ================================================================
   // Fabric request/response

   AXI4_Master_Xactor_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) master_xactor <- mkAXI4_Master_Xactor_2;

   // For discarding write-responses
   CreditCounter_IFC #(4) ctr_wr_rsps_pending <- mkCreditCounter; // Max 15 writes outstanding

   // ================================================================
   // Functions to interact with the fabric

   // Send a read-request into the fabric
   function Action fa_fabric_send_read_req (Fabric_Addr  addr);
      action
         AXI4_Size  size = axsize_8;
         let mem_req_rd_addr = AXI4_Rd_Addr {arid:     fabric_default_id,
                                             araddr:   addr,
                                             arlen:    7,           // burst len = arlen+1
                                             arsize:   size,
                                             arburst:  axburst_incr,
                                             arlock:   fabric_default_lock,
                                             arcache:  fabric_default_arcache,
                                             arprot:   fabric_default_prot,
                                             arqos:    fabric_default_qos,
                                             arregion: fabric_default_region,
                                             aruser:   fabric_default_user};

         master_xactor.i_rd_addr.enq (mem_req_rd_addr);

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
   Reg #(Bit #(3)) rg_rd_req_beat <- mkReg (0);
   Reg #(Bit #(3)) rg_rd_rsp_beat <- mkReg (0);

   FIFOF #(LdMemRq #(idT, childT)) f_pending_reads <- mkFIFOF;
   Reg #(Bit #(512)) rg_cline <- mkRegU;

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
      let  mem_rsp <- pop_o (master_xactor.o_rd_data);

      if (cfg_verbosity > 1) begin
         $display ("%0d: LLC_AXI4_Adapter.rl_handle_read_rsps: beat %0d ", cur_cycle, rg_rd_rsp_beat);
         $display ("    ", fshow (mem_rsp));
      end

      if (mem_rsp.rresp != axi4_resp_okay) begin
         // TODO: need to raise a non-maskable interrupt (NMI) here
         $display ("%0d: LLC_AXI4_Adapter.rl_handle_read_rsp: fabric response error; exit", cur_cycle);
         $display ("    ", fshow (mem_rsp));
         $finish (1);
      end

      // Shift next 64 bits from fabric into the cache line being assembled
      let new_cline = { mem_rsp.rdata, rg_cline [511:64] };

      if (mem_rsp.rlast) begin
         let ldreq <- pop (f_pending_reads);
         MemRsMsg #(idT, childT) resp = MemRsMsg {data:  unpack (new_cline),
                                                  child: ldreq.child,
                                                  id:    ldreq.id};

         llc.rsFromM.enq (resp);

         if (cfg_verbosity > 1)
            $display ("    Response to LLC: ", fshow (resp));

         rg_rd_rsp_beat <= 0;
         rg_cline <= unpack(0);
      end else begin
         rg_rd_rsp_beat <= rg_rd_rsp_beat + 1;
         rg_cline <= new_cline;
      end
   endrule

   // ================================================================
   // Handle write requests and responses

   // Each 512b cache line takes 8 beats, each handling 64 bits
   Reg #(Bit #(3)) rg_wr_req_beat <- mkReg (0);

   rule rl_handle_write_req (llc.toM.first matches tagged Wb .wb);
      if ((cfg_verbosity > 0) && (rg_wr_req_beat == 0)) begin
         $display ("%d: LLC_AXI4_Adapter.rl_handle_write_req: Wb request from LLC to memory:", cur_cycle);
         $display ("    ", fshow (wb));
      end

      // on first flit...
      // ================
      if (rg_wr_req_beat == 0) begin
         // send AXI4 AW flit
         master_xactor.i_wr_addr.enq (AXI4_Wr_Addr {
           awid:     fabric_default_id,
           awaddr:   { wb.addr [63:6], 6'h0 },
           awlen:    7, // burst len = awlen+1
           awsize:   axsize_8,
           awburst:  axburst_incr,
           awlock:   fabric_default_lock,
           awcache:  fabric_default_awcache,
           awprot:   fabric_default_prot,
           awqos:    fabric_default_qos,
           awregion: fabric_default_region,
           awuser:   0});
         // Expect a fabric response
         ctr_wr_rsps_pending.incr;
      end

      // on last flit...
      // ===============
      if (rg_wr_req_beat == 7) begin
         llc.toM.deq;
         rg_wr_req_beat <= 0;
      end else // increment flit counter
         rg_wr_req_beat <= rg_wr_req_beat + 1;

      // on each flit ...
      // ================
      Vector #(8, Bit #(8)) line_strb = unpack(pack(wb.byteEn));
      Vector #(8, Data) line_data = unpack(pack(wb.data));
      // send AXI4 W flit
      master_xactor.i_wr_data.enq(AXI4_Wr_Data {
        wdata:  line_data[rg_wr_req_beat],
        wstrb:  line_strb[rg_wr_req_beat],
        wlast:  rg_wr_req_beat == 7,
        wuser:  fabric_default_user});
   endrule

   // ----------------
   // Discard write-responses from the fabric

   rule rl_discard_write_rsp;
      let wr_resp <- pop_o (master_xactor.o_wr_resp);

      if (ctr_wr_rsps_pending.value == 0) begin
         $display ("%0d: ERROR: LLC_AXI4_Adapter.rl_discard_write_rsp: unexpected Wr response (ctr_wr_rsps_pending.value == 0)",
                   cur_cycle);
         $display ("    ", fshow (wr_resp));
         $finish (1);    // Assertion failure
      end

      ctr_wr_rsps_pending.decr;

      if (wr_resp.bresp != axi4_resp_okay) begin
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
   interface mem_master = master_xactor.axi_side;
endmodule

// ================================================================

endpackage
