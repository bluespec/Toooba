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

import VnD :: *;
import Bag :: *;
import ProcTypes :: *;

// ================================================================

interface LLC_AXI4_Adapter_IFC;
   method Action reset;

   // Fabric master interface for memory
   interface AXI4_Master #(Wd_MId, Wd_Addr, Wd_Data,
                           Wd_AW_User, Wd_W_User, Wd_B_User,
                           Wd_AR_User, Wd_R_User) mem_master;
endinterface

// ================================================================

typedef struct {
    Bool tag_req; // meaningful to upgrade to E if toState is S
    idT id; // slot id in child cache
    childT child; // from which child
} LLC_AXI_ID#(type idT, type childT) deriving(Bits, Eq, FShow);

typedef 16 OutstandingWrites;
typedef 16 WriteAddressHashW;

module mkLLC_AXi4_Adapter #(MemFifoClient #(idT, childT) llc)
                          (LLC_AXI4_Adapter_IFC)
   provisos(Bits#(idT, idSz),
            Bits#(childT, childSz),
            FShow#(ToMemMsg#(idT, childT)),
            FShow#(MemRsMsg#(idT, childT)),
            Add#(SizeOf#(Line), 0, TAdd#(512, 4)), // assert Line sz = 512 + 4 tags
            Add#(a__, SizeOf#(LLC_AXI_ID#(idT, childT)), Wd_MId) // LLC_AXI_ID must fit into the external ID.
           );

   // Verbosity: 0: quiet; 1: LLC transactions; 2: loop detail
   Integer verbosity = 0;
   Reg #(Bit #(4)) cfg_verbosity <- mkConfigReg (fromInteger (verbosity));

   // ================================================================
   // Fabric request/response

   let masterPortShim <- mkAXI4ShimFF;

   // For discarding write-responses
   CreditCounter_IFC #(TLog#(OutstandingWrites)) ctr_wr_rsps_pending <- mkCreditCounter; // 16 outstanding writes.

   Bag#(OutstandingWrites, Bit#(Wd_MId), Bit#(WriteAddressHashW)) outstandingWrites <- mkSmallBag;

   // ================================================================
   // Functions to interact with the fabric

   // Send a read-request into the fabric
   function Action fa_fabric_send_read_req (Fabric_Addr  addr, LLC_AXI_ID#(idT, childT) id);
      action
         Bit#(Wd_MId) arid = zeroExtend(pack(id));
         let mem_req_rd_addr = AXI4_ARFlit {arid:     arid,
                                            araddr:   addr,
                                            arlen:    0,           // burst len = arlen+1
                                            arsize:   id.tag_req ? 1 : 64,
                                            arburst:  INCR,
                                            arlock:   fabric_default_lock,
                                            arcache:  fabric_default_arcache,
                                            arprot:   fabric_default_prot,
                                            arqos:    fabric_default_qos,
                                            arregion: fabric_default_region,
                                            aruser:   pack(id.tag_req)};

         masterPortShim.slave.ar.put(mem_req_rd_addr);

         // Debugging
         if (cfg_verbosity > 1) begin
            $display ("    ", fshow (mem_req_rd_addr));
         end
      endaction
   endfunction

   // ================================================================
   // Handle read requests and responses

   rule rl_handle_read_req (llc.toM.first matches tagged Ld .ld &&& !outstandingWrites.dataMatch(hash(ld.addr[63:6])));
      if ((cfg_verbosity > 0)) begin
         $display ("%0d: LLC_AXI4_Adapter.rl_handle_read_req: Ld request from LLC to memory",
                   cur_cycle);
         $display ("    ", fshow (ld));
      end

      Addr  line_addr = {ld.addr [63:6], 6'h0 };                      // Addr of containing cache line
      fa_fabric_send_read_req (line_addr, LLC_AXI_ID{tag_req: ld.tag_req, id: ld.id, child: ld.child});
      llc.toM.deq;
   endrule

   rule rl_handle_read_rsps;
      let mem_rsp <- get(masterPortShim.slave.r);
      if (cfg_verbosity > 1) begin
         $display ("%0d: LLC_AXI4_Adapter.rl_handle_read_rsps: ", cur_cycle);
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
      LLC_AXI_ID#(idT, childT) id = unpack(truncate(mem_rsp.rid));
      MemRsMsg #(idT, childT) resp = MemRsMsg {data:  new_cline,
                                              child: id.child,
                                              id:    id.id};
      if (id.tag_req) begin
        resp.data = CLine { tag: unpack(truncate(mem_rsp.rdata)), data: ?};
      end
      llc.rsFromM.enq (resp);
      if (cfg_verbosity > 1)
        $display ("    Response to LLC: ", fshow (resp));
   endrule

   // ================================================================
   // Handle write requests and responses
   Reg#(Bit#(Wd_MId)) wid_reg <- mkRegU;
   rule rl_handle_write_req (llc.toM.first matches tagged Wb .wb &&&
                             !outstandingWrites.isMember(wid_reg).v && !outstandingWrites.dataMatch(hash(wb.addr[63:6])));
      if (cfg_verbosity > 0) begin
         $display ("%d: LLC_AXI4_Adapter.rl_handle_write_req: Wb request from LLC to memory:", cur_cycle);
         $display ("    ", fshow (wb));
      end

      // send AXI4 AW flit
      masterPortShim.slave.aw.put (AXI4_AWFlit {
        awid:     wid_reg,
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
      outstandingWrites.insert(wid_reg, hash(wb.addr[63:6]));
      wid_reg <= wid_reg + 1; // Best effort to use unique IDs to allow reordering in the fabric.
      llc.toM.deq;

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
      outstandingWrites.remove(wr_resp.bid);

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
      error("Reset called for LLC AXI4 adapter");
      // XXX resetting this module would cause wedges unless the surrounding
      // fabric was also fully reset
   endmethod

   // Fabric interface for memory
   interface mem_master = masterPortShim.master;
endmodule

// ================================================================

endpackage
