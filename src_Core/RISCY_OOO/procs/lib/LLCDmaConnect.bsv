// Copyright (c) 2017 Massachusetts Institute of Technology
// Portions Copyright (c) 2019-2020 Bluespec, Inc.
//
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

// This file is a modified version of:    RISCY_OOO/procs/lib/LLCDmaConnect.bsv
// Bluespec: this file is has many modifications.

// The original module had 3 params and had an Empty interface.
// The 2nd param was:    MemLoaderMemClient memLoader
// which issued only write-transactions (to load memory).
// The module discarded write responses, and ignored read-requests.

// Here, that module parameter is removed and, instead, the module has an
// AXI4_Slave interface, to be connected to the AXI4_Master of the
// Debug Module.  This axi4_slave accepts, processes and responds
// to both read and write transactions.

// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// ================================================================
// BSV library imports

import FIFOF       :: *;
import Connectable :: *;

import FShow       :: *;
import GetPut      :: *;
import Vector      :: *;
import BuildVector :: *;
import FIFO        :: *;
import Assert      :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import Semi_FIFOF :: *;
import EdgeFIFOFs :: *;

// ================================================================
// Project imports

// ----------------
// From RISCY-OOO

import Types::*;
import ProcTypes::*;
import CacheUtils::*;
import CCTypes::*;
import L2Tlb::*;
import CrossBar::*;

// ----------------
// From Toooba

import AXI4 :: *;
import SourceSink :: *;
import Fabric_Defs  :: *;
import SoC_Map  :: *;
import Semi_FIFOF   :: *;

// ================================================================

typedef struct {
    CoreId core;
    TlbMemReqId id;
    LineDataOffset dataSel;
} TlbDmaReqId deriving(Bits, Eq, FShow);

typedef union tagged {
    void Client;
    TlbDmaReqId Tlb;
} LLCDmaReqId deriving(Bits, Eq, FShow);

// ================================================================
// Help functions for read-modify-writes of 4-Byte values on a 64-Byte Cache Line

typedef enum {CACHELINE_CACHE_INVALID,
              CACHELINE_CACHE_WRITING_BACK,
              CACHELINE_CACHE_RELOADING,
              CACHELINE_CACHE_CLEAN,
              CACHELINE_CACHE_DIRTY
   } Cacheline_Cache_State
deriving (Bits, Eq, FShow);

function Addr fn_align_addr_to_line (Addr addr);
   Addr line_addr = { addr [63:6], 6'b0 };
   return line_addr;
endfunction

function Bool fn_addr_is_in_line (Addr addr, Addr line_addr);
   return (fn_align_addr_to_line (addr) == line_addr);
endfunction

// ================================================================

module mkLLCDmaConnect #( DmaServer#(LLCDmaReqId) llc
                        , Vector#(CoreNum, TlbMemClient) tlb )
  (AXI4_Slave #( Wd_CoreW_Bus_SId, Wd_Addr, Wd_Data_Periph
               , Wd_AW_User_Periph, Wd_W_User_Periph, Wd_B_User_Periph
               , Wd_AR_User_Periph, Wd_R_User_Periph ))
  provisos (Alias #(dmaRqT, DmaRq #(LLCDmaReqId)));
    Bool verbose = False;

   Integer verbosity = 0;

   // Connector to AXI4 fabric
   let slavePortShim <- mkAXI4ShimFF;

   // ================================================================
   // Write transactions from the external client (e.g., Debug Module)

   let internal_aw_ff <- mkFIFO;

   rule rl_client_st;
      let wr_addr <- get (slavePortShim.master.aw);
      let wr_data <- get (slavePortShim.master.w);

      internal_aw_ff.enq(wr_addr);

      if (verbosity >= 2) begin
         $display ("%0d: %m.rl_client_st for addr %0h", cur_cycle, wr_addr.awaddr);
      end

      let line_addr = fn_align_addr_to_line (wr_addr.awaddr);
      Vector#(TDiv#(CLineDataNumBytes,8), Bit#(TDiv#(Wd_Data_Periph,8))) line_be = replicate(0);
      line_be[getCLineDataSel(wr_addr.awaddr)] = wr_data.wstrb;
      dmaRqT req =  DmaRq {addr:   line_addr,
                           byteEn: unpack(pack(line_be)),
                           data:   setDataAt(unpack(0), getCLineDataSel(wr_addr.awaddr), wr_data.wdata),
                           id:     tagged Client};    // TODO: change uniformly to  wr_addr.awid
      llc.memReq.enq (req);
   endrule

   rule rl_client_st_rsp(llc.respSt.first matches tagged Client);
      llc.respSt.deq;
      let wr_addr <- get (internal_aw_ff);

      // Send response to external client
      slavePortShim.master.b.put(AXI4_BFlit{
        bid:   wr_addr.awid,
        bresp: OKAY,
        buser: ?
      });
   endrule

   // ================================================================
   // Read transactions from the external memory client (e.g., Debug Module)

   let internal_ar_ff <- mkFIFO;

   rule rl_client_ld_req;
      let rd_addr <- get (slavePortShim.master.ar);
      internal_ar_ff.enq(rd_addr);
      let line_addr = fn_align_addr_to_line (rd_addr.araddr);
      dmaRqT req =  DmaRq {addr:   line_addr,
                           byteEn: replicate(replicate(False)), // all False means 'read'
                           data:   ?,
                           id:     tagged Client};    // TODO: change uniformly to  wr_addr.awid
      llc.memReq.enq (req);

      if (verbosity >= 2) begin
         $display ("%0d: %m.rl_client_ld_req: line_addr %0h", cur_cycle, line_addr);
      end
   endrule

   // Finish reload
   rule rl_client_ld_rsp (llc.respLd.first.id matches tagged Client);
      let resp = llc.respLd.first;
      llc.respLd.deq;

      let rd_addr <- get (internal_ar_ff);
      let dword = getDataAt( resp.data
                           , getCLineDataSel(rd_addr.araddr));

      // Send response to external client
      slavePortShim.master.r.put(AXI4_RFlit{
        rid: rd_addr.arid,
        rdata: dword,
        rresp: OKAY,
        rlast: True,
        ruser: ?
      });

      if (verbosity >= 2) begin
         $display ("%0d: %m.rl_client_ld_rsp: addr %0h", cur_cycle, rd_addr.araddr);
         $display ("    dword: %0h", dword);
      end
   endrule

    // ================================================================
    // Transactions from the TLB
    // Expecting only LOAD requests from TLB
    // This section is unchanged from the original riscy-ooo module.

    // helper functions for cross bar
    function XBarDstInfo#(Bit#(0), Tuple2#(CoreId, TlbMemReq)) getTlbDst(CoreId core, TlbMemReq r);
        return XBarDstInfo {idx: 0, data: tuple2(core, r)};
    endfunction
    function Get#(TlbMemReq) tlbReqGet(TlbMemClient cli) = toGet(cli.memReq);

    // cross bar for Tlb
    FIFO#(Tuple2#(CoreId, TlbMemReq)) tlbQ <- mkFIFO;
    mkXBar(getTlbDst, map(tlbReqGet, tlb), vec(toPut(tlbQ)));

    // TLB req is for a whole data
    function dmaRqT getTlbDmaReq(CoreId c, TlbMemReq r);
        LineDataOffset dataSel = getLineDataOffset(r.addr);
        let id = TlbDmaReqId {
            core: c,
            id: r.id,
            dataSel: dataSel
        };
        return DmaRq {
            addr: r.addr,
            byteEn: replicate(replicate(False)), // tlb req is always load
            data: ?,
            id: Tlb (id)
        };
    endfunction

   // Prioritize external mem client over Tlb
   (* descending_urgency = "rl_client_st, sendTlbReqToLLC" *)
   (* descending_urgency = "rl_client_ld_req, sendTlbReqToLLC" *)

    rule sendTlbReqToLLC;
        let {c, r} <- toGet(tlbQ).get;
        let req = getTlbDmaReq(c, r);
        llc.memReq.enq(req);
        if(verbose) begin
            $display("  [LLCDmaConnnect sendTlbReqToLLC] ", fshow(r), " ; ", fshow(req));
        end
    endrule

    rule sendLdRespToTlb(llc.respLd.first.id matches tagged Tlb .id);
        llc.respLd.deq;
        let resp = llc.respLd.first;
        let ld = TlbLdResp {
            data: getDataAt(resp.data, id.dataSel),
            id: id.id
        };
        tlb[id.core].respLd.enq(ld);
        if(verbose) begin
            $display("  [LLCDmaConnect sendLdRespToTlb] ", fshow(resp), " ; ", fshow(ld));
        end
    endrule

    rule sendStRespToTlb(llc.respSt.first matches tagged Tlb .id);
        llc.respSt.deq;
        if(verbose) begin
            $display("  [LLCDmaConnect sendStRespToTlb] ", fshow(llc.respSt.first));
        end
        doAssert(False, "No TLB st");
    endrule

    // ================================================================
    // INTERFACE

    return slavePortShim.slave;
endmodule
