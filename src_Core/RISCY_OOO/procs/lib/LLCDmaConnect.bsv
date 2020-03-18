// Copyright (c) 2017 Massachusetts Institute of Technology
// Portions Copyright (c) 2019-2020 Bluespec, Inc.

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
import MemLoader::*;
import CrossBar::*;
import MemLoader::*;

// ----------------
// From Toooba

import AXI4_Types   :: *;
import Fabric_Defs  :: *;
import Semi_FIFOF   :: *;

// ================================================================

typedef struct {
    CoreId core;
    TlbMemReqId id;
    LineDataOffset dataSel;
} TlbDmaReqId deriving(Bits, Eq, FShow);

typedef union tagged {
    MemLoaderMemReqId MemLoader;
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

function Bit #(64) fn_expand_strb_to_mask (Bit #(8) strb);
   function Bit #(8) fn_bit_to_byte (Integer j);
      return ((strb [j] == 1'b1) ? 8'hFF : 8'h00);
   endfunction

   Vector #(8, Bit #(8)) v = genWith (fn_bit_to_byte);

   return pack (v);
endfunction

// ================================================================

module mkLLCDmaConnect #(
    DmaServer#(LLCDmaReqId) llc,
    // MemLoaderMemClient memLoader,    // REPLACED BY AXI4_Slave_interface
    Vector#(CoreNum, TlbMemClient) tlb
)(AXI4_Slave_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User)) provisos (
    Alias#(dmaRqT, DmaRq#(LLCDmaReqId))
);
    Bool verbose = False;

   Integer verbosity = 0;

   // When debugger reads a word, request a line from LLC, and remember dword-in-line here
   FIFOF #(Bit #(3)) f_dword_in_line <- mkFIFOF;

   // Slave transactor for requests from Debug Module
   AXI4_Slave_Xactor_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) axi4_slave_xactor <- mkAXI4_Slave_Xactor;

   // ================================================================
   // These regs are a 1-location local cache for an LLC Cache Line,
   // to avoid doing a full read-modify-write to the LLC on each transaction.

   Reg #(Cacheline_Cache_State) rg_cacheline_cache_state <- mkReg (CACHELINE_CACHE_CLEAN);
   Reg #(Addr)                  rg_cacheline_cache_addr  <- mkReg (1);    // never matches an LLC line addr
   Reg #(Line)                  rg_cacheline_cache_data  <- mkRegU;

   // Writeback dirty cacheline_cache if no new store requests within n-cycles
   Reg #(Bit #(10)) rg_cacheline_cache_dirty_delay <- mkReg (0);

   // ================================================================
   // Write transactions from the external client (e.g., Debug Module)

   // Respond to store-requests from the external client on store-hit
   rule rl_handle_MemLoader_st_req (   (   (rg_cacheline_cache_state == CACHELINE_CACHE_CLEAN)
					|| (rg_cacheline_cache_state == CACHELINE_CACHE_DIRTY))
				    && (fn_addr_is_in_line (axi4_slave_xactor.o_wr_addr.first.awaddr,
							    rg_cacheline_cache_addr)));
      let wr_addr <- pop_o (axi4_slave_xactor.o_wr_addr);
      let wr_data <- pop_o (axi4_slave_xactor.o_wr_data);
      Addr      addr = wr_addr.awaddr;
      Bit #(64) data = wr_data.wdata;
      Bit #(64) mask = fn_expand_strb_to_mask (wr_data.wstrb);

      // Read rg_cacheline_cache_data as 64-bit words
      Vector #(8, Bit #(64)) line_dwords = unpack (pack (rg_cacheline_cache_data));
      // Modify relevant bytes of relevant dword
      Bit #(3)  dword_in_line     = addr [5:3];
      Bit #(64) old_dword         = line_dwords [dword_in_line];
      Bit #(64) new_dword         = ((old_dword & (~ mask)) | (data & mask));
      line_dwords [dword_in_line] = new_dword;
      // Save it
      rg_cacheline_cache_data        <= unpack (pack (line_dwords));
      rg_cacheline_cache_state       <= CACHELINE_CACHE_DIRTY;
      rg_cacheline_cache_dirty_delay <= '1;    // start write-back delay countdown

      // Send response to external client
      AXI4_Wr_Resp #(Wd_Id, Wd_User)
      wr_resp = AXI4_Wr_Resp {bid:   0,    // TODO: change uniformly to Fabric_id
			      bresp: axi4_resp_okay,
			      buser: ?};
      axi4_slave_xactor.i_wr_resp.enq (wr_resp);

      if (verbosity >= 2) begin
         $display ("%0d: %m.rl_handle_MemLoader_st_req: addr %0h data %0h strb %0h",
		   cur_cycle, wr_addr.awaddr, wr_data.wdata, wr_data.wstrb);
	 $display ("    old_dword: %0h", old_dword);
	 $display ("    new_dword: %0h", old_dword);
      end
   endrule

   // ================================================================
   // Read transactions from the external memory client (e.g., Debug Module)

   // Responds to load-requests from the external client on load-hit
   rule rl_handle_MemLoader_ld_req (   (   (rg_cacheline_cache_state == CACHELINE_CACHE_CLEAN)
					|| (rg_cacheline_cache_state == CACHELINE_CACHE_DIRTY))
				    && (fn_addr_is_in_line (axi4_slave_xactor.o_rd_addr.first.araddr,
							    rg_cacheline_cache_addr)));
      let rd_addr <- pop_o (axi4_slave_xactor.o_rd_addr);
      Addr addr = rd_addr.araddr;

      // Read rg_cacheline_cache as 64-bit words
      Vector #(8, Bit #(64)) line_dwords = unpack (pack (rg_cacheline_cache_data));
      Bit #(3)  dword_in_line = addr [5:3];
      Bit #(64) dword         = line_dwords [dword_in_line];

      // Send response to external client
      AXI4_Rd_Data #(Wd_Id, Wd_Data, Wd_User)
      rd_data = AXI4_Rd_Data {rid: 0,        // TODO: fixup
			      rdata: dword,
			      rresp: axi4_resp_okay,
			      rlast: True,
			      ruser: ?};
      axi4_slave_xactor.i_rd_data.enq (rd_data);

      if (verbosity >= 2) begin
         $display ("%0d: %m.rl_handle_MemLoader_ld_req: addr %0h", cur_cycle, rd_addr.araddr);
	 $display ("    dword: %0h", dword);
      end
   endrule

   // ----------------------------------------------------------------
   // Miss and writeback processing

   // Maintain dirty delay countdown
   rule rl_cacheline_cache_writeback_dirty_delay (   (rg_cacheline_cache_state == CACHELINE_CACHE_DIRTY)
					      && (rg_cacheline_cache_dirty_delay != 0));
      rg_cacheline_cache_dirty_delay <= rg_cacheline_cache_dirty_delay - 1;
   endrule

   function Action fa_writeback;
      action
	 dmaRqT req =  DmaRq {addr:   rg_cacheline_cache_addr,
			      byteEn: replicate (True),       // Write all bytes
			      data:   rg_cacheline_cache_data,
			      id:     tagged MemLoader (?)    // TODO: use  wr_addr.awid?
			      };
	 llc.memReq.enq (req);
	 // $display ("%0d: %m.fa_writeback line at %0h", cur_cycle, rg_cacheline_cache_addr);
	 // $display ("  data %0128h", rg_cacheline_cache_data);
      endaction
   endfunction

   // Initiate writeback if dirty for full delay
   rule rl_cacheline_cache_writeback_dirty_aged (   (rg_cacheline_cache_state == CACHELINE_CACHE_DIRTY)
					     && (rg_cacheline_cache_dirty_delay == 0));
      if (verbosity >= 2) begin
	 $display ("%0d: %m.rl_cacheline_cache_writeback_dirty_aged.", cur_cycle);
	 $display ("    Old line addr %0h", rg_cacheline_cache_addr);
      end

      fa_writeback;
      rg_cacheline_cache_state <= CACHELINE_CACHE_WRITING_BACK;
   endrule

   // Initiate writeback if dirty and next request is store-miss
   rule rl_cacheline_cache_writeback_st_miss (   (rg_cacheline_cache_state == CACHELINE_CACHE_DIRTY)
					      && (! fn_addr_is_in_line (axi4_slave_xactor.o_wr_addr.first.awaddr,
									rg_cacheline_cache_addr)));
      if (verbosity >= 2) begin
	 $display ("%0d: %m.rl_cacheline_cache_writeback_st_miss.", cur_cycle);
	 $display ("    Old line addr %0h", rg_cacheline_cache_addr);
	 $display ("    New addr      %0h", axi4_slave_xactor.o_wr_addr.first.awaddr);
      end

      fa_writeback;
      rg_cacheline_cache_state <= CACHELINE_CACHE_WRITING_BACK;
   endrule

   // Initiate writeback if dirty and next request is load-miss
   rule rl_cacheline_cache_writeback_ld_miss (   (rg_cacheline_cache_state == CACHELINE_CACHE_DIRTY)
					      && (! fn_addr_is_in_line (axi4_slave_xactor.o_rd_addr.first.araddr,
									rg_cacheline_cache_addr)));
      if (verbosity >= 2) begin
	 $display ("%0d: %m.rl_cacheline_cache_writeback_ld_miss.", cur_cycle);
	 $display ("    Old line addr %0h", rg_cacheline_cache_addr);
	 $display ("    New addr      %0h", axi4_slave_xactor.o_wr_addr.first.awaddr);
      end

      fa_writeback;
      rg_cacheline_cache_state <= CACHELINE_CACHE_WRITING_BACK;
   endrule

   // Finish writeback
   rule rl_cacheline_cache_writeback_finish (llc.respSt.first matches tagged MemLoader .id
					     &&& (rg_cacheline_cache_state == CACHELINE_CACHE_WRITING_BACK));
      let resp = llc.respSt.first;
      llc.respSt.deq;
      rg_cacheline_cache_state <= CACHELINE_CACHE_CLEAN;

      if (verbosity >= 2) begin
	 $display ("%0d: %m.rl_cacheline_cache_writeback_finish. Line addr %0h",
		   cur_cycle, rg_cacheline_cache_addr);
	 $display ("    Line data %0h", rg_cacheline_cache_data);
      end
   endrule

   function Action fa_initiate_reload (Addr addr);
      action
	 let line_addr = fn_align_addr_to_line (addr);
	 dmaRqT req =  DmaRq {addr:   line_addr,
			      byteEn: replicate (False),        // all False means 'read'
			      data:   ?,
			      id:     tagged MemLoader (?)};    // TODO: change uniformly to  wr_addr.awid
	 llc.memReq.enq (req);
	 rg_cacheline_cache_addr <= line_addr;

	 if (verbosity >= 2) begin
	    $display ("    fa_initiate_reload: line_addr %0h", line_addr);
	 end
      endaction
   endfunction

   // Initiate reload when cacheline_cache is clean on store-miss
   rule rl_cacheline_cache_reload_req_st (   (rg_cacheline_cache_state == CACHELINE_CACHE_CLEAN)
					  && (! fn_addr_is_in_line (axi4_slave_xactor.o_wr_addr.first.awaddr,
								    rg_cacheline_cache_addr)));
      let addr = axi4_slave_xactor.o_wr_addr.first.awaddr;

      if (verbosity >= 2) begin
	 $display ("%0d: %m.rl_cacheline_cache_reload_req_st for addr %0h", cur_cycle, addr);
      end

      fa_initiate_reload (addr);
      rg_cacheline_cache_state   <= CACHELINE_CACHE_RELOADING;
   endrule

   // Initiate reload when cacheline_cache is clean on load-miss
   rule rl_cacheline_cache_reload_req_ld (   (rg_cacheline_cache_state == CACHELINE_CACHE_CLEAN)
					  && (! fn_addr_is_in_line (axi4_slave_xactor.o_rd_addr.first.araddr,
								    rg_cacheline_cache_addr)));
      let addr = axi4_slave_xactor.o_rd_addr.first.araddr;

      if (verbosity >= 2) begin
	 $display ("%0d: %m.rl_cacheline_cache_reload_req_ld for addr %0h", cur_cycle, addr);
      end

      fa_initiate_reload (addr);
      rg_cacheline_cache_state   <= CACHELINE_CACHE_RELOADING;
   endrule

   // Finish reload
   rule rl_cacheline_cache_reload_finish (llc.respLd.first.id matches tagged MemLoader .id
					  &&& (rg_cacheline_cache_state == CACHELINE_CACHE_RELOADING));
      let resp = llc.respLd.first;
      llc.respLd.deq;
      rg_cacheline_cache_state <= CACHELINE_CACHE_CLEAN;
      rg_cacheline_cache_data  <= resp.data;

      if (verbosity >= 2) begin
	 $display ("%0d: %m.rl_cacheline_cache_reload_finish. Line addr %0h", cur_cycle, rg_cacheline_cache_addr);
	 $display ("    Line data %0h", resp.data);
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
            byteEn: replicate(False), // tlb req is always load
            data: ?,
            id: Tlb (id)
        };
    endfunction

   // Prioritize external mem client over Tlb
   (* descending_urgency = "rl_cacheline_cache_writeback_dirty_aged, sendTlbReqToLLC" *)
   (* descending_urgency = "rl_cacheline_cache_writeback_st_miss, sendTlbReqToLLC" *)
   (* descending_urgency = "rl_cacheline_cache_writeback_ld_miss, sendTlbReqToLLC" *)
   (* descending_urgency = "rl_cacheline_cache_reload_req_st, sendTlbReqToLLC" *)
   (* descending_urgency = "rl_cacheline_cache_reload_req_ld, sendTlbReqToLLC" *)

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
            data: resp.data[id.dataSel],
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

    return axi4_slave_xactor.axi_side;
endmodule
