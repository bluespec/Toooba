// This file is a modified version of:    RISCY_OOO/procs/lib/LLCDmaConnect.bsv

// The original module had, as 2nd parameter,    MemLoaderMemClient memLoader
// The memLoader assumed only write-transactions (to load memory), and
// discarded load responses.

// Here, the module instead offers an AXI4_Slave interface to be
// connected to the AXI4_Master of the Debug Module.
// This axi4_slave accepts and responds to both read and write transactions.

// Copyright (c) 2017 Massachusetts Institute of Technology
// Portions Copyright (c) 2019-2020 Bluespec, Inc.
// 
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

import FShow::*;
import GetPut::*;
import Vector::*;
import BuildVector::*;
import FIFO::*;
import FIFOF::*;
import Assert::*;

import Types::*;
import ProcTypes::*;
import CacheUtils::*;
import CCTypes::*;
import L2Tlb::*;
import MemLoader::*;
import CrossBar::*;
import MemLoader::*;

import AXI4_Types   :: *;
import Fabric_Defs  :: *;
import Semi_FIFOF   :: *;

typedef struct {
    CoreId core;
    TlbMemReqId id;
    LineDataOffset dataSel;
} TlbDmaReqId deriving(Bits, Eq, FShow);

typedef union tagged {
    MemLoaderMemReqId MemLoader;
    TlbDmaReqId Tlb;
} LLCDmaReqId deriving(Bits, Eq, FShow);

// For writing, position a 4-byte value and 4-bit byte-enable into a 64-byte line and 64-bit line-byte-enable
function Tuple3 #(Addr, Line, LineByteEn) fn_line_and_byteen_from_word (Addr addr, Bit #(64) data);
   Vector #(16, Bit #(32)) line_words        = replicate (0);
   Vector #(16, Bit #(4))  line_word_byteens = replicate (0);
   Bit #(4) word_index = addr [5:2];
   line_words [word_index] = ((addr [2] == 0) ? data [31:0] : data [63:32]);
   line_word_byteens [word_index] = 4'b1111;
   Addr line_addr = { addr [63:6], 6'b0 };
   return tuple3 (line_addr,
		  unpack (pack (line_words)),
		  unpack (pack (line_word_byteens)));
endfunction

// For reading, extract a 4-byte value from a 64-byte line
function Bit #(64) fn_word_from_line (Line line, Bit #(4) word_in_line);
   Vector #(16, Bit #(32)) line_words = unpack (pack (line));
   Bit #(32) w  = line_words [word_in_line];
   Bit #(64) dw = ((word_in_line [0] == 0) ? { 32'b0, w } : { w, 32'b0 });
   return dw;
endfunction

module mkLLCDmaConnect #(
    DmaServer#(LLCDmaReqId) llc,
    // MemLoaderMemClient memLoader,    // REPLACED BY AXI4_Slave_interface
    Vector#(CoreNum, TlbMemClient) tlb
)(AXI4_Slave_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User)) provisos (
    Alias#(dmaRqT, DmaRq#(LLCDmaReqId))
);
    Bool verbose = False;

   Integer verbosity = 0;

   // When debugger reads a word, request a line from LLC, and remember word-in-line here
   FIFOF #(Bit #(4)) f_word_in_line <- mkFIFOF;

   // Slave transactor for requests from Debug Module
   AXI4_Slave_Xactor_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) axi4_slave_xactor <- mkAXI4_Slave_Xactor;

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

    rule sendMemLoaderReqToLLC_wr;    // write requests
       let wr_addr <- pop_o (axi4_slave_xactor.o_wr_addr);
       let wr_data <- pop_o (axi4_slave_xactor.o_wr_data);
       match { .line_addr, .line_data, .line_byteen } = fn_line_and_byteen_from_word (wr_addr.awaddr, wr_data.wdata);
       dmaRqT req =  DmaRq {addr:   line_addr,
			    byteEn: line_byteen,
			    data:   line_data,
			    id:     tagged MemLoader (?)    // TODO: change uniformly to  wr_addr.awid
			    };
       llc.memReq.enq(req);

       if (verbosity != 0) begin
          $display("[LLCDmaConnect sendMemLoaderReqToLLC_wr]");
	  $display ("    ", fshow (wr_addr));
	  $display ("    ", fshow (wr_data));
	  $display ("    ", fshow (req));
       end
    endrule

    rule sendMemLoaderReqToLLC_rd;    // read requests
       let rd_addr <- pop_o (axi4_slave_xactor.o_rd_addr);
       Addr line_addr = { rd_addr.araddr [63:6], 6'b0 };
       dmaRqT req =  DmaRq {addr:   line_addr,
			    byteEn: replicate (False),
			    data:   ?,
			    id:     MemLoader (?)    // TODO: change uniformly to  rd_addr.awid
			    };
       llc.memReq.enq(req);
       Bit #(4) word_in_line = rd_addr.araddr [5:2];
       f_word_in_line.enq (word_in_line);

       if (verbosity != 0) begin
          $display("[LLCDmaConnect sendMemLoaderReqToLLC_rd]");
	  $display ("    ", fshow (rd_addr));
	  $display ("    ", fshow (req));
       end
    endrule

    (* descending_urgency = "sendMemLoaderReqToLLC_wr, sendTlbReqToLLC" *)
    (* descending_urgency = "sendMemLoaderReqToLLC_rd, sendTlbReqToLLC" *)
    rule sendTlbReqToLLC;
        let {c, r} <- toGet(tlbQ).get;
        let req = getTlbDmaReq(c, r);
        llc.memReq.enq(req);
        if(verbose) begin
            $display("  [LLCDmaConnnect sendTlbReqToLLC] ", fshow(r), " ; ", fshow(req));
        end
    endrule

    // send Ld resp from LLC
    rule sendLdRespToMemLoader(llc.respLd.first.id matches tagged MemLoader .id);
       let resp = llc.respLd.first;
       llc.respLd.deq;
       let word_in_line = f_word_in_line.first;
       f_word_in_line.deq;
       AXI4_Rd_Data #(Wd_Id, Wd_Data, Wd_User)
       rd_data = AXI4_Rd_Data {rid: 0,    // TODO: change uniformly to Fabric_Id
			       rdata: fn_word_from_line (resp.data, word_in_line),
			       rresp: axi4_resp_okay,
			       rlast: True,
			       ruser: ?};
       axi4_slave_xactor.i_rd_data.enq (rd_data);
       if (verbosity != 0) begin
          $display ("[LLCDmaConnect sendLdRespToMemLoader]");
	  $display ("    ",  fshow (resp));
	  $display ("    ",  fshow (rd_data));
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

    // send St resp from LLC
    rule sendStRespToMemLoader(llc.respSt.first matches tagged MemLoader .id);
       let resp = llc.respSt.first;
       llc.respSt.deq;
       AXI4_Wr_Resp #(Wd_Id, Wd_User)
       wr_resp = AXI4_Wr_Resp {bid:   0,    // TODO: change uniformly to Fabric_id
			       bresp: axi4_resp_okay,
			       buser: ?};
       axi4_slave_xactor.i_wr_resp.enq (wr_resp);

       if (verbosity != 0) begin
          $display ("[LLCDmaConnect sendStRespToMemLoader]");
	  $display ("    ", fshow (resp));
	  $display ("    ", fshow (wr_resp));
       end
    endrule

    rule sendStRespToTlb(llc.respSt.first matches tagged Tlb .id);
        llc.respSt.deq;
        if(verbose) begin
            $display("  [LLCDmaConnect sendStRespToTlb] ", fshow(llc.respSt.first));
        end
        doAssert(False, "No TLB st");
    endrule

    return axi4_slave_xactor.axi_side;
endmodule
