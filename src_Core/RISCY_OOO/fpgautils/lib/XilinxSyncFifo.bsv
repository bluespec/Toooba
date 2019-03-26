
// Copyright (c) 2017 Massachusetts Institute of Technology
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

import Vector::*;
import Clocks::*;
import CBus::*;

export mkXilinxSyncFifo;
export mkXilinxSyncBramFifo;

// import sync fifos from Xilinx FIFO generator
interface SyncFifoImport#(numeric type w);
    method Bool full;
    method Action enq(Bit#(w) x);
    method Bool empty;
    method Bit#(w) first;
    method Action deq;
endinterface

import "BVI" sync_fifo_w32_d16 =
module mkSyncFifoImport_w32_d16#(Clock srcClk, Clock dstClk)(SyncFifoImport#(32));
    default_clock no_clock;
    default_reset no_reset;
    input_clock (wr_clk) = srcClk;
    input_clock (rd_clk) = dstClk;

    method full full() clocked_by(srcClk) reset_by(no_reset);
    method enq(din) enable(wr_en) clocked_by(srcClk) reset_by(no_reset);
    method empty empty() clocked_by(dstClk) reset_by(no_reset);
    method dout first() clocked_by(dstClk) reset_by(no_reset);
    method deq() enable(rd_en) clocked_by(dstClk) reset_by(no_reset);

    schedule (full, enq) CF (empty, first, deq);
    schedule (full) CF (full, enq);
    schedule (enq) C (enq);
    schedule (empty, first) CF (empty, first, deq);
    schedule (deq) C (deq);
endmodule

import "BVI" sync_bram_fifo_w36_d512 =
module mkSyncBramFifoImport_w36_d512#(Clock srcClk, Clock dstClk)(SyncFifoImport#(36));
    default_clock no_clock;
    default_reset no_reset;
    input_clock (wr_clk) = srcClk;
    input_clock (rd_clk) = dstClk;

    method full full() clocked_by(srcClk) reset_by(no_reset);
    method enq(din) enable(wr_en) clocked_by(srcClk) reset_by(no_reset);
    method empty empty() clocked_by(dstClk) reset_by(no_reset);
    method dout first() clocked_by(dstClk) reset_by(no_reset);
    method deq() enable(rd_en) clocked_by(dstClk) reset_by(no_reset);

    schedule (full, enq) CF (empty, first, deq);
    schedule (full) CF (full, enq);
    schedule (enq) C (enq);
    schedule (empty, first) CF (empty, first, deq);
    schedule (deq) C (deq);
endmodule

// wrap up imported BSV or simulation module
(* synthesize, no_default_clock, no_default_reset *)
module mkSyncFifo_w32_d16#(Clock srcClk, Reset srcRst, Clock dstClk)(SyncFIFOIfc#(Bit#(w))) provisos (NumAlias#(w, 32));
`ifdef BSIM
    SyncFIFOIfc#(Bit#(w)) q <- mkSyncFIFO(16, srcClk, srcRst, dstClk);
    return q;
`else
    SyncFifoImport#(w) q <- mkSyncFifoImport_w32_d16(srcClk, dstClk);

    method notFull = !q.full;
    method Action enq(Bit#(w) x) if(!q.full);
        q.enq(x);
    endmethod
    method notEmpty = !q.empty;
    method Bit#(w) first if(!q.empty);
        return q.first;
    endmethod
    method Action deq if(!q.empty);
        q.deq;
    endmethod
`endif
endmodule

(* synthesize, no_default_clock, no_default_reset *)
module mkSyncBramFifo_w36_d512#(Clock srcClk, Reset srcRst, Clock dstClk)(SyncFIFOIfc#(Bit#(w))) provisos (NumAlias#(w, 36));
`ifdef BSIM
    SyncFIFOIfc#(Bit#(w)) q <- mkSyncFIFO(512, srcClk, srcRst, dstClk);
    return q;
`else
    SyncFifoImport#(w) q <- mkSyncBramFifoImport_w36_d512(srcClk, dstClk);

    method notFull = !q.full;
    method Action enq(Bit#(w) x) if(!q.full);
        q.enq(x);
    endmethod
    method notEmpty = !q.empty;
    method Bit#(w) first if(!q.empty);
        return q.first;
    endmethod
    method Action deq if(!q.empty);
        q.deq;
    endmethod
`endif
endmodule

// generate parameterized-width fifo from fixed-with fifo
module mkGenXilinxSyncFifo#(
    function module#(SyncFIFOIfc#(Bit#(fifoW))) mkfifo(Clock srcClk, Reset srcRst, Clock dstClk),
    Clock srcClk, Reset srcRst, Clock dstClk
)(SyncFIFOIfc#(t)) provisos(
    Bits#(t, dataW),
    Add#(1, a__, dataW),
    NumAlias#(fifoNum, TDiv#(dataW, fifoW))
);
    Vector#(fifoNum, SyncFIFOIfc#(Bit#(fifoW))) fifos <- replicateM(mkfifo(srcClk, srcRst, dstClk));

    function Bool getNotFull(SyncFIFOIfc#(Bit#(fifoW)) ifc) = ifc.notFull;
    Bool isNotFull = all(getNotFull, fifos);

    function Bool getNotEmpty(SyncFIFOIfc#(Bit#(fifoW)) ifc) = ifc.notEmpty;
    Bool isNotEmpty = all(getNotEmpty, fifos);

    method notFull = isNotFull;

    method Action enq(t x);
        Vector#(fifoNum, Bit#(fifoW)) data = unpack(zeroExtendNP(pack(x)));
        for(Integer i = 0; i < valueof(fifoNum); i = i+1) begin
            fifos[i].enq(data[i]);
        end
    endmethod

    method notEmpty = isNotEmpty;

    method t first;
        Vector#(fifoNum, Bit#(fifoW)) data = ?;
        for(Integer i = 0; i < valueof(fifoNum); i = i+1) begin
            data[i] = fifos[i].first;
        end
        return unpack(truncateNP(pack(data)));
    endmethod

    method Action deq;
        for(Integer i = 0; i < valueof(fifoNum); i = i+1) begin
            fifos[i].deq;
        end
    endmethod
endmodule

module mkXilinxSyncFifo#(Clock srcClk, Reset srcRst, Clock dstClk)(SyncFIFOIfc#(t)) provisos(
    Bits#(t, dataW), Add#(1, a__, dataW)
);
    SyncFIFOIfc#(t) m <- mkGenXilinxSyncFifo(mkSyncFifo_w32_d16, srcClk, srcRst, dstClk);
    return m;
endmodule

module mkXilinxSyncBramFifo#(Clock srcClk, Reset srcRst, Clock dstClk)(SyncFIFOIfc#(t)) provisos(
    Bits#(t, dataW), Add#(1, a__, dataW)
);
    SyncFIFOIfc#(t) m <- mkGenXilinxSyncFifo(mkSyncBramFifo_w36_d512, srcClk, srcRst, dstClk);
    return m;
endmodule

