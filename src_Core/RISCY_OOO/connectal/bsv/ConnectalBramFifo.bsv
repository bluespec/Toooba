// Copyright (c) 2015 Quanta Research Cambridge, Inc.

// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

import Clocks::*;
import Vector::*;
import FIFO::*;
import FIFOF::*;
import BRAMFIFO::*;
import CBus::*; // extendNP and truncateNP

`include "ConnectalProjectConfig.bsv"
import Arith::*;
import ConnectalClocks::*;

`ifdef SIMULATION
`ifdef BOARD_xsim
`define USE_XILINX_MACRO
`endif // xsim
`else // not SIMULATION
`ifdef XILINX
`define USE_XILINX_MACRO
`endif // XILINX
`endif // not SIMULATION

`ifdef USE_XILINX_MACRO
(* always_ready, always_enabled *)
interface X7FifoSyncMacro#(numeric type data_width);
   method Bit#(1) empty();
   method Bit#(1) full();
   method Action din(Bit#(data_width) v);
   method Action wren(Bit#(1) wren);
   method Bit#(data_width) dout();
   method Action rden(Bit#(1) rden);
   method Bit#(9) wrcount();
   method Bit#(9) rdcount();
endinterface

import "BVI" FIFO_DUALCLOCK_MACRO =
module  vmkBramFifo#(String fifo_size, Clock wrClock, Reset wrReset, Clock rdClock, Reset rdReset)(X7FifoSyncMacro#(data_width));
`ifndef BSV_POSITIVE_RESET
   let rdReset1 <- mkSyncReset(10, rdReset, wrClock);
   let eitherReset <- mkResetEither(wrReset, rdReset1, clocked_by wrClock);
   let positiveReset <- mkPositiveReset(10, eitherReset, wrClock);
   // rst must be asserted for 5 read and write clock cycles
   let fifoReset = positiveReset.positiveReset;
`else
   let fifoReset = wrReset;
`endif
`ifdef XilinxUltrascale
   parameter DEVICE = "ULTRASCALE";
`else
   parameter DEVICE = "7SERIES";
`endif
   parameter DATA_WIDTH = valueOf(data_width);
   parameter FIFO_SIZE = fifo_size;
`ifndef SIMULATION
   parameter FIRST_WORD_FALL_THROUGH = "TRUE"; // not supported by xsim
`else
   parameter FIRST_WORD_FALL_THROUGH = True;
`endif
   default_clock wrClock(WRCLK) = wrClock;
   no_reset;
   // RST is asynchronous
   input_reset wrReset(RST) clocked_by(no_clock) = fifoReset;
   input_clock rdClock(RDCLK) = rdClock;
   method EMPTY empty() clocked_by (rdClock) reset_by (wrReset);
   method FULL full() clocked_by (wrClock) reset_by (wrReset);
   method din(DI) enable ((*inhigh*)EN_di) clocked_by (wrClock) reset_by (wrReset);
   method wren(WREN) enable ((*inhigh*)EN_wren) clocked_by (wrClock) reset_by (wrReset);
   method DO dout() clocked_by (rdClock) reset_by (wrReset);
   method rden(RDEN) enable ((*inhigh*)EN_rden) clocked_by (rdClock) reset_by (wrReset);
   // wrcount and rdcount ports are needed for xsim
   method WRCOUNT wrcount() clocked_by (wrClock) reset_by (wrReset);
   method RDCOUNT rdcount() clocked_by (rdClock) reset_by (wrReset);
   schedule (empty, full, dout, din, wren, rden, wrcount, rdcount) CF (empty, full, dout, din, wren, rden, wrcount, rdcount);
endmodule

module mkDualClockBramFIFOF#(Clock srcClock, Reset srcReset, Clock dstClock, Reset dstReset)(FIFOF#(t))
   provisos (Bits#(t,sizet),
	     Add#(1,a__,sizet));
   String fifo_size = "18Kb";
   Vector#(TDiv#(sizet,36),X7FifoSyncMacro#(36)) fifos <- replicateM(vmkBramFifo(fifo_size, srcClock, srcReset, dstClock, dstReset));
   Wire#(Bit#(1)) rdenWire <- mkDWire(0, clocked_by dstClock, reset_by dstReset);
   Wire#(Bit#(1)) wrenWire <- mkDWire(0, clocked_by srcClock, reset_by srcReset);
   Vector#(TDiv#(sizet,36),Wire#(Bit#(36))) dinWires <- replicateM(mkDWire(0, clocked_by srcClock, reset_by srcReset));

   for (Integer i = 0; i < valueOf(TDiv#(sizet,36)); i = i+1) begin
      Reg#(Bit#(9)) rdcount <- mkReg(0, clocked_by dstClock, reset_by dstReset);
      Reg#(Bit#(9)) wrcount <- mkReg(0, clocked_by srcClock, reset_by srcReset);
      rule rdenRule;
	 fifos[i].rden(rdenWire);
      endrule
      rule wrenRule;
	 fifos[i].wren(wrenWire);
      endrule
      rule inputs;
	 fifos[i].din(dinWires[i]);
      endrule
      rule countrds;
	 rdcount <= fifos[i].rdcount();
      endrule
      rule countwrs;
	 wrcount <= fifos[i].wrcount();
      endrule
   end

   function Bool fifoNotEmpty(X7FifoSyncMacro#(36) f); return f.empty == 0; endfunction
   function Bool fifoNotFull(X7FifoSyncMacro#(36) f); return f.full == 0; endfunction

   method t first() if (all(fifoNotEmpty, fifos));
      function Bit#(36) fifoFirst(Integer i); return fifos[i].dout(); endfunction
      Vector#(TDiv#(sizet,36), Bit#(36)) v = genWith(fifoFirst);
      return unpack(truncateNP(pack(v)));
   endmethod
   method Action deq() if (all(fifoNotEmpty, fifos));
      rdenWire <= 1;
   endmethod
   method notEmpty = all(fifoNotEmpty, fifos);
   method Action enq(t v) if (all(fifoNotFull, fifos));
      Vector#(TDiv#(sizet,36), Bit#(36)) vs = unpack(extendNP(pack(v)));
      Vector#(TDiv#(sizet,36), Integer) indices = genVector();
      function Action fifoEnq(Integer i); action dinWires[i] <= vs[i]; endaction endfunction
      mapM_(fifoEnq, indices);
      wrenWire <= 1;
   endmethod
   method notFull = all(fifoNotEmpty, fifos);
endmodule

module mkDualClockBramFIFO#(Clock srcClock, Reset srcReset, Clock dstClock, Reset dstReset)(FIFO#(t))
   provisos (Bits#(t,sizet),
	     Add#(1,a__,sizet));
   
   let syncFifo <- mkDualClockBramFIFOF(srcClock, srcReset, dstClock, dstReset);
   method enq = syncFifo.enq;
   method deq = syncFifo.deq;
   method first = syncFifo.first;
endmodule

`else // compatibility mode
module mkDualClockBramFIFOF#(Clock srcClock, Reset srcReset, Clock dstClock, Reset dstReset)(FIFOF#(t))
   provisos (Bits#(t,sizet),
	     Add#(1,a__,sizet));
   let syncFifo <- mkSyncFIFO(512, srcClock, srcReset, dstClock);
   method enq = syncFifo.enq;
   method deq = syncFifo.deq;
   method first = syncFifo.first;
   method notFull = syncFifo.notFull;
   method notEmpty = syncFifo.notEmpty;
endmodule
module mkDualClockBramFIFO#(Clock srcClock, Reset srcReset, Clock dstClock, Reset dstReset)(FIFO#(t))
   provisos (Bits#(t,sizet),
	     Add#(1,a__,sizet));
   
   let syncFifo <- mkSyncFIFO(512, srcClock, srcReset, dstClock);
   method enq = syncFifo.enq;
   method deq = syncFifo.deq;
   method first = syncFifo.first;
endmodule
`endif
