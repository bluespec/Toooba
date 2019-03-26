// Copyright (c) 2014 Quanta Research Cambridge, Inc.

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

`include "ConnectalProjectConfig.bsv"

`ifdef PcieClockPeriod
Real pcieClockPeriod = `PcieClockPeriod;
`endif

Real mainClockPeriod = `MainClockPeriod;
Real derivedClockPeriod =`DerivedClockPeriod;

(* always_ready, always_enabled *)
interface B2C;
    interface Clock c;
    interface Reset r;
    method Action inputclock(Bit#(1) v);
    method Action inputreset(Bit#(1) v);
endinterface
import "BVI" CONNECTNET2 =
module mkB2C(B2C);
    default_clock no_clock;
    default_reset no_reset;
    output_clock c(OUT1);
    output_reset r(OUT2);
    method inputclock(IN1) enable((*inhigh*) en_inputclock) clocked_by(c);
    method inputreset(IN2) enable((*inhigh*) en_inputreset) clocked_by(c);
    schedule ( inputclock, inputreset) CF ( inputclock, inputreset);
endmodule

(* always_ready, always_enabled *)
interface B2C1;
    interface Clock c;
    method Action inputclock(Bit#(1) v);
endinterface
import "BVI" CONNECTNET =
module mkB2C1(B2C1);
    default_clock clk();
    default_reset rst();
    output_clock c(OUT);
    method inputclock(IN) enable((*inhigh*) en_inputclock);
    schedule ( inputclock) CF ( inputclock);
endmodule

(* always_ready, always_enabled *)
interface C2B;
    method Bit#(1) o();
endinterface
import "BVI" CONNECTNET =
module mkC2B#(Clock c)(C2B);
    default_clock clk();
    default_reset no_reset;
    //default_reset rst();
    input_clock ck(IN) = c;
    method OUT o();
    schedule ( o) CF ( o);
endmodule

(* always_ready, always_enabled *)
interface B2R;
    interface Reset r;
    method Action inputreset(Bit#(1) v);
endinterface
import "BVI" CONNECTNET =
module mkB2R(B2R);
    default_clock clk();
    default_reset rst();
    output_reset r(OUT);
    method inputreset(IN) enable((*inhigh*) en_inputclock);
    schedule ( inputreset) CF ( inputreset);
endmodule

(* always_ready, always_enabled *)
interface R2B;
    method Bit#(1) o();
endinterface
import "BVI" CONNECTNET =
module mkR2B#(Reset r)(C2B);
    default_clock no_clock;
    default_reset no_reset;
    //default_reset rst();
    input_reset rst(IN) = r;
    method OUT o();
    schedule ( o) CF ( o);
endmodule

interface PositiveReset;
   interface Reset positiveReset;
endinterface

import "BVI" PositiveReset =
module mkPositiveReset#(Integer resetDelay, Reset reset, Clock clock)(PositiveReset);
   parameter RSTDELAY = resetDelay;
   default_clock clock(CLK) = clock;
   default_reset reset(IN_RST) clocked_by (no_clock) = reset;
   output_reset positiveReset(OUT_RST);
endmodule

interface FpgaReset;
   interface Reset fpgaReset;
endinterface

import "BVI" FpgaReset =
module exposeFpgaReset#(Integer resetDelay, Clock clock)(FpgaReset);
   parameter RSTDELAY = resetDelay;
   default_clock clock(CLK) = clock;
   no_reset;
   output_reset fpgaReset(OUT_RST);
endmodule
