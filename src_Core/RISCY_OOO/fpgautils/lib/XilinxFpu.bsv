
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

import DefaultValue::*;
import GetPut::*;
import ClientServer::*;
import WaitAutoReset::*;
import FloatingPoint::*;
import FIFO::*;

export mkXilinxFpFmaIP;
export mkXilinxFpDivIP;
export mkXilinxFpSqrtIP;
export mkXilinxFpFmaSim;
export mkXilinxFpDivSim;
export mkXilinxFpSqrtSim;
export mkXilinxFpFma;
export mkXilinxFpDiv;
export mkXilinxFpSqrt;

typedef FloatingPoint::RoundMode FpuRoundMode;
typedef FloatingPoint::Exception FpuException;

export FpuRoundMode(..);
export FpuException(..);

// import Xilinx IP core

// xilinx raw FMA IP is a * b + c, later we need to adapt to a + b * c
interface FpFmaImport;
    method Action enqA(Bit#(64) a);
    method Action enqB(Bit#(64) b);
    method Action enqC(Bit#(64) c);

    method Action deq;
    method Bit#(64) resp;
    method Bit#(3) excep;
endinterface

import "BVI" fp_fma =
module mkFpFmaImport(FpFmaImport);
    default_clock clk(aclk, (*unused*) unused_gate);
    default_reset no_reset;

    method enqA(s_axis_a_tdata) enable(s_axis_a_tvalid) ready(s_axis_a_tready);
    method enqB(s_axis_b_tdata) enable(s_axis_b_tvalid) ready(s_axis_b_tready);
    method enqC(s_axis_c_tdata) enable(s_axis_c_tvalid) ready(s_axis_c_tready);

    method deq() enable(m_axis_result_tready) ready(m_axis_result_tvalid);
    method m_axis_result_tdata resp() ready(m_axis_result_tvalid);
    method m_axis_result_tuser excep() ready(m_axis_result_tvalid);

    schedule (enqA, enqB, enqC) CF (deq, resp, excep);

    schedule (enqA) CF (enqB, enqC);
    schedule (enqB) CF (enqC);
    schedule (enqA) C (enqA);
    schedule (enqB) C (enqB);
    schedule (enqC) C (enqC);

    schedule (deq) C (deq);
    schedule (resp, excep, deq) CF (resp, excep);
endmodule

interface FpDivImport;
    method Action enqA(Bit#(64) a);
    method Action enqB(Bit#(64) b);
    
    method Action deq;
    method Bit#(64) resp;
    method Bit#(4) excep;
endinterface

import "BVI" fp_div =
module mkFpDivImport(FpDivImport);
    default_clock clk(aclk, (*unused*) unused_gate);
    default_reset no_reset;

    method enqA(s_axis_a_tdata) enable(s_axis_a_tvalid) ready(s_axis_a_tready);
    method enqB(s_axis_b_tdata) enable(s_axis_b_tvalid) ready(s_axis_b_tready);

    method deq() enable(m_axis_result_tready) ready(m_axis_result_tvalid);
    method m_axis_result_tdata resp() ready(m_axis_result_tvalid);
    method m_axis_result_tuser excep() ready(m_axis_result_tvalid);

    schedule (enqA, enqB) CF (deq, resp, excep);

    schedule (enqA) CF (enqB);
    schedule (enqA) C (enqA);
    schedule (enqB) C (enqB);

    schedule (deq) C (deq);
    schedule (resp, excep, deq) CF (resp, excep);
endmodule

interface FpSqrtImport;
    method Action enqA(Bit#(64) a);

    method Action deq;
    method Bit#(64) resp;
    method Bit#(1) excep;
endinterface

import "BVI" fp_sqrt =
module mkFpSqrtImport(FpSqrtImport);
    default_clock clk(aclk, (*unused*) unused_gate);
    default_reset no_reset;

    method enqA(s_axis_a_tdata) enable(s_axis_a_tvalid) ready(s_axis_a_tready);

    method deq() enable(m_axis_result_tready) ready(m_axis_result_tvalid);
    method m_axis_result_tdata resp() ready(m_axis_result_tvalid);
    method m_axis_result_tuser excep() ready(m_axis_result_tvalid);

    schedule (enqA) CF (deq, resp, excep);

    schedule (enqA) C (enqA);

    schedule (deq) C (deq);
    schedule (resp, excep, deq) CF (resp, excep);
endmodule

// adapt the IP core to Bluespec FPU interface
(* synthesize *)
module mkXilinxFpFmaIP(Server#(
    Tuple4#(Maybe#(Double), Double, Double, FpuRoundMode),
    Tuple2#(Double, FpuException)
));
    WaitAutoReset#(8) init <- mkWaitAutoReset;
    FpFmaImport fpFma <- mkFpFmaImport;

    // xilinx raw ip is a * b + c
    // what we need to provide is in1 + in2 * in3
    // so in1 -> c, in2 -> a, in3 -> b

    interface Put request;
        method Action put(Tuple4#(Maybe#(Double), Double, Double, FpuRoundMode) req) if(init.isReady);
            let {maybe_in1, in2, in3, rm} = req;
            Double in1 = fromMaybe(zero(False), maybe_in1);
            fpFma.enqA(pack(in2));
            fpFma.enqB(pack(in3));
            fpFma.enqC(pack(in1));
            if(rm != Rnd_Nearest_Even) begin
                $fdisplay(stderr, "[Xlinx FMA] WARNING: unsupported rounding mode ", fshow(rm));
            end
        endmethod
    endinterface

    interface Get response;
        method ActionValue#(Tuple2#(Double, FpuException)) get if(init.isReady);
            fpFma.deq;
            Double val = unpack(fpFma.resp);
            Bit#(3) exBits = fpFma.excep;
            FpuException excep = defaultValue;
            excep.underflow = exBits[0] == 1;
            excep.overflow = exBits[1] == 1;
            excep.invalid_op = exBits[2] == 1;
            return tuple2(val, excep);
        endmethod
    endinterface
endmodule

(* synthesize *)
module mkXilinxFpDivIP(Server#(
    Tuple3#(Double, Double, FpuRoundMode),
    Tuple2#(Double, FpuException)
));
    WaitAutoReset#(8) init <- mkWaitAutoReset;
    FpDivImport fpDiv <- mkFpDivImport;

    interface Put request;
        method Action put(Tuple3#(Double, Double, FpuRoundMode) req) if(init.isReady);
            let {a, b, rm} = req;
            fpDiv.enqA(pack(a));
            fpDiv.enqB(pack(b));
            if(rm != Rnd_Nearest_Even) begin
                $fdisplay(stderr, "[Xlinx DIV] WARNING: unsupported rounding mode ", fshow(rm));
            end
        endmethod
    endinterface

    interface Get response;
        method ActionValue#(Tuple2#(Double, FpuException)) get if(init.isReady);
            fpDiv.deq;
            Double val = unpack(fpDiv.resp);
            Bit#(4) exBits = fpDiv.excep;
            FpuException excep = defaultValue;
            excep.underflow = exBits[0] == 1;
            excep.overflow = exBits[1] == 1;
            excep.invalid_op = exBits[2] == 1;
            excep.divide_0 = exBits[3] == 1;
            return tuple2(val, excep);
        endmethod
    endinterface
endmodule

(* synthesize *)
module mkXilinxFpSqrtIP(Server#(
    Tuple2#(Double, FpuRoundMode),
    Tuple2#(Double, FpuException)
));
    WaitAutoReset#(8) init <- mkWaitAutoReset;
    FpSqrtImport fpSqrt <- mkFpSqrtImport;

    interface Put request;
        method Action put(Tuple2#(Double, FpuRoundMode) req) if(init.isReady);
            let {a, rm} = req;
            fpSqrt.enqA(pack(a));
            if(rm != Rnd_Nearest_Even) begin
                $fdisplay(stderr, "[Xlinx SQRT] WARNING: unsupported rounding mode ", fshow(rm));
            end
        endmethod
    endinterface

    interface Get response;
        method ActionValue#(Tuple2#(Double, FpuException)) get if(init.isReady);
            fpSqrt.deq;
            Double val = unpack(fpSqrt.resp);
            Bit#(1) exBits = fpSqrt.excep;
            FpuException excep = defaultValue;
            excep.invalid_op = exBits[0] == 1;
            return tuple2(val, excep);
        endmethod
    endinterface
endmodule


// import simulation
interface FpFmaSim;
    method Bit#(64) fma(Bit#(64) a, Bit#(64) b, Bit#(64) c);
endinterface

import "BVI" fp_fma_sim =
module mkFpFmaSim(FpFmaSim);
    default_clock no_clock;
    default_reset no_reset;

    method RES fma(A, B, C);
endmodule

interface FpDivSim;
    method Bit#(64) div(Bit#(64) a, Bit#(64) b);
endinterface

import "BVI" fp_div_sim =
module mkFpDivSim(FpDivSim);
    default_clock no_clock;
    default_reset no_reset;

    method RES div(A, B);
endmodule

interface FpSqrtSim;
    method Bit#(64) sqrt(Bit#(64) a);
endinterface

import "BVI" fp_sqrt_sim =
module mkFpSqrtSim(FpSqrtSim);
    default_clock no_clock;
    default_reset no_reset;

    method RES sqrt(A);
endmodule

// adapt the simulation fp ops to Bluespec FPU interface
// Xilinx FPU has 2-element buffers inside
(* synthesize *)
module mkXilinxFpFmaSim(Server#(
    Tuple4#(Maybe#(Double), Double, Double, FpuRoundMode),
    Tuple2#(Double, FpuException)
));
    FpFmaSim fpFma <- mkFpFmaSim;
    FIFO#(Tuple2#(Double, FpuException)) respQ <- mkFIFO;

    // xilinx raw ip is a * b + c
    // what we need to provide is in1 + in2 * in3
    // so in1 -> c, in2 -> a, in3 -> b

    interface Put request;
        method Action put(Tuple4#(Maybe#(Double), Double, Double, FpuRoundMode) req);
            let {maybe_in1, in2, in3, rm} = req;
            Double in1 = fromMaybe(zero(False), maybe_in1);
            Double val = unpack(fpFma.fma(pack(in2), pack(in3), pack(in1)));
            respQ.enq(tuple2(val, defaultValue));
        endmethod
    endinterface
    
    interface response = toGet(respQ);
endmodule

(* synthesize *)
module mkXilinxFpDivSim(Server#(
    Tuple3#(Double, Double, FpuRoundMode),
    Tuple2#(Double, FpuException)
));
    FpDivSim fpDiv <- mkFpDivSim;
    FIFO#(Tuple2#(Double, FpuException)) respQ <- mkFIFO;

    interface Put request;
        method Action put(Tuple3#(Double, Double, FpuRoundMode) req);
            let {a, b, rm} = req;
            Double val = unpack(fpDiv.div(pack(a), pack(b)));
            respQ.enq(tuple2(val, defaultValue));
        endmethod
    endinterface
    
    interface response = toGet(respQ);
endmodule

(* synthesize *)
module mkXilinxFpSqrtSim(Server#(
    Tuple2#(Double, FpuRoundMode),
    Tuple2#(Double, FpuException)
));
    FpSqrtSim fpSqrt <- mkFpSqrtSim;
    FIFO#(Tuple2#(Double, FpuException)) respQ <- mkFIFO;

    interface Put request;
        method Action put(Tuple2#(Double, FpuRoundMode) req);
            let {a, rm} = req;
            Double val = unpack(fpSqrt.sqrt(pack(a)));
            respQ.enq(tuple2(val, defaultValue));
        endmethod
    endinterface
    
    interface response = toGet(respQ);
endmodule

// wrap simulation and xilinx ip together
(* synthesize *)
module mkXilinxFpFma(Server#(
    Tuple4#(Maybe#(Double), Double, Double, FpuRoundMode),
    Tuple2#(Double, FpuException)
));
`ifdef BSIM
    let m <- mkXilinxFpFmaSim;
`else
    let m <- mkXilinxFpFmaIP;
`endif
    return m;
endmodule

(* synthesize *)
module mkXilinxFpDiv(Server#(
    Tuple3#(Double, Double, FpuRoundMode),
    Tuple2#(Double, FpuException)
));
`ifdef BSIM
    let m <- mkXilinxFpDivSim;
`else
    let m <- mkXilinxFpDivIP;
`endif
    return m;
endmodule

(* synthesize *)
module mkXilinxFpSqrt(Server#(
    Tuple2#(Double, FpuRoundMode),
    Tuple2#(Double, FpuException)
));
`ifdef BSIM
    let m <- mkXilinxFpSqrtSim;
`else
    let m <- mkXilinxFpSqrtIP;
`endif
    return m;
endmodule
