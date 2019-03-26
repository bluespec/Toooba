
// Copyright (c) 2016, 2017 Massachusetts Institute of Technology

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

`include "ProcConfig.bsv"
// Fpu.bsv
// This file contains modules needed for hardware floating-point arithmetic.
// The FpuExec module abstracts away the ISA implementation simplifying the
// requirements for the hardware FPU units.

import BuildVector::*;
import Types::*;
import ProcTypes::*;
import FIFO::*;
import FIFOF::*;
import ClientServer::*;
import GetPut::*;
import Divide::*;
import SquareRoot::*;
import FloatingPoint::*;
import XilinxFpu::*;
import HasSpecBits::*;
import SpecFifo::*;
import SpecPoisonFifo::*;

export FpuResult(..);
export FpuResp(..);
export FpuExec(..);
export mkFpuExecPipeline;

typedef FloatingPoint::RoundMode FpuRoundMode;
typedef FloatingPoint::Exception FpuException;

function FloatingPoint#(e,m) canonicalNaN = FloatingPoint{sign: False, exp: '1, sfd: 1 << (valueof(m)-1)};

typedef struct {
    Data    data;
    Bit#(5) fflags;
} FpuResult deriving(Bits, Eq, FShow);

typedef struct {
    FpuResult res;
    Maybe#(PhyDst) dst;
    InstTag tag;
    // spec bits is not used in later stage, so not included here
} FpuResp deriving(Bits, Eq, FShow);

interface FpuExec;
    // input req
    method Action exec(FpuInst fpu_inst, Data rVal1, Data rVal2, Data rVal3,
                       Maybe#(PhyDst) dst, InstTag tag, SpecBits specBits);
    // output
    method ActionValue#(FpuResp) simpleResp;
    method ActionValue#(FpuResp) fmaResp;
    method ActionValue#(FpuResp) divResp;
    method ActionValue#(FpuResp) sqrtResp;
    // speculation
    interface SpeculationUpdate specUpdate;
endinterface

(* synthesize *)
module mkDoubleDiv(Server#(Tuple3#(Double, Double, FpuRoundMode), Tuple2#(Double, FpuException)));
`ifdef USE_XILINX_FPU
    let fpu <- mkXilinxFpDiv;
`else
    let int_div <- mkDivider(1); // [sizhuo] size in RVFpu: 2
    let fpu <- mkFloatingPointDivider(int_div);
`endif
    return fpu;
endmodule

(* synthesize *)
module mkDoubleSqrt(Server#(Tuple2#(Double, FpuRoundMode), Tuple2#(Double, FpuException)));
`ifdef USE_XILINX_FPU
    let fpu <- mkXilinxFpSqrt;
`else
    let int_sqrt <- mkSquareRooter(1); // [sizhuo] size in RVFpu: 3
    let fpu <- mkFloatingPointSquareRooter(int_sqrt);
`endif
    return fpu;
endmodule

// Bluespec FMA is a + b * c, and we have wrap Xilinx FMA also into this form
(* synthesize *)
module mkDoubleFMA(Server#(Tuple4#(Maybe#(Double), Double, Double, FpuRoundMode), Tuple2#(Double, FpuException)));
`ifdef USE_XILINX_FPU
    let fpu <- mkXilinxFpFma;
`else
    let fpu <- mkFloatingPointFusedMultiplyAccumulate;
`endif
    return fpu;
endmodule

// wrap FMA to an adder
function Tuple4#(Maybe#(Double), Double, Double, FpuRoundMode) getAddReqToFma(Tuple3#(Double, Double, FpuRoundMode) x);
    let in1 = tpl_1(x);
    let in2 = tpl_2(x);
    let rm = tpl_3(x);
    Double one_const = one(False);
    return tuple4(tagged Valid in1, in2, one_const, rm);
endfunction

// wrap FMA to a multiplier
function Tuple4#(Maybe#(Double), Double, Double, FpuRoundMode) getMulReqToFma(Tuple3#(Double, Double, FpuRoundMode) x);
    let in1 = tpl_1(x);
    let in2 = tpl_2(x);
    let rm = tpl_3(x);
    return tuple4(tagged Invalid, in1, in2, rm);
endfunction

// FCVT float -> float functions
function Tuple2#(Double, FpuException) fcvt_d_s (Float in, FpuRoundMode rmode);
    return convert(in, rmode, True);
endfunction
function Tuple2#(Float, FpuException) fcvt_s_d (Double in, FpuRoundMode rmode);
    return convert(in, rmode, True);
endfunction

// FCVT float -> int functions
function Tuple2#(Bit#(64), FpuException) fcvt_l_f (FloatingPoint#(e,m) in, FpuRoundMode rmode);
    return float_to_int(in, False, False, rmode);
endfunction
function Tuple2#(Bit#(64), FpuException) fcvt_lu_f (FloatingPoint#(e,m) in, FpuRoundMode rmode);
    return float_to_int(in, False, True, rmode);
endfunction
function Tuple2#(Bit#(64), FpuException) fcvt_w_f (FloatingPoint#(e,m) in, FpuRoundMode rmode);
    return float_to_int(in, True, False, rmode);
endfunction
function Tuple2#(Bit#(64), FpuException) fcvt_wu_f (FloatingPoint#(e,m) in, FpuRoundMode rmode);
    return float_to_int(in, True, True, rmode);
endfunction

// FCVT int -> float functions
function Tuple2#(FloatingPoint#(e,m), FpuException) fcvt_f_l (Bit#(64) in_bits, FpuRoundMode rmode)
        provisos (FixedFloatCVT#(FloatingPoint#(e, m), Int#(64)));
    Int#(64) in = unpack(in_bits);
    return vFixedToFloat(in, 1'b0, rmode);
endfunction
function Tuple2#(FloatingPoint#(e,m), FpuException) fcvt_f_lu (Bit#(64) in_bits, FpuRoundMode rmode)
        provisos (FixedFloatCVT#(FloatingPoint#(e, m), UInt#(64)));
    UInt#(64) in = unpack(in_bits);
    return vFixedToFloat(in, 1'b0, rmode);
endfunction
function Tuple2#(FloatingPoint#(e,m), FpuException) fcvt_f_w (Bit#(64) in_bits, FpuRoundMode rmode)
        provisos (FixedFloatCVT#(FloatingPoint#(e, m), Int#(32)));
    Int#(32) in = unpack(truncate(in_bits));
    return vFixedToFloat(in, 1'b0, rmode);
endfunction
function Tuple2#(FloatingPoint#(e,m), FpuException) fcvt_f_wu (Bit#(64) in_bits, FpuRoundMode rmode)
        provisos (FixedFloatCVT#(FloatingPoint#(e, m), UInt#(32)));
    UInt#(32) in = unpack(truncate(in_bits));
    return vFixedToFloat(in, 1'b0, rmode);
endfunction

function Tuple2#(Bit#(64), FpuException) fmin_s(Bit#(64) in1, Bit#(64) in2);
    Float in1_f = unpack(in1[31:0]);
    Float in2_f = unpack(in2[31:0]);
    Float nan_f = qnan(); // canonical NAN
    FpuException e = unpack(0);

    if (isSNaN(in1_f) || isSNaN(in2_f) || (isNaN(in1_f) && isNaN(in2_f))) begin
        e.invalid_op = True;
        return tuple2(zeroExtend(pack(nan_f)), e);
    end else if (isNaN(in2_f)) begin
        return tuple2(in1, e);
    end else if (isNaN(in1_f)) begin
        return tuple2(in2, e);
    end else begin
        let signLT = (in1_f.sign && !in2_f.sign);
        let signEQ = in1_f.sign == in2_f.sign;
        let absLT = {in1_f.exp, in1_f.sfd} < {in2_f.exp, in2_f.sfd};
        if (signLT || (signEQ && (in1_f.sign ? !absLT : absLT))) begin
            return tuple2(in1, e);
        end else begin
            return tuple2(in2, e);
        end
    end
endfunction

function Tuple2#(Bit#(64), FpuException) fmin_d(Bit#(64) in1, Bit#(64) in2);
    Double in1_f = unpack(in1);
    Double in2_f = unpack(in2);
    Double nan_f = qnan(); // canonical NAN
    FpuException e = unpack(0);

    if (isSNaN(in1_f) || isSNaN(in2_f) || (isNaN(in1_f) && isNaN(in2_f))) begin
        e.invalid_op = True;
        return tuple2(pack(nan_f), e);
    end else if (isNaN(in2_f)) begin
        return tuple2(in1, e);
    end else if (isNaN(in1_f)) begin
        return tuple2(in2, e);
    end else begin
        let signLT = (in1_f.sign && !in2_f.sign);
        let signEQ = in1_f.sign == in2_f.sign;
        let absLT = {in1_f.exp, in1_f.sfd} < {in2_f.exp, in2_f.sfd};
        if (signLT || (signEQ && (in1_f.sign ? !absLT : absLT))) begin
            return tuple2(in1, e);
        end else begin
            return tuple2(in2, e);
        end
    end
endfunction

function Tuple2#(Bit#(64), FpuException) fmax_s(Bit#(64) in1, Bit#(64) in2);
    Float in1_f = unpack(in1[31:0]);
    Float in2_f = unpack(in2[31:0]);
    Float nan_f = qnan(); // canonical NAN
    FpuException e = unpack(0);

    if (isSNaN(in1_f) || isSNaN(in2_f) || (isNaN(in1_f) && isNaN(in2_f))) begin
        e.invalid_op = True;
        return tuple2(zeroExtend(pack(nan_f)), e);
    end else if (isNaN(in2_f)) begin
        return tuple2(in1, e);
    end else if (isNaN(in1_f)) begin
        return tuple2(in2, e);
    end else begin
        let signGT = (!in1_f.sign && in2_f.sign);
        let signEQ = in1_f.sign == in2_f.sign;
        let absGT = {in1_f.exp, in1_f.sfd} > {in2_f.exp, in2_f.sfd};
        if (signGT || (signEQ && (in1_f.sign ? !absGT : absGT))) begin
            return tuple2(in1, e);
        end else begin
            return tuple2(in2, e);
        end
    end
endfunction

function Tuple2#(Bit#(64), FpuException) fmax_d(Bit#(64) in1, Bit#(64) in2);
    Double in1_f = unpack(in1);
    Double in2_f = unpack(in2);
    Double nan_f = qnan(); // canonical NAN
    FpuException e = unpack(0);

    if (isSNaN(in1_f) || isSNaN(in2_f) || (isNaN(in1_f) && isNaN(in2_f))) begin
        e.invalid_op = True;
        return tuple2(pack(nan_f), e);
    end else if (isNaN(in2_f)) begin
        return tuple2(in1, e);
    end else if (isNaN(in1_f)) begin
        return tuple2(in2, e);
    end else begin
        let signGT = (!in1_f.sign && in2_f.sign);
        let signEQ = in1_f.sign == in2_f.sign;
        let absGT = {in1_f.exp, in1_f.sfd} > {in2_f.exp, in2_f.sfd};
        if (signGT || (signEQ && (in1_f.sign ? !absGT : absGT))) begin
            return tuple2(in1, e);
        end else begin
            return tuple2(in2, e);
        end
    end
endfunction

function Tuple2#(Bit#(64), FpuException) float_to_int(
    FloatingPoint#(e, m) in, Bool is_32bit, Bool is_unsigned, FpuRoundMode rmode
);
    // 3 cases of exponents:
    Bit#(64) out = 0;
    Bit#(64) max_val = is_unsigned ? '1 : (is_32bit ? 64'h000000007FFFFFFF : 64'h7FFFFFFFFFFFFFFF);
    Bit#(64) min_val = is_unsigned ? 0 : (is_32bit ? 64'hFFFFFFFF80000000 : 64'h8000000000000000);

    FpuException exc = unpack(0);
    if (isNaN(in)) begin
        out = max_val;
        exc.invalid_op = True;
    end else if (isInfinity(in)) begin
        out = in.sign ? min_val : max_val;
        exc.invalid_op = True;
    end else if (isZero(in)) begin
        out = 0;
    end else begin
        // Now actually do the conversion
        Int#(TAdd#(e,TLog#(m))) bias_exp = fromInteger((2**(valueOf(e)-1))-1);
        Int#(TAdd#(e,TLog#(m))) in_exp = unpack(zeroExtend(in.exp));
        // The bottom two bits of int_val will be fractional data.
        // The bottom bit holds information about all bits with lesser
        // significance that were shifted out - same for top bit - this is
        // necessary for rounding and overflow detection.
        Bit#(TAdd#(66,m)) int_val = {64'b1, in.sfd, 2'b0}; // this is 2**m times larger than it should be - we will shift this to correct for that
        int_val = saturating_shift_right(int_val, fromInteger(valueOf(m)) + bias_exp - in_exp);
        
        // do rounding
        // 00 : exact
        // 01 : < 0.5
        // 10 : = 0.5
        // 11 : > 0.5
        Bool round_up = False; // by magnitude (default behavior will be drop bits)
        if (int_val[1:0] != 0) begin
            exc.inexact = True;
            case (rmode)
                Rnd_Nearest_Even:       round_up = (int_val[1:0] == 2'b11) || ((int_val[1:0] == 2'b10) && (int_val[2] == 1));
                Rnd_Nearest_Away_Zero:  round_up = (int_val[1] == 1);
                Rnd_Plus_Inf:           round_up = !in.sign;
                Rnd_Minus_Inf:          round_up = in.sign;
                Rnd_Zero:               round_up = False;
            endcase
        end

        // Take the integer part of int_val and round it up if necessary
        Bit#(TAdd#(64,m)) int_val_rnd = truncateLSB(int_val) + (round_up ? 1 : 0);
        // correct the output sign for negative numbers rounded to 0
        Bool out_sign = (int_val_rnd == 0) ? False : in.sign;

        // Now check to see if int_val_rnd is in range
        Bit#(TAdd#(64,m)) mask_32bit = {0, 32'hFFFFFFFF};
        Bit#(TAdd#(64,m)) mask_64bit = {0, 64'hFFFFFFFFFFFFFFFF};
        if (is_unsigned) begin
            if (out_sign) begin
                // negative number - out of range
                out = 0;
                exc.invalid_op = True;
            end else begin
                // positive number
                if (is_32bit) begin
                    // WU
                    if ((int_val_rnd & mask_32bit) == int_val_rnd) begin
                        Bit#(32) val = truncate(int_val_rnd);
                        out = signExtend(val);
                    end else begin
                        // out of range
                        out = '1;
                        exc.invalid_op = True;
                    end
                end else begin
                    // LU
                    if ((int_val_rnd & mask_64bit) == int_val_rnd) begin
                        out = truncate(int_val_rnd);
                    end else begin
                        // out of range
                        out = '1;
                        exc.invalid_op = True;
                    end
                end
            end
        end else begin
            // signed
            if (is_32bit) begin
                // W
                Bit#(32) max_val = out_sign ? 32'h80000000 : 32'h7FFFFFFF;
                if (((int_val_rnd & mask_32bit) == int_val_rnd) && (truncate(int_val_rnd) <= max_val)) begin
                    Bit#(32) val = truncate(int_val_rnd);
                    val = out_sign ? ((~val) + 1) : val;
                    out = signExtend(val);
                end else begin
                    // out of range
                    out = signExtend(max_val);
                    exc.invalid_op = True;
                end
            end else begin
                // L
                Bit#(64) max_val = out_sign ? 64'h8000000000000000 : 64'h7FFFFFFFFFFFFFFF;
                if (((int_val_rnd & mask_64bit) == int_val_rnd) && (truncate(int_val_rnd) <= max_val)) begin
                    Bit#(64) val = truncate(int_val_rnd);
                    out = out_sign ? ((~val) + 1) : val;
                end else begin
                    // out of range
                    out = max_val;
                    exc.invalid_op = True;
                end
            end
        end
    end
    if (exc.invalid_op) begin
        // by convention
        exc.inexact = False;
    end
    return tuple2(out, exc);
endfunction

function Bit#(n) saturating_shift_right(Bit#(n) in, Int#(m) amt)
        provisos (Add#(a__,1,n));
    // This function saturates in each direction
    Bool amt_sign = msb(amt) == 1;
    Bit#(m) amt_abs = amt_sign ? ((~pack(amt))+1) : pack(amt);
    Bit#(n) shifted = amt_sign ? (in << amt_abs) : (in >> amt_abs);
    Bit#(n) shifted_out_mask = amt_sign ? ~('1 >> amt_abs) : ~('1 << amt_abs);
    Bit#(1) saturated_bit = |(in & shifted_out_mask);
    shifted = amt_sign ? (shifted | {saturated_bit, 0}) : (shifted | {0, saturated_bit});
    return shifted;
endfunction

// exec function for simple operations
(* noinline *)
function FpuResult execFpuSimple(FpuInst fpu_inst, Data rVal1, Data rVal2);
    FpuResult fpu_result = FpuResult{data: 0, fflags: 0};

    // Convert the Risc-V RVRoundMode to FloatingPoint::RoundMode
    FpuRoundMode fpu_rm = (case (fpu_inst.rm)
            RNE:      Rnd_Nearest_Even;
            RTZ:      Rnd_Zero;
            RDN:      Rnd_Minus_Inf;
            RUP:      Rnd_Plus_Inf;
            RMM:      Rnd_Nearest_Away_Zero;
            RDyn:     Rnd_Nearest_Even;
            default:  Rnd_Nearest_Even;
        endcase);

    if (fpu_inst.precision == Single) begin
        // single precision
        Float in1 = unpack(rVal1[31:0]);
        Float in2 = unpack(rVal2[31:0]);
        Float dst = unpack(0);
        Maybe#(Data) full_dst = Invalid;
        FpuException e = unpack(0);
        let fpu_f = fpu_inst.func;
        // Fpu Decoding
        case (fpu_f)
            // combinational instructions
            FMin:     begin
                Data x;
                {x, e} = fmin_s(rVal1, rVal2);
                full_dst = tagged Valid x;
            end
            FMax:     begin
                Data x;
                {x, e} = fmax_s(rVal1, rVal2);
                full_dst = tagged Valid x;
            end
            FEq:        dst = unpack(zeroExtend(pack(compareFP(in1, in2) == EQ)));
            FLt:        begin
                dst = unpack(zeroExtend(pack(compareFP(in1, in2) == LT)));
                if (isNaN(in1) || isNaN(in2)) begin
                    e.invalid_op = True;
                end
            end
            FLe:        begin
                dst = unpack(zeroExtend(pack((compareFP(in1, in2) == LT) || (compareFP(in1, in2) == EQ))));
                if (isNaN(in1) || isNaN(in2)) begin
                    e.invalid_op = True;
                end
            end
            // CLASS functions
            FClass: begin
                Bool exp_0s = (in1.exp == 0);
                Bool exp_1s = (in1.exp == '1);
                Bool sfd_0s = (in1.sfd == 0);
                Bit#(10) res = 0;
                res[0] = pack(in1.sign && exp_1s && sfd_0s);                // -inf
                res[1] = pack(in1.sign && !exp_1s && !exp_0s);              // -normal
                res[2] = pack(in1.sign && exp_0s && !sfd_0s);               // -subnormal
                res[3] = pack(in1.sign && exp_0s && sfd_0s);                // -0
                res[4] = pack(!in1.sign && exp_0s && sfd_0s);               // +0
                res[5] = pack(!in1.sign && exp_0s && !sfd_0s);              // +subnormal
                res[6] = pack(!in1.sign && !exp_1s && !exp_0s);             // +normal
                res[7] = pack(!in1.sign && exp_1s && sfd_0s);               // -inf
                res[8] = pack(exp_1s && !sfd_0s && (msb(in1.sfd) == 0));    // signaling NaN
                res[9] = pack(exp_1s && !sfd_0s && (msb(in1.sfd) == 1));    // quiet NaN
                full_dst = tagged Valid zeroExtend(res);
            end
            // Sign Injection
            FSgnj:    begin
                dst = in1;
                dst.sign = in2.sign;
            end
            FSgnjn: begin
                dst = in1;
                dst.sign = !in2.sign;
            end
            FSgnjx: begin
                dst = in1;
                dst.sign = unpack(pack(in1.sign) ^ pack(in2.sign));
            end
            // Float -> Bits
            FMv_XF:     full_dst = tagged Valid signExtend(pack(in1));
            // Bits -> Float
            FMv_FX:     full_dst = tagged Valid zeroExtend(pack(in1));
            // Float -> Float
            FCvt_FF:    begin
                Double in1_double = unpack(rVal1);
                {dst, e} = fcvt_s_d(in1_double, fpu_rm);
                if (isNaN(dst)) dst = canonicalNaN;
            end
            // Float -> Int
            FCvt_WF:    begin
                Data dst_bits;
                {dst_bits, e} = fcvt_w_f(in1, fpu_rm);
                full_dst = tagged Valid dst_bits;
            end
            FCvt_WUF: begin
                Data dst_bits;
                {dst_bits, e} = fcvt_wu_f(in1, fpu_rm);
                full_dst = tagged Valid dst_bits;
            end
            FCvt_LF:    begin
                Data dst_bits;
                {dst_bits, e} = fcvt_l_f(in1, fpu_rm);
                full_dst = tagged Valid dst_bits;
            end
            FCvt_LUF: begin
                Data dst_bits;
                {dst_bits, e} = fcvt_lu_f(in1, fpu_rm);
                full_dst = tagged Valid dst_bits;
            end
            // Int -> Float
            FCvt_FW: begin
                {dst, e} = fcvt_f_w(rVal1, fpu_rm);
                if (isNaN(dst)) dst = canonicalNaN;
            end
            FCvt_FWU: begin
                {dst, e} = fcvt_f_wu(rVal1, fpu_rm);
                if (isNaN(dst)) dst = canonicalNaN;
            end
            FCvt_FL: begin
                {dst, e} = fcvt_f_l(rVal1, fpu_rm);
                if (isNaN(dst)) dst = canonicalNaN;
            end
            FCvt_FLU: begin
                {dst, e} = fcvt_f_lu(rVal1, fpu_rm);
                if (isNaN(dst)) dst = canonicalNaN;
            end
        endcase
        fpu_result.data = (full_dst matches tagged Valid .data ? data : zeroExtend(pack(dst)));
        fpu_result.fflags = pack(e);
    end else if (fpu_inst.precision == Double) begin
        // double precision
        Double in1 = unpack(rVal1);
        Double in2 = unpack(rVal2);
        Double dst = unpack(0);
        Maybe#(Data) full_dst = Invalid;
        FpuException e = unpack(0);
        let fpu_f = fpu_inst.func;
        // Fpu Decoding
        case (fpu_f)
            // combinational instructions
            FMin:     begin
                Data x;
                {x, e} = fmin_d(rVal1, rVal2);
                full_dst = tagged Valid x;
            end
            FMax:     begin
                Data x;
                {x, e} = fmax_d(rVal1, rVal2);
                full_dst = tagged Valid x;
            end
            FEq:        dst = unpack(zeroExtend(pack(compareFP(in1, in2) == EQ)));
            FLt:        begin
                dst = unpack(zeroExtend(pack(compareFP(in1, in2) == LT)));
                if (isNaN(in1) || isNaN(in2)) begin
                    e.invalid_op = True;
                end
            end
            FLe:        begin
                dst = unpack(zeroExtend(pack((compareFP(in1, in2) == LT) || (compareFP(in1, in2) == EQ))));
                if (isNaN(in1) || isNaN(in2)) begin
                    e.invalid_op = True;
                end
            end
            // CLASS functions
            FClass: begin
                Bool exp_0s = (in1.exp == 0);
                Bool exp_1s = (in1.exp == '1);
                Bool sfd_0s = (in1.sfd == 0);
                Bit#(10) res = 0;
                res[0] = pack(in1.sign && exp_1s && sfd_0s);                // -inf
                res[1] = pack(in1.sign && !exp_1s && !exp_0s);              // -normal
                res[2] = pack(in1.sign && exp_0s && !sfd_0s);               // -subnormal
                res[3] = pack(in1.sign && exp_0s && sfd_0s);                // -0
                res[4] = pack(!in1.sign && exp_0s && sfd_0s);               // +0
                res[5] = pack(!in1.sign && exp_0s && !sfd_0s);              // +subnormal
                res[6] = pack(!in1.sign && !exp_1s && !exp_0s);             // +normal
                res[7] = pack(!in1.sign && exp_1s && sfd_0s);               // -inf
                res[8] = pack(exp_1s && !sfd_0s && (msb(in1.sfd) == 0));    // signaling NaN
                res[9] = pack(exp_1s && !sfd_0s && (msb(in1.sfd) == 1));    // quiet NaN
                full_dst = tagged Valid zeroExtend(res);
            end
            // Sign Injection
            FSgnj:    begin
                dst = in1;
                dst.sign = in2.sign;
            end
            FSgnjn: begin
                dst = in1;
                dst.sign = !in2.sign;
            end
            FSgnjx: begin
                dst = in1;
                dst.sign = unpack(pack(in1.sign) ^ pack(in2.sign));
            end
            // Float -> Bits
            FMv_XF:     full_dst = tagged Valid pack(in1);
            // Bits -> Float
            FMv_FX:     full_dst = tagged Valid pack(in1);
            // Float -> Float
            FCvt_FF:    begin
                Float in1_float = unpack(rVal1[31:0]);
                {dst, e} = fcvt_d_s(in1_float, fpu_rm);
                if (isNaN(dst)) dst = canonicalNaN;
            end
            // Float -> Int
            FCvt_WF:    begin
                Data dst_bits;
                {dst_bits, e} = fcvt_w_f(in1, fpu_rm);
                full_dst = tagged Valid dst_bits;
            end
            FCvt_WUF: begin
                Data dst_bits;
                {dst_bits, e} = fcvt_wu_f(in1, fpu_rm);
                full_dst = tagged Valid dst_bits;
            end
            FCvt_LF:    begin
                Data dst_bits;
                {dst_bits, e} = fcvt_l_f(in1, fpu_rm);
                full_dst = tagged Valid dst_bits;
            end
            FCvt_LUF: begin
                Data dst_bits;
                {dst_bits, e} = fcvt_lu_f(in1, fpu_rm);
                full_dst = tagged Valid dst_bits;
            end
            // Int -> Float
            FCvt_FW: begin
                {dst, e} = fcvt_f_w(rVal1, fpu_rm);
                if (isNaN(dst)) dst = canonicalNaN;
            end
            FCvt_FWU: begin
                {dst, e} = fcvt_f_wu(rVal1, fpu_rm);
                if (isNaN(dst)) dst = canonicalNaN;
            end
            FCvt_FL: begin
                {dst, e} = fcvt_f_l(rVal1, fpu_rm);
                if (isNaN(dst)) dst = canonicalNaN;
            end
            FCvt_FLU: begin
                {dst, e} = fcvt_f_lu(rVal1, fpu_rm);
                if (isNaN(dst)) dst = canonicalNaN;
            end
        endcase
        fpu_result.data = (full_dst matches tagged Valid .data ? data : pack(dst));
        fpu_result.fflags = pack(e);
    end
    return fpu_result;
endfunction

// Spec FIFO in parallel with FP units. Div and Sqrt should not be frequent, so
// we use small FIFOs. FMA may be mroe frequent, so match its latency
typedef enum {Fma, Div, Sqrt} FpuFuncUnit deriving(Bits, Eq, FShow);
typedef struct {
    // fpu inst specific
    FpuRoundMode roundMode;
    FpuPrecision precision;
    FpuException exc_conv_in; // exception during convert single input to double
    Bool negateResult;
    // generic bookkeeping
    Maybe#(PhyDst) dst;
    InstTag tag;
} FpuExecInfo deriving(Bits, Eq, FShow);

typedef SpecPoisonFifo#(n, FpuExecInfo) FpuExecQ#(numeric type n);
module mkFpuExecQ(FpuExecQ#(n));
    let m <- mkSpecPoisonFifo(True); // lazy enq
    return m;
endmodule

typedef FpuExecQ#(2) MinimumExecQ;
(* synthesize *)
module mkMinimumExecQ(MinimumExecQ);
    let m <- mkFpuExecQ;
    return m;
endmodule

typedef FpuExecQ#(`BOOKKEEPING_FP_FMA_SIZE) FmaExecQ;
(* synthesize *)
module mkFmaExecQ(FmaExecQ);
    let m <- mkFpuExecQ;
    return m;
endmodule

// for simple ops that do not go to func units, we have a respQ for them
typedef SpecFifo_SB_deq_enq_C_deq_enq#(2, FpuResp) SimpleRespQ;
(* synthesize *)
module mkSimpleRespQ(SimpleRespQ);
    let m <- mkSpecFifo_SB_deq_enq_C_deq_enq(True); // lazy enq
    return m;
endmodule

// don't synthesize to optimize guard for exec method
module mkFpuExecPipeline(FpuExec);
    // simple req that can be done combinationally
    let simpleQ <- mkSimpleRespQ;
    // spec fifos in parallel with each func unit
    let fmaQ <- mkFmaExecQ;
    let divQ <- mkMinimumExecQ;
    let sqrtQ <- mkMinimumExecQ;

    // Pipelined units
    let double_fma <- mkDoubleFMA;
    let double_div <- mkDoubleDiv;
    let double_sqrt <- mkDoubleSqrt;

    // post processing of results that come out of function unit
    function FpuResp finalizeResp(FpuExecInfo info, Double out, FpuException exc_op);
        FpuResult res;
        if(info.precision == Single) begin
            // convert out back to single
            let {out_f, exc_conv_out} = fcvt_s_d(out, info.roundMode);
            // negate result if needed
            if(info.negateResult) begin
                out_f = -out_f;
            end
            // canonicalize NaN
            out_f = isNaN(out_f) ? canonicalNaN : out_f;
            res = FpuResult {
                data: zeroExtend(pack(out_f)),
                fflags: pack(info.exc_conv_in | exc_op | exc_conv_out)
            };
        end
        else begin
            // no convert is needed
            // negate result if needed
            if(info.negateResult) begin
                out = -out;
            end
            // canonicalize NaN
            out = isNaN(out) ? canonicalNaN : out;
            res = FpuResult {
                data: pack(out),
                fflags: pack(exc_op) // info.exc_conv_in should be 0
            };
        end
        return FpuResp {
            res: res,
            dst: info.dst,
            tag: info.tag
        };
    endfunction

    // drain poisoned insts
    rule deqFmaPoisoned(fmaQ.first_poisoned);
        fmaQ.deq;
        let x <- double_fma.response.get;
    endrule
    rule deqDivPoisoned(divQ.first_poisoned);
        divQ.deq;
        let x <- double_div.response.get;
    endrule
    rule deqSqrtPoisoned(sqrtQ.first_poisoned);
        sqrtQ.deq;
        let x <- double_sqrt.response.get;
    endrule

    method Action exec(FpuInst fpu_inst, Data rVal1, Data rVal2, Data rVal3,
                       Maybe#(PhyDst) dst, InstTag tag, SpecBits spec_bits);
        // Convert the Risc-V RVRoundMode to FloatingPoint::RoundMode
        FpuRoundMode fpu_rm = (case (fpu_inst.rm)
                RNE:        Rnd_Nearest_Even;
                RTZ:        Rnd_Zero;
                RDN:        Rnd_Minus_Inf;
                RUP:        Rnd_Plus_Inf;
                RMM:        Rnd_Nearest_Away_Zero;
                RDyn:       Rnd_Nearest_Even;
                default:    Rnd_Nearest_Even;
            endcase);

        // convert float inputs to double: may have exceptions
        FpuException exc_conv = unpack(0);
        Double in1 = unpack(rVal1);
        Double in2 = unpack(rVal2);
        Double in3 = unpack(rVal3);
        if (fpu_inst.precision == Single) begin
            // conver single to double
            Float f1 = unpack(rVal1[31:0]);
            Float f2 = unpack(rVal2[31:0]);
            Float f3 = unpack(rVal3[31:0]);
            let {d1, exc1} = fcvt_d_s(f1, fpu_rm);
            let {d2, exc2} = fcvt_d_s(f2, fpu_rm);
            let {d3, exc3} = fcvt_d_s(f3, fpu_rm);
            in1 = d1;
            in2 = d2;
            in3 = d3;
            // get exception
            case (fpu_inst.func)
                FSqrt: begin
                    exc_conv = exc1;
                end
                FAdd, FSub, FMul, FDiv: begin
                    exc_conv = exc1 | exc2;
                end
                FMAdd, FMSub, FNMSub, FNMAdd: begin
                    exc_conv = exc1 | exc2 | exc3;
                end
            endcase
        end

        // request function unit
        case (fpu_inst.func)
            FAdd:   double_fma.request.put(getAddReqToFma(tuple3(in1, in2, fpu_rm)));
            FSub:   double_fma.request.put(getAddReqToFma(tuple3(in1, -in2, fpu_rm)));
            FMul:   double_fma.request.put(getMulReqToFma(tuple3(in1, in2, fpu_rm)));
            FDiv:   double_div.request.put(tuple3(in1, in2, fpu_rm));
            FSqrt:  double_sqrt.request.put(tuple2(in1, fpu_rm));
            // Bluespec FMA(a, b, c) is a + b * c, RISC-V ISA needs in1 *
            // in2 + in3, so we need to shuffle the inputs
            FMAdd:  double_fma.request.put(tuple4(tagged Valid in3, in1, in2, fpu_rm));
            FMSub:  double_fma.request.put(tuple4(tagged Valid (-in3), in1, in2, fpu_rm));
            FNMSub: double_fma.request.put(tuple4(tagged Valid (-in3), in1, in2, fpu_rm));
            FNMAdd: double_fma.request.put(tuple4(tagged Valid in3, in1, in2, fpu_rm));
        endcase

        // enq to spec fifo
        let info = FpuExecInfo {
            roundMode: fpu_rm,
            precision: fpu_inst.precision,
            exc_conv_in: exc_conv,
            negateResult: fpu_inst.func == FNMSub || fpu_inst.func == FNMAdd,
            dst: dst,
            tag: tag
        };
        case (fpu_inst.func)
            FAdd, FSub, FMul, FMAdd, FMSub, FNMSub, FNMAdd: begin
                fmaQ.enq(ToSpecFifo {
                    data: info,
                    spec_bits: spec_bits
                });
            end
            FDiv: begin
                divQ.enq(ToSpecFifo {
                    data: info,
                    spec_bits: spec_bits
                });
            end
            FSqrt: begin
                sqrtQ.enq(ToSpecFifo{
                    data: info,
                    spec_bits: spec_bits
                });
            end
            default: begin
                // simple ops that can be directly handled
                FpuResult fpu_result = execFpuSimple(fpu_inst, rVal1, rVal2);
                simpleQ.enq(ToSpecFifo {
                    data: FpuResp {
                        res: fpu_result,
                        dst: dst,
                        tag: tag
                    },
                    spec_bits: spec_bits
                });
            end
        endcase
    endmethod

    method ActionValue#(FpuResp) simpleResp;
        simpleQ.deq;
        return simpleQ.first.data;
    endmethod

    method ActionValue#(FpuResp) fmaResp if(!fmaQ.first_poisoned);
        fmaQ.deq;
        let {out, exc} <- double_fma.response.get;
        return finalizeResp(fmaQ.first_data.data, out, exc);
    endmethod

    method ActionValue#(FpuResp) divResp if(!divQ.first_poisoned);
        divQ.deq;
        let {out, exc} <- double_div.response.get;
        return finalizeResp(divQ.first_data.data, out, exc);
    endmethod

    method ActionValue#(FpuResp) sqrtResp if(!sqrtQ.first_poisoned);
        sqrtQ.deq;
        let {out, exc} <- double_sqrt.response.get;
        return finalizeResp(sqrtQ.first_data.data, out, exc);
    endmethod

    interface specUpdate = joinSpeculationUpdate(vec(
        simpleQ.specUpdate,
        fmaQ.specUpdate,
        divQ.specUpdate,
        sqrtQ.specUpdate
    ));
endmodule

