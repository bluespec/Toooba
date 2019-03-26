
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

`include "ProcConfig.bsv"
import BuildVector::*;
import Types::*;
import ProcTypes::*;
import Fifo::*;
import FIFO::*;
import XilinxIntMul::*;
import XilinxIntDiv::*;
import HasSpecBits::*;
import SpecFifo::*;
import SpecPoisonFifo::*;

export MulDivResp(..);
export MulDivExec(..);
export mkMulDivExec;

function Bool isMulFunc(MulDivFunc func);
    return (case(func)
        Mul, Mulh: True;
        default: False;
    endcase);
endfunction

function Bool isDivFunc(MulDivFunc func);
    return (case(func)
        Div, Rem: True;
        default: False;
    endcase);
endfunction

function XilinxIntMulSign getXilinxMulSign(MulDivSign s);
    return (case(s)
        Signed: (Signed);
        Unsigned: (Unsigned);
        default: (SignedUnsigned);
    endcase);
endfunction

function Bool isDivSigned(MulDivSign s);
    return s == Signed; // there is no SignedUnsigned for div
endfunction

// resp type
typedef struct {
    Data data;
    Maybe#(PhyDst) dst;
    InstTag tag;
    // spec bits is not used in later stage, so not included here
} MulDivResp deriving(Bits, Eq, FShow);

interface MulDivExec;
    // input req
    method Action exec(MulDivInst mdInst, Data rVal1, Data rVal2,
                       Maybe#(PhyDst) dst, InstTag tag, SpecBits spec_bits);
    // output
    method ActionValue#(MulDivResp) mulResp;
    method ActionValue#(MulDivResp) divResp;
    // speculation
    interface SpeculationUpdate specUpdate;
endinterface

// spec fifos in parallel with func units
typedef struct {
    MulDivFunc func;
    Bool w; // op32
    // generic bookkeepings
    Maybe#(PhyDst) dst;
    InstTag tag;
} MulDivExecInfo deriving(Bits, Eq, FShow);

typedef SpecPoisonFifo#(`BOOKKEEPING_INT_MUL_SIZE, MulDivExecInfo) MulExecQ;
(* synthesize *)
module mkMulExecQ(MulExecQ);
    let m <- mkSpecPoisonFifo(True); // lazy enq
    return m;
endmodule

typedef SpecPoisonFifo#(2, MulDivExecInfo) DivExecQ;
(* synthesize *)
module mkDivExecQ(DivExecQ);
    let m <- mkSpecPoisonFifo(True); // lazy enq
    return m;
endmodule

// don't synthesize to optimize guard of exec
module mkMulDivExec(MulDivExec);
    Bool verbose = False;

    XilinxIntMul#(void) mulUnit <- mkXilinxIntMul;
    XilinxIntDiv#(void) divUnit <- mkXilinxIntDiv;

    let mulQ <- mkMulExecQ;
    let divQ <- mkDivExecQ;

    // drain poisoned resp
    rule deqMulPoisoned(mulQ.first_poisoned);
        mulQ.deq;
        mulUnit.deqResp;
    endrule
    rule deqDivPoisoned(divQ.first_poisoned);
        divQ.deq;
        divUnit.deqResp;
    endrule

    method Action exec(MulDivInst mdInst, Data rVal1, Data rVal2,
                       Maybe#(PhyDst) dst, InstTag tag, SpecBits spec_bits);
        if(verbose) begin
            $display("[MulDiv] ", fshow(mdInst), ", ",
                     fshow(rVal1), ", ", fshow(rVal2));
        end

        // get the operands, we need to extend correctly in case of OP32. This
        // does not matter for MULW but matters for DIVW.
        Data a = rVal1;
        Data b = rVal2;
        if(mdInst.w) begin
            a = mdInst.sign == Unsigned ? zeroExtend(rVal1[31:0]) :
                                          signExtend(rVal1[31:0]);
            b = mdInst.sign == Signed ? signExtend(rVal2[31:0]) :
                                        zeroExtend(rVal2[31:0]);
        end

        // issue to func unit & bookkeeping fifo
        let info = MulDivExecInfo {
            func: mdInst.func,
            w: mdInst.w,
            dst: dst,
            tag: tag
        };
        if(isMulFunc(mdInst.func)) begin
            mulUnit.req(a, b, getXilinxMulSign(mdInst.sign), ?);
            mulQ.enq(ToSpecFifo {
                data: info,
                spec_bits: spec_bits
            });
        end
        else begin
            divUnit.req(a, b, isDivSigned(mdInst.sign), ?);
            divQ.enq(ToSpecFifo {
                data: info,
                spec_bits: spec_bits
            });
        end
    endmethod

    // output
    method ActionValue#(MulDivResp) mulResp if(!mulQ.first_poisoned);
        mulUnit.deqResp;
        mulQ.deq;
        let info = mulQ.first_data.data;
        doAssert(isMulFunc(info.func), "must be mul func");
        // get result data
        Data data = (case(info.func)
            Mul  : (truncate(mulUnit.product));
            Mulh : (truncateLSB(mulUnit.product));
            default: ?;
        endcase);
        // correct for OP32 instructions
        if (info.w) begin
            data = signExtend(data[31:0]);
        end
        return MulDivResp {
            data: data,
            dst: info.dst,
            tag: info.tag
        };
    endmethod

    method ActionValue#(MulDivResp) divResp if(!divQ.first_poisoned);
        divUnit.deqResp;
        divQ.deq;
        let info = divQ.first_data.data;
        doAssert(isDivFunc(info.func), "must be div func");
        // get result data
        Data data = (case(info.func)
            Div  : (divUnit.quotient);
            Rem  : (divUnit.remainder);
            default: ?;
        endcase);
        // correct for OP32 instructions
        if (info.w) begin
            data = signExtend(data[31:0]);
        end
        return MulDivResp {
            data: data,
            dst: info.dst,
            tag: info.tag
        };
    endmethod

    interface specUpdate = joinSpeculationUpdate(vec(
        mulQ.specUpdate,
        divQ.specUpdate
    ));
endmodule

