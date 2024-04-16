
// Copyright (c) 2017 Massachusetts Institute of Technology
//
//-
// RVFI_DII + CHERI modifications:
//     Copyright (c) 2020 Jonathan Woodruff
//     Copyright (c) 2024 Franz Fuchs
//     All rights reserved.
//
//     This software was developed by SRI International and the University of
//     Cambridge Computer Laboratory (Department of Computer Science and
//     Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
//     DARPA SSITH research programme.
//
//     This work was supported by NCSC programme grant 4212611/RFA 15971 ("SafeBet").
//
//     This software was developed by the University of  Cambridge
//     Department of Computer Science and Technology under the
//     SIPP (Secure IoT Processor Platform with Remote Attestation)
//     project funded by EPSRC: EP/S030868/1
//-
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
import DefaultValue::*;
import Cntrs::*;
import Vector::*;
import BuildVector::*;
import Types::*;
import ProcTypes::*;
import SynthParam::*;
import Exec::*;
import Performance::*;
import ReservationStationEhr::*;
import ReservationStationFpuMulDiv::*;
import ReorderBuffer::*;
import HasSpecBits::*;
import SpecFifo::*;
import MulDiv::*;
import Fpu::*;
import Bypass::*;
import CHERICap::*;
import CHERICC_Fat::*;
import ISA_Decls_CHERI::*;

`ifdef KONATA
import Cur_Cycle :: *;
`endif

typedef struct {
    // inst info
    ExecFunc execFunc;
    PhyRegs regs;
    InstTag tag;
    // FpuMulDiv must not have valid spec tag
`ifdef KONATA
    Bit#(64) u_id;
`endif
} FpuMulDivDispatchToRegRead deriving(Bits, Eq, FShow);

typedef struct {
    // inst info
    ExecFunc execFunc;
    Maybe#(PhyDst) dst;
    InstTag tag;
    // src reg vals
    Data rVal1;
    Data rVal2;
    Data rVal3;
`ifdef KONATA
    Bit#(64) u_id;
`endif
} FpuMulDivRegReadToExe deriving(Bits, Eq, FShow);

typedef struct {
    // inst info
    ExecFunc execFunc;
    Maybe#(PhyDst) dst;
    InstTag tag;
} FpuMulDivExeToFinish deriving(Bits, Eq, FShow);

// synthesized pipeline fifos
typedef SpecFifo_SB_deq_enq_C_deq_enq#(1, FpuMulDivDispatchToRegRead) FpuMulDivDispToRegFifo;
(* synthesize *)
module mkFpuMulDivDispToRegFifo(FpuMulDivDispToRegFifo);
    let m <- mkSpecFifo_SB_deq_enq_C_deq_enq(False);
    return m;
endmodule

typedef SpecFifo_SB_deq_enq_C_deq_enq#(1, FpuMulDivRegReadToExe) FpuMulDivRegToExeFifo;
(* synthesize *)
module mkFpuMulDivRegToExeFifo(FpuMulDivRegToExeFifo);
    let m <- mkSpecFifo_SB_deq_enq_C_deq_enq(False);
    return m;
endmodule

interface FpuMulDivExeInput;
    // conservative scoreboard check in reg read stage
    method RegsReady sbCons_lazyLookup(PhyRegs r);
    // Phys reg file
    method Data rf_rd1(PhyRIndx rindx);
    method Data rf_rd2(PhyRIndx rindx);
    method Data rf_rd3(PhyRIndx rindx);
    // CSR file
    method Data csrf_rd(CSR csr);
    // Special Capability Register file.
    method CapReg scaprf_rd(SCR csr);
    // ROB
    method Action rob_setExecuted(
        InstTag t,
`ifdef INCLUDE_TANDEM_VERIF
        Data dst_data,
`endif
        Bit#(5) fflags
`ifdef RVFI
        , ExtraTraceBundle tb
`endif
    );

    // global broadcast methods
    // write reg file & set both conservative and aggressive sb & wake up inst
    method Action writeRegFile(PhyRIndx dst, Data data);
    // spec update
    method Action conflictWrongSpec;
    // performance
    method Bool doStats;
endinterface

interface FpuMulDivExePipeline;
    // recv bypass from the ALU exe and finish stages
    interface Vector#(TMul#(2, AluExeNum), RecvBypass) recvBypass;
    interface ReservationStationFpuMulDiv rsFpuMulDivIfc;
    interface SpeculationUpdate specUpdate;
    // performance
    method Data getPerf(ExeStagePerfType t);
endinterface

module mkFpuMulDivExePipeline#(FpuMulDivExeInput inIfc)(FpuMulDivExePipeline);
    Bool verbose = False;

    // fpu mul div reservation station
    ReservationStationFpuMulDiv rsFpuMulDiv <- mkReservationStationFpuMulDiv;

    // pipeline fifos
    let dispToRegQ <- mkFpuMulDivDispToRegFifo;
    let regToExeQ <- mkFpuMulDivRegToExeFifo;

    // wire to recv bypass
    Vector#(TMul#(2, AluExeNum), RWire#(Tuple2#(PhyRIndx, Data))) bypassWire <- replicateM(mkRWire);

    // mul div fpu func units
    MulDivExec mulDivExec <- mkMulDivExec;
    FpuExec fpuExec <- mkFpuExecPipeline;

    // fpu/mul/div performance counters
`ifdef PERF_COUNT
    Count#(Data) exeIntMulCnt <- mkCount(0);
    Count#(Data) exeIntDivCnt <- mkCount(0);
    Count#(Data) exeFpFmaCnt <- mkCount(0);
    Count#(Data) exeFpDivCnt <- mkCount(0);
    Count#(Data) exeFpSqrtCnt <- mkCount(0);
`endif

    rule doDispatchFpuMulDiv;
        rsFpuMulDiv.doDispatch;
        let x = rsFpuMulDiv.dispatchData;
        if(verbose) $display("[doDispatchFpuMulDiv] ", fshow(x));

        // FPU MUL DIV never have exception or misprecition, so no spec tag
        doAssert(!isValid(x.spec_tag), "FpuMulDiv should not carry any spec tag");

`ifdef KONATA 
        $display("KONATAE\t%0d\t%0d\t0\tRsvF", cur_cycle, x.u_id);
        $display("KONATAS\t%0d\t%0d\t0\tFpu1", cur_cycle, x.u_id);
        $fflush;
`endif
        // go to next stage
        dispToRegQ.enq(ToSpecFifo {
            data: FpuMulDivDispatchToRegRead {
                execFunc: x.data.execFunc,
                regs: x.regs,
                tag: x.tag
`ifdef KONATA
                , u_id: x.u_id
`endif
            },
            spec_bits: x.spec_bits
        });
    endrule

    rule doRegReadFpuMulDiv;
        dispToRegQ.deq;
        let dispToReg = dispToRegQ.first;
        let x = dispToReg.data;
        if(verbose) $display("[doRegReadFpuMulDiv] ", fshow(dispToReg));

        // check conservative scoreboard
        let regsReady = inIfc.sbCons_lazyLookup(x.regs);

        // get rVal1 (check bypass)
        Data rVal1 = ?;
        if(x.regs.src1 matches tagged Valid .src1) begin
            rVal1 <- readRFBypass(src1, regsReady.src1, inIfc.rf_rd1(src1), bypassWire);
        end

        // get rVal2 (check bypass)
        Data rVal2 = ?;
        if(x.regs.src2 matches tagged Valid .src2) begin
            rVal2 <- readRFBypass(src2, regsReady.src2, inIfc.rf_rd2(src2), bypassWire);
        end

        // get rVal3 (check bypass)
        Data rVal3 = ?;
        if(x.regs.src3 matches tagged Valid .src3) begin
            rVal3 <- readRFBypass(src3, regsReady.src3, inIfc.rf_rd3(src3), bypassWire);
        end

`ifdef KONATA 
        $display("KONATAE\t%0d\t%0d\t0\tFpu1", cur_cycle, x.u_id);
        $display("KONATAS\t%0d\t%0d\t0\tFpu2", cur_cycle, x.u_id);
        $fflush;
`endif
        // go to next stage
        regToExeQ.enq(ToSpecFifo {
            data: FpuMulDivRegReadToExe {
                execFunc: x.execFunc,
                dst: x.regs.dst,
                tag: x.tag,
                rVal1: rVal1,
                rVal2: rVal2,
                rVal3: rVal3
`ifdef KONATA
                , u_id: x.u_id
`endif
            },
            spec_bits: dispToReg.spec_bits
        });
    endrule

    rule doExeFpuMulDiv;
        regToExeQ.deq;
        let regToExe = regToExeQ.first;
        let x = regToExe.data;
        let spec_bits = regToExe.spec_bits;
        if(verbose) $display("[doExeFpuMulDiv] ", fshow(regToExe));

        // send to exe unit
        Data rVal1 = x.rVal1;
        Data rVal2 = x.rVal2;
        Data rVal3 = x.rVal3;
        case (x.execFunc) matches
            tagged Fpu .fpu_inst: begin
`ifdef KONATA
                fpuExec.exec(fpu_inst, rVal1, rVal2, rVal3, x.dst, x.tag, spec_bits, x.u_id);
                $display("KONATAE\t%0d\t%0d\t0\tFpu2", cur_cycle, x.u_id);
                $display("KONATAS\t%0d\t%0d\t0\tFpu3", cur_cycle, x.u_id);
                $fflush;
`else
                fpuExec.exec(fpu_inst, rVal1, rVal2, rVal3, x.dst, x.tag, spec_bits);
`endif
            end
            tagged MulDiv .muldiv_inst: begin
`ifdef KONATA
                mulDivExec.exec(muldiv_inst, rVal1, rVal2, x.dst, x.tag, spec_bits, x.u_id);
                $display("KONATAE\t%0d\t%0d\t0\tFpu2", cur_cycle, x.u_id);
                $display("KONATAS\t%0d\t%0d\t0\tFpu3", cur_cycle, x.u_id);
                $fflush;
`else
                mulDivExec.exec(muldiv_inst, rVal1, rVal2, x.dst, x.tag, spec_bits);
`endif
            end
            default: begin
                doAssert(False, "unknown execFunc for doExeFpuMulDiv");
            end
        endcase
    endrule

`ifdef KONATA
    function Action doFinish(Maybe#(PhyDst) dst, InstTag tag, Data data, Bit#(5) fflags, Bit#(64) u_id);
`else
    function Action doFinish(Maybe#(PhyDst) dst, InstTag tag, Data data, Bit#(5) fflags);
`endif
    action
        // write to register file
        if(dst matches tagged Valid .valid_dst) begin
            inIfc.writeRegFile(valid_dst.indx, data);
        end
        // update the instruction in the reorder buffer.
`ifdef KONATA 
        $display("KONATAE\t%0d\t%0d\t0\tFpu3", cur_cycle, u_id);
        $display("KONATAS\t%0d\t%0d\t0\tFpu4", cur_cycle, u_id);
        $fflush;
`endif
        inIfc.rob_setExecuted(tag,
`ifdef INCLUDE_TANDEM_VERIF
                              data,
`endif
                              fflags
`ifdef RVFI
                              , ExtraTraceBundle{regWriteData: data, memByteEn: ?}
`endif
        );
        // since FPU op has no spec tag, this doFinish rule is ordered before
        // other rules that calls incorrectSpec, and BSV compiler creates
        // cycles in scheduling. We manually creates a conflict between this
        // rule and incorrectSpec to break the cycle
        //inIfc.conflictWrongSpec;
    endaction
    endfunction

    rule doFinishFpSimple;
        FpuResp resp <- fpuExec.simpleResp;
        if(verbose) $display("[doFinishFpSimple] ", fshow(resp));
`ifdef KONATA
        doFinish(resp.dst, resp.tag, resp.res.data, resp.res.fflags, resp.u_id);
`else
        doFinish(resp.dst, resp.tag, resp.res.data, resp.res.fflags);
`endif
    endrule

    rule doFinishFpFma;
        FpuResp resp <- fpuExec.fmaResp;
        if(verbose) $display("[doFinishFpFma] ", fshow(resp));
`ifdef KONATA
        doFinish(resp.dst, resp.tag, resp.res.data, resp.res.fflags, resp.u_id);
`else
        doFinish(resp.dst, resp.tag, resp.res.data, resp.res.fflags);
`endif
`ifdef PERF_COUNT
        if(inIfc.doStats) begin
            exeFpFmaCnt.incr(1);
        end
`endif
    endrule

    rule doFinishFpDiv;
        FpuResp resp <- fpuExec.divResp;
        if(verbose) $display("[doFinishFpDiv] ", fshow(resp));
`ifdef KONATA
        doFinish(resp.dst, resp.tag, resp.res.data, resp.res.fflags, resp.u_id);
`else
        doFinish(resp.dst, resp.tag, resp.res.data, resp.res.fflags);
`endif
`ifdef PERF_COUNT
        if(inIfc.doStats) begin
            exeFpDivCnt.incr(1);
        end
`endif
    endrule

    rule doFinishFpSqrt;
        FpuResp resp <- fpuExec.sqrtResp;
        if(verbose) $display("[doFinishFpSqrt] ", fshow(resp));
`ifdef KONATA
        doFinish(resp.dst, resp.tag, resp.res.data, resp.res.fflags, resp.u_id);
`else
        doFinish(resp.dst, resp.tag, resp.res.data, resp.res.fflags);
`endif
`ifdef PERF_COUNT
        if(inIfc.doStats) begin
            exeFpSqrtCnt.incr(1);
        end
`endif
    endrule

    rule doFinishIntMul;
        MulDivResp resp <- mulDivExec.mulResp;
        if(verbose) $display("[doFinishIntMul] ", fshow(resp));
`ifdef KONATA
        doFinish(resp.dst, resp.tag, resp.data, 0, resp.u_id);
`else
        doFinish(resp.dst, resp.tag, resp.data, 0);
`endif
`ifdef PERF_COUNT
        if(inIfc.doStats) begin
            exeIntMulCnt.incr(1);
        end
`endif
    endrule

    rule doFinishIntDiv;
        MulDivResp resp <- mulDivExec.divResp;
        if(verbose) $display("[doFinishIntDiv] ", fshow(resp));
`ifdef KONATA
        doFinish(resp.dst, resp.tag, resp.data, 0, resp.u_id);
`else
        doFinish(resp.dst, resp.tag, resp.data, 0);
`endif
`ifdef PERF_COUNT
        if(inIfc.doStats) begin
            exeIntDivCnt.incr(1);
        end
`endif
    endrule

    interface recvBypass = map(getRecvBypassDataIfc, bypassWire);

    interface rsFpuMulDivIfc = rsFpuMulDiv;

    interface specUpdate = joinSpeculationUpdate(vec(
        rsFpuMulDiv.specUpdate,
        dispToRegQ.specUpdate,
        regToExeQ.specUpdate,
        fpuExec.specUpdate,
        mulDivExec.specUpdate
    ));

    method Data getPerf(ExeStagePerfType t);
        return (case(t)
`ifdef PERF_COUNT
            ExeIntMulCnt: exeIntMulCnt;
            ExeIntDivCnt: exeIntDivCnt;
            ExeFpFmaCnt: exeFpFmaCnt;
            ExeFpDivCnt: exeFpDivCnt;
            ExeFpSqrtCnt: exeFpSqrtCnt;
`endif
            default: 0;
        endcase);
    endmethod
endmodule
