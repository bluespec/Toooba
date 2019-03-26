
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
import Vector::*;
import FIFO::*;
import GetPut::*;
import BuildVector::*;
import Cntrs::*;
import Types::*;
import ProcTypes::*;
import SynthParam::*;
import Exec::*;
import Performance::*;
import BrPred::*;
import DirPredictor::*;
import ReservationStationEhr::*;
import ReservationStationAlu::*;
import ReorderBuffer::*;
import SpecFifo::*;
import HasSpecBits::*;
import Bypass::*;

// ALU pipeline has 4 stages
// dispatch -> reg read -> exe -> finish (write reg)
// bypass is sent out from the end of exe stage
// and reg read stage will recv bypass

typedef struct {
    // inst info
    DecodedInst dInst;
    PhyRegs regs;
    InstTag tag;
    DirPredTrainInfo dpTrain;
    // specualtion
    Maybe#(SpecTag) spec_tag;
} AluDispatchToRegRead deriving(Bits, Eq, FShow);

typedef struct {
    // inst info
    DecodedInst dInst;
    Maybe#(PhyDst) dst;
    InstTag tag;
    DirPredTrainInfo dpTrain;
    // src reg vals & pc & ppc
    Data rVal1;
    Data rVal2;
    Addr pc;
    Addr ppc;
    // specualtion
    Maybe#(SpecTag) spec_tag;
} AluRegReadToExe deriving(Bits, Eq, FShow);

typedef struct {
    // inst info
    IType iType;
    Maybe#(PhyDst) dst;
    InstTag tag;
    DirPredTrainInfo dpTrain;
    // result
    Data data; // alu compute result
    Maybe#(Data) csrData; // data to write CSR file
    ControlFlow controlFlow;
    // speculation
    Maybe#(SpecTag) spec_tag;
} AluExeToFinish deriving(Bits, Eq, FShow);

// XXX currently ALU/Br should not have any exception, so we don't have cause feild above
// TODO FIXME In future, if branch target is unaligned to 4 bytes, we may have exception
// and probably JR/JAL should NOT write dst reg when exception happens
// However, currently JR/JAL renaming is included in the previous checkpoint
// so we need to explicitly check exception and don't write reg

// synthesized pipeline fifos
typedef SpecFifo_SB_deq_enq_C_deq_enq#(1, AluDispatchToRegRead) AluDispToRegFifo;
(* synthesize *)
module mkAluDispToRegFifo(AluDispToRegFifo);
    let m <- mkSpecFifo_SB_deq_enq_C_deq_enq(False);
    return m;
endmodule

// this one must be 1 elem FIFO, otherwise we cannot get correct bypass
typedef SpecFifo_SB_deq_enq_C_deq_enq#(1, AluRegReadToExe) AluRegToExeFifo;
(* synthesize *)
module mkAluRegToExeFifo(AluRegToExeFifo);
    let m <- mkSpecFifo_SB_deq_enq_C_deq_enq(False);
    return m;
endmodule

// this one must be 1 elem FIFO, otherwise we cannot get correct bypass
typedef SpecFifo_SB_deq_enq_SB_deq_wrong_C_enq#(1, AluExeToFinish) AluExeToFinFifo;
(* synthesize *)
module mkAluExeToFinFifo(AluExeToFinFifo);
    let m <- mkSpecFifo_SB_deq_enq_SB_deq_wrong_C_enq(False);
    return m;
endmodule

typedef struct {
    Addr pc;
    Addr nextPc;
    IType iType;
    Bool taken;
    DirPredTrainInfo dpTrain;
    Bool mispred;
} FetchTrainBP deriving(Bits, Eq, FShow);

interface AluExeInput;
    // conservative scoreboard check in reg read stage
    method RegsReady sbCons_lazyLookup(PhyRegs r);
    // Phys reg file
    method Data rf_rd1(PhyRIndx rindx);
    method Data rf_rd2(PhyRIndx rindx);
    // CSR file
    method Data csrf_rd(CSR csr);
    // ROB
    method Addr rob_getPC(InstTag t);
    method Addr rob_getPredPC(InstTag t);
    method Action rob_setExecuted(InstTag t, Maybe#(Data) csrData, ControlFlow cf);
    // Fetch stage
    method Action fetch_train_predictors(FetchTrainBP train);

    // global broadcast methods
    // set aggressive sb & wake up inst in RS
    method Action setRegReadyAggr(PhyRIndx dst);
    // send bypass from exe and finish stage
    interface Vector#(2, SendBypass) sendBypass;
    // write reg file & set conservative sb
    method Action writeRegFile(PhyRIndx dst, Data data);
    // redirect
    method Action redirect(Addr new_pc, SpecTag spec_tag, InstTag inst_tag);
    // spec update
    method Action correctSpec(SpecTag t);

    // performance
    method Bool doStats;
endinterface

interface AluExePipeline;
    // recv bypass from exe and finish stages of each ALU pipeline
    interface Vector#(TMul#(2, AluExeNum), RecvBypass) recvBypass;
    interface ReservationStationAlu rsAluIfc;
    interface SpeculationUpdate specUpdate;
    method Data getPerf(ExeStagePerfType t);
endinterface

module mkAluExePipeline#(AluExeInput inIfc)(AluExePipeline);
    Bool verbose = True;

    // alu reservation station
    ReservationStationAlu rsAlu <- mkReservationStationAlu;
    // pipeline fifos
    let dispToRegQ <- mkAluDispToRegFifo;
    let regToExeQ <- mkAluRegToExeFifo;
    let exeToFinQ <- mkAluExeToFinFifo;
    // wire to recv bypass
    Vector#(TMul#(2, AluExeNum), RWire#(Tuple2#(PhyRIndx, Data))) bypassWire <- replicateM(mkRWire);
    // index to send bypass, ordering doesn't matter
    Integer exeSendBypassPort = 0;
    Integer finishSendBypassPort = 1;

`ifdef PERF_COUNT
    // performance counters
    Count#(Data) exeRedirectBrCnt <- mkCount(0);
    Count#(Data) exeRedirectJrCnt <- mkCount(0);
    Count#(Data) exeRedirectOtherCnt <- mkCount(0);
`endif

    rule doDispatchAlu;
        rsAlu.doDispatch;
        let x = rsAlu.dispatchData;
        if(verbose) $display("[doDispatchAlu] ", fshow(x));

        // set reg ready aggressively
        if(x.regs.dst matches tagged Valid .dst) begin
            inIfc.setRegReadyAggr(dst.indx);
        end
        
        // go to next stage
        dispToRegQ.enq(ToSpecFifo {
            data: AluDispatchToRegRead {
                dInst: x.data.dInst,
                regs: x.regs,
                tag: x.tag,
                dpTrain: x.data.dpTrain,
                spec_tag: x.spec_tag
            },
            spec_bits: x.spec_bits
        });
    endrule

    rule doRegReadAlu;
        dispToRegQ.deq;
        let dispToReg = dispToRegQ.first;
        let x = dispToReg.data;
        if(verbose) $display("[doRegReadAlu] ", fshow(dispToReg));

        // check conservative scoreboard
        let regsReady = inIfc.sbCons_lazyLookup(x.regs);

        // get rVal1 (check bypass)
        Data rVal1 = ?;
        if(x.dInst.csr matches tagged Valid .csr) begin
            rVal1 = inIfc.csrf_rd(csr);
        end
        else if(x.regs.src1 matches tagged Valid .src1) begin
            rVal1 <- readRFBypass(src1, regsReady.src1, inIfc.rf_rd1(src1), bypassWire);
        end

        // get rVal2 (check bypass)
        Data rVal2 = ?;
        if(x.regs.src2 matches tagged Valid .src2) begin
            rVal2 <- readRFBypass(src2, regsReady.src2, inIfc.rf_rd2(src2), bypassWire);
        end

        // get PC and PPC
        let pc = inIfc.rob_getPC(x.tag);
        let ppc = inIfc.rob_getPredPC(x.tag);

        // go to next stage
        regToExeQ.enq(ToSpecFifo {
            data: AluRegReadToExe {
                dInst: x.dInst,
                dst: x.regs.dst,
                tag: x.tag,
                dpTrain: x.dpTrain,
                rVal1: rVal1,
                rVal2: rVal2,
                pc: pc,
                ppc: ppc,
                spec_tag: x.spec_tag
            },
            spec_bits: dispToReg.spec_bits
        });
    endrule

    rule doExeAlu;
        regToExeQ.deq;
        let regToExe = regToExeQ.first;
        let x = regToExe.data;
        if(verbose) $display("[doExeAlu] ", fshow(regToExe));

        // execution
        ExecResult exec_result = basicExec(x.dInst, x.rVal1, x.rVal2, x.pc, x.ppc);

        // when inst needs to store csrData in ROB, it must have iType = Csr, cannot mispredict
        if(isValid(x.dInst.csr)) begin
            doAssert(x.dInst.iType == Csr, "Only Csr inst needs to update csrData in ROB");
            doAssert(!exec_result.controlFlow.mispredict, "Csr inst cannot mispredict");
            doAssert(exec_result.controlFlow.nextPc == x.ppc && x.ppc == x.pc + 4, "Csr inst ppc = pc+4");
        end

        // send bypass
        if(x.dst matches tagged Valid .dst) begin
            inIfc.sendBypass[exeSendBypassPort].send(dst.indx, exec_result.data);
        end

        // go to next stage
        exeToFinQ.enq(ToSpecFifo {
            data: AluExeToFinish {
                iType: x.dInst.iType,
                dst: x.dst,
                tag: x.tag,
                dpTrain: x.dpTrain,
                data: exec_result.data,
                csrData: isValid(x.dInst.csr) ? Valid (exec_result.csrData) : Invalid,
                controlFlow: exec_result.controlFlow,
                spec_tag: x.spec_tag
            },
            spec_bits: regToExe.spec_bits
        });
    endrule

    rule doFinishAlu;
        exeToFinQ.deq;
        let exeToFin = exeToFinQ.first;
        let x = exeToFin.data;
        if(verbose) $display("[doFinishAlu] ", fshow(exeToFin));

        // send bypass & write reg file
        if(x.dst matches tagged Valid .dst) begin
            inIfc.sendBypass[finishSendBypassPort].send(dst.indx, x.data);
            inIfc.writeRegFile(dst.indx, x.data);
        end

        // update the instruction in the reorder buffer.
        inIfc.rob_setExecuted(
            x.tag,
            x.csrData,
            x.controlFlow
        );

        // handle spec tags for branch predictions
        (* split *)
        if (x.controlFlow.mispredict) (* nosplit *) begin
            // wrong branch predictin, we must have spec tag
            doAssert(isValid(x.spec_tag), "mispredicted branch must have spec tag");
            inIfc.redirect(x.controlFlow.nextPc, validValue(x.spec_tag), x.tag);
            // must be a branch, train branch predictor
            doAssert(x.iType == Jr || x.iType == Br, "only jr and br can mispredict");
            inIfc.fetch_train_predictors(FetchTrainBP {
                pc: x.controlFlow.pc,
                nextPc: x.controlFlow.nextPc,
                iType: x.iType,
                taken: x.controlFlow.taken,
                dpTrain: x.dpTrain,
                mispred: True
            });
`ifdef PERF_COUNT
            // performance counter
            if(inIfc.doStats) begin
                case(x.iType)
                    Br: exeRedirectBrCnt.incr(1);
                    Jr: exeRedirectJrCnt.incr(1);
                    default: exeRedirectOtherCnt.incr(1);
                endcase
            end
`endif
        end
        else (* nosplit *) begin
            // release spec tag
            if (x.spec_tag matches tagged Valid .valid_spec_tag) begin
                inIfc.correctSpec(valid_spec_tag);
            end
            // train branch predictor if needed 
            // since we can only do 1 training in a cycle, split the rule
            // XXX not training JAL, reduce chance of conflicts
            if(x.iType == Jr || x.iType == Br) begin
                inIfc.fetch_train_predictors(FetchTrainBP {
                    pc: x.controlFlow.pc,
                    nextPc: x.controlFlow.nextPc,
                    iType: x.iType,
                    taken: x.controlFlow.taken,
                    dpTrain: x.dpTrain,
                    mispred: False
                });
            end
        end
    endrule

    interface recvBypass = map(getRecvBypassIfc, bypassWire);

    interface rsAluIfc = rsAlu;

    interface specUpdate = joinSpeculationUpdate(vec(
        rsAlu.specUpdate,
        dispToRegQ.specUpdate,
        regToExeQ.specUpdate,
        exeToFinQ.specUpdate
    ));

    method Data getPerf(ExeStagePerfType t);
        return (case(t)
`ifdef PERF_COUNT
            ExeRedirectBr: exeRedirectBrCnt;
            ExeRedirectJr: exeRedirectJrCnt;
            ExeRedirectOther: exeRedirectOtherCnt;
`endif
            default: 0;
        endcase);
    endmethod
endmodule

