
// Copyright (c) 2017 Massachusetts Institute of Technology
//
//-
// RVFI_DII + CHERI modifications:
//     Copyright (c) 2020 Jessica Clarke
//     Copyright (c) 2020 Peter Rugg
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
import CHERICap::*;
import CHERICC_Fat::*;
import ISA_Decls_CHERI::*;
`ifdef PERFORMANCE_MONITORING
import BlueUtils::*;
import StatCounters::*;
import DReg::*;
`endif


import Cur_Cycle :: *;

// ALU pipeline has 4 stages
// dispatch -> reg read -> exe -> finish (write reg)
// bypass is sent out from the end of exe stage
// and reg read stage will recv bypass

typedef struct {
    // inst info
    DecodedInst dInst;
    PhyRegs regs;
    InstTag tag;
    PredTrainInfo trainInfo;
    // specualtion
    Maybe#(SpecTag) spec_tag;
`ifdef KONATA
    Bit#(64) u_id;
`endif
} AluDispatchToRegRead deriving(Bits, Eq, FShow);

typedef struct {
    // inst info
    DecodedInst dInst;
    Maybe#(PhyDst) dst;
    InstTag tag;
    PredTrainInfo trainInfo;
    // src reg vals & pc & ppc
    CapPipe rVal1;
    CapPipe rVal2;
    CapMem pc;
    CapMem ppc;
    Bit #(32) orig_inst;
    // specualtion
    Maybe#(SpecTag) spec_tag;
`ifdef KONATA
    Bit#(64) u_id;
`endif
} AluRegReadToExe deriving(Bits, FShow);

typedef struct {
    // inst info
    IType iType;
    Maybe#(PhyDst) dst;
    InstTag tag;
    PredTrainInfo trainInfo;
    Bool link;
    Bool isCompressed;
    // result
    CapPipe data; // alu compute result
    PPCVAddrCSRData csrData; // data to write CSR file, or predicted next PC if not. (For reorder buffer)
    ControlFlow controlFlow;
    Maybe#(CSR_XCapCause) capException;
    Maybe#(BoundsCheck) check;
    // speculation
    Maybe#(SpecTag) spec_tag;
`ifdef RVFI
    ExtraTraceBundle   traceBundle;
`endif
`ifdef KONATA
    Bit#(64) u_id;
`endif
} AluExeToFinish deriving(Bits, FShow);

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
    CapMem pc;
    CapMem nextPc;
    Bool link;
    IType iType;
    Bool taken;
    PredTrainInfo trainInfo;
    Bool mispred;
    Bool isCompressed;
} FetchTrainBP deriving(Bits, Eq, FShow);

interface AluExeInput;
    // conservative scoreboard check in reg read stage
    method RegsReady sbCons_lazyLookup(PhyRegs r);
    // Phys reg file
    method CapPipe rf_rd1(PhyRIndx rindx);
    method CapPipe rf_rd2(PhyRIndx rindx);
    // CSR file
    method Data csrf_rd(CSR csr);
    // Special Capability Register file.
    method CapReg scaprf_rd(SCR csr);
    // ROB
    method CapMem rob_getPC(InstTag t);
    method CapMem rob_getPredPC(InstTag t);
    method Bit #(32) rob_getOrig_Inst (InstTag t);
    method Action rob_setExecuted(
        InstTag t,
`ifdef INCLUDE_TANDEM_VERIF
        CapPipe dst_data,
`endif
        PPCVAddrCSRData csrData,
        Maybe#(CSR_XCapCause) capCause
`ifdef RVFI
        , ExtraTraceBundle tb
`endif
    );
    // Fetch stage
    method Action fetch_train_predictors(ToSpecFifo#(FetchTrainBP) train);

    // global broadcast methods
    // set aggressive sb & wake up inst in RS
    method Action setRegReadyAggr(PhyRIndx dst);
    // send bypass from exe and finish stage
    interface Vector#(2, SendBypass) sendBypass;
    // write reg file & set conservative sb
    method Action writeRegFile(PhyRIndx dst, CapPipe data);
    // redirect
    method Action redirect(CapMem new_pc, SpecTag spec_tag, InstTag inst_tag, SpecBits spec_bits);
    // pending invalidation could pause execute/redirections.
    method Bool pauseExecute;
    // spec update
    method Action correctSpec(SpecTag t);

    // performance
    method Bool doStats;

`ifdef PERFORMANCE_MONITORING
`ifdef CONTRACTS_VERIFY
    // check previous branch targets
    method Bool checkTarget(CapMem ppc);
    // check (previous) return targets
    method Bool checkReturnTarget(CapMem ppc);
`endif
`endif
endinterface

interface AluExePipeline;
    // recv bypass from exe and finish stages of each ALU pipeline
    interface Vector#(TMul#(2, AluExeNum), RecvBypass) recvBypass;
    interface ReservationStationAlu rsAluIfc;
    interface SpeculationUpdate specUpdate;
    method Data getPerf(ExeStagePerfType t);

`ifdef PERFORMANCE_MONITORING
`ifdef CONTRACTS_VERIFY
    method EventsTransExe events;
`endif
`endif
endinterface

module mkAluExePipeline#(AluExeInput inIfc)(AluExePipeline);
    Bool verbose = False;
    Integer verbosity = 0;

    // alu reservation station
    ReservationStationAlu rsAlu <- mkReservationStationAlu;
    // pipeline fifos
    let dispToRegQ <- mkAluDispToRegFifo;
    let regToExeQ <- mkAluRegToExeFifo;
    let exeToFinQ <- mkAluExeToFinFifo;
    // wire to recv bypass
    Vector#(TMul#(2, AluExeNum), RWire#(Tuple2#(PhyRIndx, CapPipe))) bypassWire <- replicateM(mkRWire);
    // index to send bypass, ordering doesn't matter
    Integer exeSendBypassPort = 0;
    Integer finishSendBypassPort = 1;

`ifdef PERFORMANCE_MONITORING
`ifdef CONTRACTS_VERIFY
    Array#(Reg#(EventsTransExe)) events_reg <- mkDRegOR(2, unpack(0));
`endif
`endif

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
`ifdef KONATA 
        $display("KONATAE\t%0d\t%0d\t0\tRsvA", cur_cycle, x.u_id);
        $display("KONATAS\t%0d\t%0d\t0\tAlu1", cur_cycle, x.u_id);
        $fflush;
`endif

        // go to next stage
        dispToRegQ.enq(ToSpecFifo {
            data: AluDispatchToRegRead {
                dInst: x.data.dInst,
                regs: x.regs,
                tag: x.tag,
                trainInfo: x.data.trainInfo,
                spec_tag: x.spec_tag
`ifdef KONATA
                , u_id: x.u_id
`endif
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
        CapPipe rVal1 = nullCap;
        if(x.dInst.csr matches tagged Valid .csr) begin
            rVal1 = nullWithAddr(inIfc.csrf_rd(csr));
        end
        else if(x.regs.src1 matches tagged Valid .src1 &&& src1 != 0) begin
            rVal1 <- readRFBypass(src1, regsReady.src1, inIfc.rf_rd1(src1), bypassWire);
        end

        // get rVal2 (check bypass)
        CapPipe rVal2 = nullCap;
        if(x.dInst.scr matches tagged Valid .scr) begin
            rVal2 = cast(inIfc.scaprf_rd(scr));
        end
        if(x.regs.src2 matches tagged Valid .src2 &&& src2 != 0) begin
            rVal2 <- readRFBypass(src2, regsReady.src2, inIfc.rf_rd2(src2), bypassWire);
        end

        // get PC and PPC
        let pc = inIfc.rob_getPC(x.tag);
        let ppc = inIfc.rob_getPredPC(x.tag);
        let orig_inst = inIfc.rob_getOrig_Inst (x.tag);

`ifdef PERFORMANCE_MONITORING
`ifdef CONTRACTS_VERIFY
        let ppc_addr = getAddr(ppc);
        let pc_addr = getAddr(pc);
        EventsTransExe events = unpack(0);
        if(x.dInst.iType == Br || x.dInst.iType == CJAL || x.dInst.iType == J) begin
            Bit#(32) imm = fromMaybe(32'h00000000, x.dInst.imm);
            let val = pc_addr + signExtend(imm);
            if((val != ppc_addr) && (ppc_addr != pc_addr + 2) && (ppc_addr != pc_addr + 4)) begin
                events.evt_WILD_JUMP = 1;
                events_reg[1] <= events;
            end
        end



        else if(x.dInst.iType == CJALR || x.dInst.iType == Jr) begin
            let res_targets = inIfc.checkTarget(ppc);
            let res_ret_targets = inIfc.checkReturnTarget(ppc);
            if(!res_targets && !res_ret_targets && (ppc_addr != pc_addr + 2) && (ppc_addr != pc_addr + 4)) begin
                events.evt_WILD_JUMP = 1;
                events_reg[1] <= events;
            end
        end
`endif
`endif

`ifdef KONATA 
        $display("KONATAE\t%0d\t%0d\t0\tAlu1", cur_cycle, x.u_id);
        $display("KONATAS\t%0d\t%0d\t0\tAlu2", cur_cycle, x.u_id);
        $fflush;
`endif

        // go to next stage
        regToExeQ.enq(ToSpecFifo {
            data: AluRegReadToExe {
                dInst: x.dInst,
                dst: x.regs.dst,
                tag: x.tag,
                trainInfo: x.trainInfo,
                rVal1: rVal1,
                rVal2: rVal2,
                pc: pc,
                ppc: ppc,
                orig_inst: orig_inst,
                spec_tag: x.spec_tag
`ifdef KONATA
                , u_id: x.u_id
`endif
            },
            spec_bits: dispToReg.spec_bits
        });
    endrule

    rule doExeAlu(!inIfc.pauseExecute);
        regToExeQ.deq;
        let regToExe = regToExeQ.first;
        let x = regToExe.data;
        if(verbose) $display("[doExeAlu] ", fshow(regToExe));
        // execution
        ExecResult exec_result = basicExec(x.dInst, x.rVal1, x.rVal2, cast(x.pc), cast(x.ppc), x.orig_inst);

        Bool link = (case (x.dInst.iType)
                J, CJAL, CJALR, Jr: isValid(x.dst);
                default: False;
            endcase);
`ifdef RAS_HIT_TRACING
        if (linkedR(Valid(tagged Gpr x.orig_inst[19:15])) && (x.orig_inst[19:15] != x.orig_inst[11:7])) begin
            case (x.dInst.iType)
                Jr, CJALR: $display("Jr/CJALR ra: PC: %x Mispredict: %x , %x vs %x src1: %d", getAddr(x.pc), exec_result.controlFlow.mispredict, getAddr(exec_result.controlFlow.nextPc), getAddr(x.ppc), x.orig_inst[19:15]);
            endcase
        end
`endif

        if (verbosity > 0) begin
           $display ("AluExePipeline.doExeAlu: regToExe    = ", fshow (regToExe));
           $display ("AluExePipeline.doExeAlu: exec_result = ", fshow (exec_result));
           CapMem cm_npc = cast(exec_result.controlFlow.nextPc);
           $display ("CapMem eq: %d, nextPc: %x, predPc: %x", cm_npc==x.ppc, cm_npc, x.ppc);
        end

        // when inst needs to store csrData in ROB, it must have iType = Csr, cannot mispredict
        if(isValid(x.dInst.csr)) begin
            doAssert(x.dInst.iType == Csr, "Only Csr inst needs to update csrData in ROB");
            doAssert(!exec_result.controlFlow.mispredict, "Csr inst cannot mispredict");
            doAssert(cast(exec_result.controlFlow.nextPc) == x.ppc && x.ppc == addPc(x.pc, 4), "Csr inst ppc = pc+4");
        end
        // when inst needs to store scrData in ROB, it must have iType = Scr, cannot mispredict
        if(isValid(x.dInst.scr)) begin
            // doAssert(x.dInst.iType == Scr, "Only Scr inst needs to update scrData in ROB"); // Removed because normal instructions can read SCRs
            doAssert(!exec_result.controlFlow.mispredict, "Scr inst cannot mispredict");
            doAssert(cast(exec_result.controlFlow.nextPc) == x.ppc && x.ppc == addPc(x.pc, 4), "Scr inst ppc = pc+4");
        end
`ifdef MELTDOWN_CF_MITIGATION
        // This condition can eliminate Meltdown-CF vulnerabilities for exception conditions that don't need the bounds check result.
        if (isValid(exec_result.capException)) exec_result.data = setValidCap(exec_result.data, False);
`endif
        // send bypass
        if(x.dst matches tagged Valid .dst) begin
            inIfc.sendBypass[exeSendBypassPort].send(dst.indx, exec_result.data);
        end

        Bool is_scr_or_csr = (isValid(x.dInst.scr) && x.dInst.iType == Scr) || isValid(x.dInst.csr);

`ifdef KONATA 
        $display("KONATAE\t%0d\t%0d\t0\tAlu2", cur_cycle, x.u_id);
        $display("KONATAS\t%0d\t%0d\t0\tAlu3", cur_cycle, x.u_id);
        $fflush;
`endif
        // go to next stage
        exeToFinQ.enq(ToSpecFifo {
            data: AluExeToFinish {
                iType: x.dInst.iType,
                dst: x.dst,
                tag: x.tag,
                trainInfo: x.trainInfo,
                link: link,
                isCompressed: x.orig_inst[1:0] != 2'b11,
                data: exec_result.data,
                csrData: is_scr_or_csr ? CSRData (exec_result.csrData) : PPC (cast(exec_result.controlFlow.nextPc)),
                capException: exec_result.capException,
                check: exec_result.boundsCheck,
`ifdef RVFI
                traceBundle: ExtraTraceBundle{
                    regWriteData: getAddr(exec_result.data),
                    memByteEn: replicate(False)
                },
`endif
`ifdef KONATA
                u_id: x.u_id,
`endif
                controlFlow: exec_result.controlFlow,
                spec_tag: x.spec_tag
            },
            spec_bits: regToExe.spec_bits
        });
    endrule

    rule doFinishAlu(!inIfc.pauseExecute);
        exeToFinQ.deq;
        let exeToFin = exeToFinQ.first;
        let x = exeToFin.data;
        if(verbose) $display("[doFinishAlu] ", fshow(exeToFin));

        // send bypass & write reg file
        if(x.dst matches tagged Valid .dst) begin
            inIfc.sendBypass[finishSendBypassPort].send(dst.indx, x.data);
            inIfc.writeRegFile(dst.indx, x.data);
        end

        if (x.check matches tagged Valid .check &&& x.capException matches tagged Invalid) begin
            if (!(                         (check.check_low  >= check.authority_base) &&
                  (check.check_inclusive ? (check.check_high <= check.authority_top )
                                         : (check.check_high <  check.authority_top ))))
                x.capException = Valid(CSR_XCapCause{cheri_exc_reg: check.authority_idx, cheri_exc_code: cheriExcLengthViolation});
        end

        // update the instruction in the reorder buffer.
        inIfc.rob_setExecuted(
            x.tag,
`ifdef INCLUDE_TANDEM_VERIF
            x.data,
`endif
            x.csrData,
            x.capException
`ifdef RVFI
            , x.traceBundle
`endif
        );

`ifdef KONATA 
        $display("KONATAE\t%0d\t%0d\t0\tAlu3\t%0d", cur_cycle, x.u_id, cur_cycle);
        $display("KONATAS\t%0d\t%0d\t0\tAlu4\t%0d", cur_cycle, x.u_id, cur_cycle);
        $fflush;
`endif
`ifdef PERFORMANCE_MONITORING
`ifdef CONTRACTS_VERIFY
        // get PC and PPC
        let pc = getAddr(x.controlFlow.pc);
        let ppc = getAddr(x.controlFlow.nextPc);
        let validPc = x.isCompressed ? (pc + 2) : (pc + 4);
        if(x.capException matches tagged Valid .exc &&& (ppc != validPc)) begin
            EventsTransExe events = unpack(0);
            events.evt_WILD_EXCEPTION = 1;
            events_reg[0] <= events;
        end
`endif
`endif

        let train_spec_bits = 0;
`ifdef NO_SPEC_TRAINING
        train_spec_bits = exeToFin.spec_bits;
`endif

        // handle spec tags for branch predictions
        // TODO what happens here if we trap?
        (* split *)
        if (x.controlFlow.mispredict) (* nosplit *) begin
            // wrong branch predictin, we must have spec tag
            doAssert(isValid(x.spec_tag), "mispredicted branch must have spec tag");
            inIfc.redirect(cast(x.controlFlow.nextPc), validValue(x.spec_tag), x.tag, exeToFin.spec_bits);
            // must be a branch, train branch predictor
            doAssert(x.iType == Jr || x.iType == CJALR || x.iType == CCall || x.iType == Br, "only jr, br, cjalr, and ccall can mispredict");
            inIfc.fetch_train_predictors(ToSpecFifo {
                data: FetchTrainBP {
                    pc: cast(x.controlFlow.pc),
                    nextPc: cast(x.controlFlow.nextPc),
                    link: x.link,
                    iType: x.iType,
                    taken: x.controlFlow.taken,
                    trainInfo: x.trainInfo,
                    mispred: True,
                    isCompressed: x.isCompressed},
                spec_bits: train_spec_bits
            });
            if (verbose)
                $display("alu mispredict pc: %x, nextPc: %x, %d",
                         x.controlFlow.pc, x.controlFlow.nextPc, cur_cycle);
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
            if(x.iType == Jr || x.iType == CJALR || x.iType == CCall || x.iType == Br) begin
                inIfc.fetch_train_predictors(ToSpecFifo {
                    data: FetchTrainBP {
                        pc: cast(x.controlFlow.pc),
                        nextPc: cast(x.controlFlow.nextPc),
                        link: x.link,
                        iType: x.iType,
                        taken: x.controlFlow.taken,
                        trainInfo: x.trainInfo,
                        mispred: False,
                        isCompressed: x.isCompressed},
                    spec_bits: train_spec_bits
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

`ifdef PERFORMANCE_MONITORING
`ifdef CONTRACTS_VERIFY
    method events = events_reg[0];
`endif
`endif

endmodule
