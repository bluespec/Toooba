
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

`include "ProcConfig.bsv"

import Vector::*;
import BuildVector::*;
import DefaultValue::*;
import ClientServer::*;
import GetPut::*;
import Assert::*;
import Cntrs::*;
import ConfigReg::*;
import FIFO::*;
import Fifos::*;
import Ehr::*;
import Connectable::*;

import Types::*;
import ProcTypes::*;
import CacheUtils::*;
import TlbTypes::*;
import SynthParam::*;
import VerificationPacket::*;
import Performance::*;
`ifdef PERFORMANCE_MONITORING
import PerformanceMonitor::*;
import SpecialRegs::*;
`endif
import HasSpecBits::*;
import Exec::*;
import FetchStage::*;
import ITlb::*;
import DTlb::*;
import L2Tlb::*;
import TlbConnect::*;
import EpochManager::*;
import PhysRFile::*;
import RFileSynth::*;
import RenamingTable::*;
import ReorderBuffer::*;
import ReorderBufferSynth::*;
import Scoreboard::*;
import ScoreboardSynth::*;
import SpecTagManager::*;
import Fpu::*;
import MulDiv::*;
import ReservationStationEhr::*;
import ReservationStationAlu::*;
import ReservationStationMem::*;
import ReservationStationFpuMulDiv::*;
import AluExePipeline::*;
import FpuMulDivExePipeline::*;
import MemExePipeline::*;
import SplitLSQ::*;
import StoreBuffer::*;
import GlobalSpecUpdate::*;
import CCTypes::*;
import L1CoCache::*;
import L1Bank::*;
import IBank::*;
import MMIOCore::*;
import RenameStage::*;
import CommitStage::*;
import Bypass::*;

import CsrFile :: *;

// ================================================================
// Toooba

import Cur_Cycle  :: *;
import FIFOF      :: *;
import GetPut_Aux :: *;

`ifdef INCLUDE_GDB_CONTROL
import DM_CPU_Req_Rsp  :: *;
`endif

`ifdef INCLUDE_TANDEM_VERIF
import Trace_Data2 :: *;
`endif

// ================================================================

`ifdef SECURITY
`define SECURITY_OR_INCLUDE_GDB_CONTROL
`elsif INCLUDE_GDB_CONTROL
`define SECURITY_OR_INCLUDE_GDB_CONTROL
`endif

interface CoreReq;
    method Action start(
        Bool running,
        Addr startpc,
        Addr toHostAddr, Addr fromHostAddr
    );
    method Action perfReq(PerfLocation loc, PerfType t);
endinterface

interface CoreIndInv;
    method ActionValue#(ProcPerfResp) perfResp;
    method ActionValue#(void) terminate;
endinterface

interface CoreDeadlock;
    interface Get#(L1DCRqStuck) dCacheCRqStuck;
    interface Get#(L1DPRqStuck) dCachePRqStuck;
    interface Get#(L1ICRqStuck) iCacheCRqStuck;
    interface Get#(L1IPRqStuck) iCachePRqStuck;
    interface Get#(RenameStuck) renameInstStuck;
    interface Get#(RenameStuck) renameCorrectPathStuck;
    interface Get#(CommitStuck) commitInstStuck;
    interface Get#(CommitStuck) commitUserInstStuck;
    interface Get#(void) checkStarted;
endinterface

interface CoreRenameDebug;
    interface Get#(RenameErrInfo) renameErr;
endinterface

interface Core;
    // core request & indication
    interface CoreReq coreReq;
    interface CoreIndInv coreIndInv;
    // coherent caches to LLC
    interface ChildCacheToParent#(L1Way, void) dCacheToParent;
    interface ChildCacheToParent#(L1Way, void) iCacheToParent;
    // DMA to LLC
    interface TlbMemClient tlbToMem;
    // MMIO
    interface MMIOCoreToPlatform mmioToPlatform;
    // stats enable
    method ActionValue#(Bool) sendDoStats;
    method Action recvDoStats(Bool x);
    // detect deadlock: only in use when macro CHECK_DEADLOCK is defined
    interface CoreDeadlock deadlock;
    // debug rename
    interface CoreRenameDebug renameDebug;

   // Bluespec: external interrupt requests targeting Machine and Supervisor modes
    method Action setMEIP (Bit #(1) v);
    method Action setSEIP (Bit #(1) v);

`ifdef INCLUDE_GDB_CONTROL
   interface Server #(Bool, Bool)                             hart_run_halt_server;
   interface Server #(DM_CPU_Req #(5, 64),  DM_CPU_Rsp #(64)) hart_gpr_mem_server;
`ifdef ISA_F
   interface Server #(DM_CPU_Req #(5, 64),  DM_CPU_Rsp #(64)) hart_fpr_mem_server;
`endif
   interface Server #(DM_CPU_Req #(12, 64), DM_CPU_Rsp #(64)) hart_csr_mem_server;
`endif

`ifdef INCLUDE_TANDEM_VERIF
   // Note: this is a SupSize vector of streams of Trace_Data2 structs,
   // each of which has a serialnum field.  Each of the SupSize
   // streams has serialnums in increasing order.  Each serialnum
   // appears exactly once in exactly one of the streams. Thus, the
   // channels can easily be merged into a single program-order stream.
   interface Vector #(SupSize, Get #(Trace_Data2)) v_to_TV;
`endif

`ifdef PERFORMANCE_MONITORING
    method Action events_llc(EventsCache events);
`endif
endinterface

// fixpoint to instantiate modules
interface CoreFixPoint;
    interface Vector#(AluExeNum, AluExePipeline) aluExeIfc;
    interface Vector#(FpuMulDivExeNum, FpuMulDivExePipeline) fpuMulDivExeIfc;
    interface MemExePipeline memExeIfc;
    method Action killAll; // kill everything: used by commit stage
    interface Reg#(Bool) doStatsIfc;
endinterface

typedef enum {
`ifdef INCLUDE_GDB_CONTROL
   CORE_HALTING,
   CORE_HALTED,
`endif
   CORE_RUNNING
   } Core_Run_State
deriving (Bits, Eq, FShow);

`ifdef PERFORMANCE_MONITORING
instance BitVectorable #(EventsCore, SizeOf#(SupCnt), EventsCoreElements) provisos (Bits #(EventsCore, m));
   function Vector#(EventsCoreElements, SupCnt) to_vector(EventsCore e) =
      reverse(unpack(pack(e)));
endinstance
instance BitVectorable #(EventsCoreMem, SizeOf#(HpmRpt), EventsCoreMemElements) provisos (Bits #(EventsCoreMem, m));
   function Vector#(EventsCoreMemElements, HpmRpt) to_vector(EventsCoreMem e) =
      reverse(unpack(pack(e)));
endinstance
instance BitVectorable #(EventsCache, SizeOf#(HpmRpt), EventsCacheElements) provisos (Bits #(EventsCache, m));
   function Vector#(EventsCacheElements, HpmRpt) to_vector(EventsCache e) =
      reverse(unpack(pack(e)));
endinstance
`endif

(* synthesize *)
module mkCore#(CoreId coreId)(Core);
    let verbose = False;

   // ================================================================
   Integer verbosity = 0;    // More levels of verbosity control than 'Bool verbose'

    Reg#(Bool) outOfReset <- mkReg(False);
    rule rl_outOfReset if (!outOfReset);
        $fwrite(stderr, "mkProc came out of reset\n");
        outOfReset <= True;
    endrule

    Reg#(Bool) started <- mkReg(False);

   // ================================================================

`ifdef INCLUDE_GDB_CONTROL
    // Using a ConfigReg since scheduling of reads/writes not critical (TODO: verify this)
    Reg #(Core_Run_State) rg_core_run_state <- mkConfigReg (CORE_RUNNING);
`endif

`ifdef INCLUDE_TANDEM_VERIF
   Vector #(SupSize, FIFOF #(Trace_Data2)) v_f_to_TV <- replicateM (mkFIFOF);
`endif

`ifdef PERFORMANCE_MONITORING
   Array #(Reg #(EventsCore)) hpm_core_events <- mkDRegOR (2, unpack (0));
`endif

   // ================================================================

    // front end
    FetchStage fetchStage <- mkFetchStage;
    ITlb iTlb = fetchStage.iTlbIfc;
    ICoCache iMem = fetchStage.iMemIfc;

    // back end
    RFileSynth rf <- mkRFileSynth;

   // Bluespec: CsrFile including external interrupt request methods
    CsrFile csrf <- mkCsrFile(zeroExtend(coreId)); // hartid in CSRF should be core id

    RegRenamingTable regRenamingTable <- mkRegRenamingTable;
    EpochManager epochManager <- mkEpochManager;
    SpecTagManager specTagManager <- mkSpecTagManager;
    ReorderBufferSynth rob <- mkReorderBufferSynth;

    // We have two scoreboards: one conservative and other aggressive
    // - Aggressive sb is checked at rename stage, so inst after rename may be issued early
    // - Conservative sb is checked at reg read stage, to ensure correctness
    // Every pipeline should set both sb if it needs to write reg
    // - Conservative sb is set when data is written into rf
    // - Aggressive sb is set when pipeline sends out wakeup for reservation staion
    //   Note that wakeup can be sent early if it knows when the data will be produced
    ScoreboardCons sbCons <- mkScoreboardCons; // conservative sb
    ScoreboardAggr sbAggr <- mkScoreboardAggr; // aggressive sb

    // MMIO: need to track in flight CSR inst or interrupt; note we can at most
    // 1 CSR inst or 1 interrupt in ROB, so just use 1 bit track it. Commit
    // stage use port 0 to reset this, and Rename stage use port 1 to set this.
    Ehr#(2, Bool) csrInstOrInterruptInflight <- mkEhr(False);
    Reg#(Bool) csrInstOrInterruptInflight_commit = csrInstOrInterruptInflight[0];
    Reg#(Bool) csrInstOrInterruptInflight_rename = csrInstOrInterruptInflight[1];
    MMIOCoreInput mmioInIfc = (interface MMIOCoreInput;
        interface fetch = fetchStage.mmioIfc;
        method getMSIP = csrf.getMSIP;
        method setMSIP = csrf.setMSIP;
        method setMTIP = csrf.setMTIP;
        method noInflightCSRInstOrInterrupt = !csrInstOrInterruptInflight[0];
        method setTime = csrf.setTime;
    endinterface);
    MMIOCore mmio <- mkMMIOCore(mmioInIfc);

    // fix point module to instantiate other function units
    module mkCoreFixPoint#(CoreFixPoint fix)(CoreFixPoint);
        // spec update
        Vector#(AluExeNum, SpeculationUpdate) aluSpecUpdate;
        for(Integer i = 0; i < valueof(AluExeNum); i = i+1) begin
            aluSpecUpdate[i] = fix.aluExeIfc[i].specUpdate;
        end
        Vector#(FpuMulDivExeNum, SpeculationUpdate) fpuMulDivSpecUpdate;
        for(Integer i = 0; i < valueof(FpuMulDivExeNum); i = i+1) begin
            fpuMulDivSpecUpdate[i] = fix.fpuMulDivExeIfc[i].specUpdate;
        end
        GlobalSpecUpdate#(CorrectSpecPortNum, ConflictWrongSpecPortNum) globalSpecUpdate <- mkGlobalSpecUpdate(
            joinSpeculationUpdate(
                append(append(vec(regRenamingTable.specUpdate,
                                  specTagManager.specUpdate,
                                  fix.memExeIfc.specUpdate), aluSpecUpdate), fpuMulDivSpecUpdate)
            ),
            rob.specUpdate
        );

        // whether perf data is collected
        Reg#(Bool) doStatsReg <- mkConfigReg(False); 

        // write aggressive elements + wakupe reservation stations
        function Action writeAggr(Integer wrAggrPort, PhyRIndx dst);
        action
            sbAggr.setReady[wrAggrPort].put(dst);
            for(Integer i = 0; i < valueof(AluExeNum); i = i+1) begin
                fix.aluExeIfc[i].rsAluIfc.setRegReady[wrAggrPort].put(Valid (dst));
            end
            for(Integer i = 0; i < valueof(FpuMulDivExeNum); i = i+1) begin
                fix.fpuMulDivExeIfc[i].rsFpuMulDivIfc.setRegReady[wrAggrPort].put(Valid (dst));
            end
            fix.memExeIfc.rsMemIfc.setRegReady[wrAggrPort].put(Valid (dst));
        endaction
        endfunction

        // write conservative elements
        function Action writeCons(Integer wrConsPort, PhyRIndx dst, Data data);
        action
            rf.write[wrConsPort].wr(dst, data);
            sbCons.setReady[wrConsPort].put(dst);
        endaction
        endfunction

        Vector#(AluExeNum, FIFO#(FetchTrainBP)) trainBPQ <- replicateM(mkFIFO);
        Vector#(AluExeNum, AluExePipeline) aluExe;
        for(Integer i = 0; i < valueof(AluExeNum); i = i+1) begin
            Vector#(2, SendBypass) sendBypassIfc; // exe and finish
            for(Integer sendPort = 0; sendPort < 2; sendPort = sendPort + 1) begin
                sendBypassIfc[sendPort] = (interface SendBypass;
                    method Action send(PhyRIndx dst, Data data);
                        // broadcast bypass
                        Integer recvPort = valueof(AluExeNum) * sendPort + i;
                        for(Integer j = 0; j < valueof(FpuMulDivExeNum); j = j+1) begin
                            fix.fpuMulDivExeIfc[j].recvBypass[recvPort].recv(dst, data);
                        end
                        fix.memExeIfc.recvBypass[recvPort].recv(dst, data);
                        for(Integer j = 0; j < valueof(AluExeNum); j = j+1) begin
                            fix.aluExeIfc[j].recvBypass[recvPort].recv(dst, data);
                        end
                    endmethod
                endinterface);
            end
            let aluExeInput = (interface AluExeInput;
                method sbCons_lazyLookup = sbCons.lazyLookup[aluRdPort(i)].get;
                method rf_rd1 = rf.read[aluRdPort(i)].rd1;
                method rf_rd2 = rf.read[aluRdPort(i)].rd2;
                method csrf_rd = csrf.rd;
                method rob_getPC = rob.getOrigPC[i].get;
                method rob_getPredPC = rob.getOrigPredPC[i].get;
                method rob_getOrig_Inst = rob.getOrig_Inst[i].get;
                method rob_setExecuted = rob.setExecuted_doFinishAlu[i].set;
                method fetch_train_predictors = toPut(trainBPQ[i]).put;
                method setRegReadyAggr = writeAggr(aluWrAggrPort(i));
                interface sendBypass = sendBypassIfc;
                method writeRegFile = writeCons(aluWrConsPort(i));
                method Action redirect(Addr new_pc, SpecTag spec_tag, InstTag inst_tag);
                    if (verbose) begin
                        $display("[ALU redirect - %d] ", i, fshow(new_pc),
                                 "; ", fshow(spec_tag), "; ", fshow(inst_tag));
                    end
                    epochManager.incrementEpoch;
                    fetchStage.redirect(new_pc);
                    globalSpecUpdate.incorrectSpec(False, spec_tag, inst_tag);
                endmethod
                method correctSpec = globalSpecUpdate.correctSpec[finishAluCorrectSpecPort(i)].put;
                method doStats = doStatsReg._read;
            endinterface);
            aluExe[i] <- mkAluExePipeline(aluExeInput);
            // truly call fetch method to train branch predictor
            rule doFetchTrainBP;
                let train <- toGet(trainBPQ[i]).get;
                fetchStage.train_predictors(
                    train.pc, train.nextPc, train.iType, train.taken,
                    train.dpTrain, train.mispred, train.isCompressed
                );
            endrule
        end

        Vector#(FpuMulDivExeNum, FpuMulDivExePipeline) fpuMulDivExe;
        for(Integer i = 0; i < valueof(FpuMulDivExeNum); i = i+1) begin
            let fpuMulDivExeInput = (interface FpuMulDivExeInput;
                method sbCons_lazyLookup = sbCons.lazyLookup[fpuMulDivRdPort(i)].get;
                method rf_rd1 = rf.read[fpuMulDivRdPort(i)].rd1;
                method rf_rd2 = rf.read[fpuMulDivRdPort(i)].rd2;
                method rf_rd3 = rf.read[fpuMulDivRdPort(i)].rd3;
                method csrf_rd = csrf.rd;
                method rob_setExecuted = rob.setExecuted_doFinishFpuMulDiv[i].set;
                method Action writeRegFile(PhyRIndx dst, Data data);
                    writeAggr(fpuMulDivWrAggrPort(i), dst);
                    writeCons(fpuMulDivWrConsPort(i), dst, data);
                endmethod
                method conflictWrongSpec = globalSpecUpdate.conflictWrongSpec[finishFpuMulDivConflictWrongSpecPort(i)].put(?);
                method doStats = doStatsReg._read;
            endinterface);
            fpuMulDivExe[i] <- mkFpuMulDivExePipeline(fpuMulDivExeInput);
        end

        let memExeInput = (interface MemExeInput;
            method sbCons_lazyLookup = sbCons.lazyLookup[memRdPort].get;
            method rf_rd1 = rf.read[memRdPort].rd1;
            method rf_rd2 = rf.read[memRdPort].rd2;
            method csrf_rd = csrf.rd;
            method rob_getPC = rob.getOrigPC[valueof(AluExeNum)].get; // last getPC port
            method rob_setExecuted_doFinishMem = rob.setExecuted_doFinishMem;
`ifdef INCLUDE_TANDEM_VERIF
	    method rob_setExecuted_doFinishMem_RegData = rob.setExecuted_doFinishMem_RegData;
`endif
            method rob_setExecuted_deqLSQ = rob.setExecuted_deqLSQ;
            method isMMIOAddr = mmio.isMMIOAddr;
            method mmioReq = mmio.dataReq;
            method mmioRespVal = mmio.dataRespVal;
            method mmioRespDeq = mmio.dataRespDeq;
            method setRegReadyAggr_mem = writeAggr(memWrAggrPort);
            method setRegReadyAggr_forward = writeAggr(forwardWrAggrPort);
            method writeRegFile = writeCons(memWrConsPort);
            method doStats = doStatsReg._read;
        endinterface);
        let memExe <- mkMemExePipeline(memExeInput);

        interface aluExeIfc = aluExe;
        interface fpuMulDivExeIfc = fpuMulDivExe;
        interface memExeIfc = memExe;
        method Action killAll;
            globalSpecUpdate.incorrectSpec(True, ?, ?);
        endmethod
        interface doStatsIfc = doStatsReg;
    endmodule
    CoreFixPoint coreFix <- moduleFix(mkCoreFixPoint);

    Vector#(AluExeNum, ReservationStationAlu) reservationStationAlu;
    for(Integer i = 0; i < valueof(AluExeNum); i = i+1) begin
        reservationStationAlu[i] = coreFix.aluExeIfc[i].rsAluIfc;
    end
    Vector#(FpuMulDivExeNum, ReservationStationFpuMulDiv) reservationStationFpuMulDiv;
    for(Integer i = 0; i < valueof(FpuMulDivExeNum); i = i+1) begin
        reservationStationFpuMulDiv[i] = coreFix.fpuMulDivExeIfc[i].rsFpuMulDivIfc;
    end
    ReservationStationMem reservationStationMem = coreFix.memExeIfc.rsMemIfc;
    DTlbSynth dTlb = coreFix.memExeIfc.dTlbIfc;
    SplitLSQ lsq = coreFix.memExeIfc.lsqIfc;
    StoreBuffer stb = coreFix.memExeIfc.stbIfc;
    DCoCache dMem = coreFix.memExeIfc.dMemIfc;

    // L2 TLB
    L2Tlb l2Tlb <- mkL2Tlb;
    mkTlbConnect(iTlb.toParent, dTlb.toParent, l2Tlb.toChildren);

    // flags to flush
    Reg#(Bool)  flush_tlbs <- mkReg(False);
    Reg#(Bool)  update_vm_info <- mkReg(False);
    Reg#(Bool)  flush_reservation <- mkReg(False);

`ifdef SECURITY_OR_INCLUDE_GDB_CONTROL
    Reg#(Bool)  flush_caches <- mkReg(False);
    Reg#(Bool)  flush_brpred <- mkReg(False);
`else
    Reg#(Bool)  flush_caches <- mkReadOnlyReg(False);
    Reg#(Bool)  flush_brpred <- mkReadOnlyReg(False);
`endif

`ifdef SELF_INV_CACHE
    Reg#(Bool)  reconcile_i <- mkReg(False);
`else
    Reg#(Bool)  reconcile_i <- mkReadOnlyReg(False);
`endif
`ifdef SELF_INV_CACHE
`ifdef SYSTEM_SELF_INV_L1D
    Reg#(Bool)  reconcile_d <- mkReg(False);
`else // !SYSTEM_SELF_INV_L1D
    Reg#(Bool)  reconcile_d <- mkReadOnlyReg(False);
`endif // SYSTEM_SELF_INV_L1D
`else // !SELF_INV_CACHE
    Reg#(Bool)  reconcile_d <- mkReadOnlyReg(False);
`endif // SELF_INV_CACHE

    // performance counters
    Reg#(Bool) doStats = coreFix.doStatsIfc; // whether data is collected
`ifdef PERF_COUNT
    // OOO execute stag (in AluExePipeline and MemExePipeline)

    // commit stage (many in CommitStage.bsv)
    // cycle
    Count#(Data) cycleCnt <- mkCount(0);

    // buffer/tags size
    Count#(Data) ldqFullCycles <- mkCount(0);
    Count#(Data) stqFullCycles <- mkCount(0);
    Count#(Data) robFullCycles <- mkCount(0);
    Count#(Data) aluRS0FullCycles <- mkCount(0);
    Count#(Data) aluRS1FullCycles <- mkCount(0);
    Count#(Data) fpuMulDivRSFullCycles <- mkCount(0);
    Count#(Data) memRSFullCycles <- mkCount(0);
    Count#(Data) epochFullCycles <- mkCount(0);
    Count#(Data) specTagFullCycles <- mkCount(0);

    // FIFOs to connect performance counters
    FIFO#(ExeStagePerfType) exePerfReqQ <- mkFIFO1;
    FIFO#(ComStagePerfType) comPerfReqQ <- mkFIFO1;
    FIFO#(CoreSizePerfType) sizePerfReqQ <- mkFIFO1;
    Fifo#(1, PerfResp#(ExeStagePerfType)) exePerfRespQ <- mkCFFifo;
    Fifo#(1, PerfResp#(ComStagePerfType)) comPerfRespQ <- mkCFFifo;
    Fifo#(1, PerfResp#(CoreSizePerfType)) sizePerfRespQ <- mkCFFifo;

    // FIFO of perf resp
    FIFO#(ProcPerfResp) perfRespQ <- mkFIFO1;
`endif
    // FIFO of perf req
    FIFO#(ProcPerfReq) perfReqQ <- mkFIFO1;

    // -- End of performance counters

`ifdef CHECK_DEADLOCK
    // when to start deadlock checking
    Reg#(Bool) startDeadlockCheck <- mkReg(False);
    FIFO#(void) deadlockCheckStartedQ <- mkFIFO;

    rule doStartDeadlockCheck(!startDeadlockCheck && started);
        startDeadlockCheck <= True;
        deadlockCheckStartedQ.enq(?);
    endrule
`endif

    // Rename stage
    let renameInput = (interface RenameInput;
        interface fetchIfc = fetchStage;
        interface robIfc = rob;
        interface rtIfc = regRenamingTable;
        interface sbConsIfc = sbCons;
        interface sbAggrIfc = sbAggr;
        interface csrfIfc = csrf;
        interface emIfc = epochManager;
        interface smIfc = specTagManager;
        interface rsAluIfc = reservationStationAlu;
        interface rsFpuMulDivIfc = reservationStationFpuMulDiv;
        interface rsMemIfc = reservationStationMem;
        interface lsqIfc = lsq;
        method pendingMMIOPRq = mmio.hasPendingPRq;
        method issueCsrInstOrInterrupt = csrInstOrInterruptInflight_rename._write(True);
        method Bool checkDeadlock;
`ifdef CHECK_DEADLOCK
            return startDeadlockCheck;
`else
            return False;
`endif
        endmethod
        method doStats = coreFix.doStatsIfc._read;
`ifdef INCLUDE_GDB_CONTROL
        method Bool core_is_running = (rg_core_run_state == CORE_RUNNING);
`endif
    endinterface);
    RenameStage renameStage <- mkRenameStage(renameInput);

    // commit stage
    let commitInput = (interface CommitInput;
        interface robIfc = rob;
        interface rtIfc = regRenamingTable;
        interface csrfIfc = csrf;
        method stbEmpty = stb.isEmpty;
        method stqEmpty = lsq.stqEmpty;
        method lsqSetAtCommit = lsq.setAtCommit;
        method tlbNoPendingReq = iTlb.noPendingReq && dTlb.noPendingReq;

        method setFlushTlbs;
	   action
	      flush_tlbs <= True;
              // $display ("%0d: %m.commitInput.setFlushTlbs", cur_cycle);
	   endaction
        endmethod

        method setUpdateVMInfo;
	   action
	      update_vm_info <= True;
              // $display ("%0d: %m.commitInput.setUpdateVMInfo", cur_cycle);
	   endaction
        endmethod

        method setFlushReservation;
	   action
	      flush_reservation <= True;
              // $display ("%0d: %m.commitInput.setFlushReservation", cur_cycle);
	   endaction
        endmethod

        method setFlushBrPred;
	   action
	      flush_brpred <= True;
              // $display ("%0d: %m.commitInput.setFlushBrPred", cur_cycle);
	   endaction
        endmethod

        method setFlushCaches;
	   action
	      flush_caches <= True;
              // $display ("%0d: %m.commitInput.setFlushCaches", cur_cycle);
	   endaction
        endmethod

        method setReconcileI = reconcile_i._write(True);
        method setReconcileD = reconcile_d._write(True);
        method killAll = coreFix.killAll;
        method redirectPc = fetchStage.redirect;
        method setFetchWaitRedirect = fetchStage.setWaitRedirect;
`ifdef INCLUDE_GDB_CONTROL
        method setFetchWaitFlush    = fetchStage.setWaitFlush;
`endif
        method incrementEpoch = epochManager.incrementEpoch;
        method commitCsrInstOrInterrupt = csrInstOrInterruptInflight_commit._write(False);
        method doStats = coreFix.doStatsIfc._read;
        method Bool checkDeadlock;
`ifdef CHECK_DEADLOCK
            return startDeadlockCheck;
`else
            return False;
`endif
        endmethod

`ifdef INCLUDE_TANDEM_VERIF
       interface v_to_TV = map (toPut, v_f_to_TV);
`endif

    endinterface);
    CommitStage commitStage <- mkCommitStage(commitInput);

    // send rob enq time to reservation stations
    (* fire_when_enabled, no_implicit_conditions *)
    rule sendRobEnqTime;
        InstTime t = rob.getEnqTime;
        reservationStationMem.setRobEnqTime(t);
        for(Integer i = 0; i < valueof(FpuMulDivExeNum); i = i+1) begin
            reservationStationFpuMulDiv[i].setRobEnqTime(t);
        end
        for(Integer i = 0; i < valueof(AluExeNum); i = i+1) begin
            reservationStationAlu[i].setRobEnqTime(t);
        end
    endrule

    // preempt has 2 functions here
    // 1. break scheduling cycles
    // 2. XXX since csrf is configReg now, we should not let this rule fire together with doCommit
    // because we read csrf here and write csrf in doCommit

    // TODO We can use wires to catch flush / updateVM enable sigals, because
    // there cannot be any instruction in pipeline (there can be poisoned inst
    // which cannot change CSR or link reg in D$), so doCommit cannot fire.
    // MMIO manager may change pending interrupt bits, but will not affect VM
    // info.
    (* preempts = "prepareCachesAndTlbs, commitStage.doCommitTrap_handle" *)
    (* preempts = "prepareCachesAndTlbs, commitStage.doCommitSystemInst" *)
    rule prepareCachesAndTlbs(flush_reservation || flush_tlbs || update_vm_info);
        if (flush_reservation) begin
            flush_reservation <= False;
            dMem.resetLinkAddr;
	   // $display ("%0d: %m.rule prepareCachesAndTlbs: flushing reservation", cur_cycle);
        end
        if (flush_tlbs) begin
            flush_tlbs <= False;
            iTlb.flush;
            dTlb.flush;
	   // $display ("%0d: %m.rule prepareCachesAndTlbs: flushing iTlb and dTlb", cur_cycle);
        end
        if (update_vm_info) begin
            update_vm_info <= False;
            let vmI = csrf.vmI;
            let vmD = csrf.vmD;
            iTlb.updateVMInfo(vmI);
            dTlb.updateVMInfo(vmD);
            l2Tlb.updateVMInfo(vmI, vmD);
	   // $display ("%0d: %m.rule prepareCachesAndTlbs: updating VMInfo", cur_cycle);
        end
    endrule

`ifdef SECURITY_OR_INCLUDE_GDB_CONTROL
    // Use wires to capture flush regs and empty signals. This is ok because
    // there cannot be any activity to make empty -> not-empty or need-flush ->
    // no-need-flush when we are trying to flush.
    PulseWire doFlushCaches <- mkPulseWire;
    PulseWire doFlushBrPred <- mkPulseWire;

    rule setDoFlushCaches(flush_caches && fetchStage.emptyForFlush && lsq.noWrongPathLoads);
        doFlushCaches.send;
        // $display ("%0d: %m.rl_setDoFlushCaches", cur_cycle);
    endrule

    rule setDoFlushBrPred(flush_brpred && fetchStage.emptyForFlush);
        doFlushBrPred.send;
    endrule

    // security flush cache: need to wait for wrong path loads or inst fetches
    // to finish
    rule flushCaches(doFlushCaches);
        flush_caches <= False;
        iMem.flush;
        dMem.flush;
        // $display ("%0d: %m.rule flushCaches (imem and dmem)", cur_cycle);
    endrule

    // security flush branch predictors: wait for wrong path inst fetches to
    // finish
    rule flushBrPred(doFlushBrPred);
        flush_brpred <= False;
        fetchStage.flush_predictors;
        // $display ("%0d: %m.rule flushBrPred", cur_cycle);
    endrule
`endif

`ifdef SELF_INV_CACHE
    // Use wires to capture flush regs and empty signals. This is ok because
    // there cannot be any activity to make empty -> not-empty or need-flush ->
    // no-need-flush when we are trying to flush.
    PulseWire doReconcileI <- mkPulseWire;

    // We don't really need to wait for fetch to be empty, but just in case we
    // back pressure I TLB because I$ is reconciling.
    rule setDoReconcileI(reconcile_i && fetchStage.emptyForFlush);
        doReconcileI.send;
    endrule

    rule reconcileI(doReconcileI);
        reconcile_i <= False;
        iMem.reconcile;
    endrule

`ifdef SYSTEM_SELF_INV_L1D
    PulseWire doReconcileD <- mkPulseWire;

    Reg#(Bool) waitReconcileD <- mkReg(False);

    // We don't really need to wait for lsq empty, but just in case
    rule setDoReconcileD(reconcile_d && lsq.noWrongPathLoads);
        doReconcileD.send;
    endrule

    rule startReconcileD(doReconcileD && !waitReconcileD);
        coreFix.memExeIfc.reconcile.request.put(?);
        waitReconcileD <= True;
    endrule

    rule completeReconcileD(waitReconcileD);
        let unused <- coreFix.memExeIfc.reconcile.response.get;
        waitReconcileD <= False;
        reconcile_d <= False;
    endrule
`endif // SYSTEM_SELF_INV_L1D
`endif // SELF_INV_CACHE

    rule readyToFetch(
`ifdef INCLUDE_GDB_CONTROL
        (rg_core_run_state == CORE_RUNNING) &&
`endif
        !flush_reservation && !flush_tlbs && !update_vm_info
        && iTlb.flush_done && dTlb.flush_done
`ifdef SECURITY_OR_INCLUDE_GDB_CONTROL
        && !flush_caches && !flush_brpred
        && iMem.flush_done && dMem.flush_done
        && fetchStage.flush_predictors_done
`endif
`ifdef SELF_INV_CACHE
        && !reconcile_i && iMem.reconcile_done
`ifdef SYSTEM_SELF_INV_L1D
        && !reconcile_d
`endif
`endif
    );
        fetchStage.done_flushing();

`ifdef INCLUDE_GDB_CONTROL
        if (commitStage.is_debug_halted) begin
	   started           <= False;
	   rg_core_run_state <= CORE_HALTING;
	   if (verbosity >= 1)
	      $display ("%0d: %m.rule readyToFetch: halting for debug mode", cur_cycle);
	end
`endif
    endrule

`ifdef PERF_COUNT
    // incr cycle count
    (* fire_when_enabled, no_implicit_conditions *)
    rule incCycleCnt(doStats);
        cycleCnt.incr(1);
    endrule

    // incr buffer full cycles
    (* fire_when_enabled, no_implicit_conditions *)
    rule incLdQFull(doStats && lsq.ldqFull_ehrPort0);
        ldqFullCycles.incr(1);
    endrule
    (* fire_when_enabled, no_implicit_conditions *)
    rule incStQFull(doStats && lsq.stqFull_ehrPort0);
        stqFullCycles.incr(1);
    endrule
    (* fire_when_enabled, no_implicit_conditions *)
    rule incROBFull(doStats && rob.isFull_ehrPort0);
        robFullCycles.incr(1);
    endrule
    (* fire_when_enabled, no_implicit_conditions *)
    rule incAluRS0Full(doStats && reservationStationAlu[0].isFull_ehrPort0);
        aluRS0FullCycles.incr(1);
    endrule
    (* fire_when_enabled, no_implicit_conditions *)
    rule incAluRS1Full(doStats && reservationStationAlu[1].isFull_ehrPort0);
        aluRS1FullCycles.incr(1);
    endrule
    (* fire_when_enabled, no_implicit_conditions *)
    rule incFpuMulDivRSFull(doStats && reservationStationFpuMulDiv[0].isFull_ehrPort0);
        fpuMulDivRSFullCycles.incr(1);
    endrule
    (* fire_when_enabled, no_implicit_conditions *)
    rule incMemRSFull(doStats && reservationStationMem.isFull_ehrPort0);
        memRSFullCycles.incr(1);
    endrule
    (* fire_when_enabled, no_implicit_conditions *)
    rule incEpochFull(doStats && epochManager.isFull_ehrPort0);
        epochFullCycles.incr(1);
    endrule
    (* fire_when_enabled, no_implicit_conditions *)
    rule incSpecTagFull(doStats && specTagManager.isFull_ehrPort0);
        specTagFullCycles.incr(1);
    endrule

    // broadcast whether we should collect data
    rule broadcastDoStats;
        let stats = csrf.doPerfStats;
        doStats <= stats;
        iMem.perf.setStatus(stats);
        dMem.perf.setStatus(stats);
        iTlb.perf.setStatus(stats);
        dTlb.perf.setStatus(stats);
        l2Tlb.perf.setStatus(stats);
        fetchStage.perf.setStatus(stats);

        if(stats && !doStats) begin
            $display("[stats] enabled");
        end
        else if(!stats && doStats) begin
            $display("[stats] disabled");
        end
    endrule

    // dispatch perf req
    rule dispathPerfReq;
        perfReqQ.deq;
        let r = perfReqQ.first;
        case(r.loc)
            ICache: begin
                iMem.perf.req(unpack(truncate(r.pType)));
            end
            DCache: begin
                dMem.perf.req(unpack(truncate(r.pType)));
            end
            ITlb: begin
                iTlb.perf.req(unpack(truncate(r.pType)));
            end
            DTlb: begin
                dTlb.perf.req(unpack(truncate(r.pType)));
            end
            L2Tlb: begin
                l2Tlb.perf.req(unpack(truncate(r.pType)));
            end
            DecStage: begin
                fetchStage.perf.req(unpack(truncate(r.pType)));
            end
            ExeStage: begin
                exePerfReqQ.enq(unpack(truncate(r.pType)));
            end
            ComStage: begin
                comPerfReqQ.enq(unpack(truncate(r.pType)));
            end
            CoreSize: begin
                sizePerfReqQ.enq(unpack(truncate(r.pType)));
            end
            default: begin
                $fwrite(stderr, "[WARNING] unrecognzied perf req location ", fshow(r.loc), "\n");
                doAssert(False, "unknown perf location");
            end
        endcase
    endrule

    // handle perf req: exe stage
    rule readPerfCnt_Exe;
        function Data getAluCnt(ExeStagePerfType pType);
            Data cnt = 0;
            for(Integer i = 0; i < valueof(AluExeNum); i = i+1) begin
                cnt = cnt + coreFix.aluExeIfc[i].getPerf(pType);
            end
            return cnt;
        endfunction

        function Data getFpuMulDivCnt(ExeStagePerfType pType);
            Data cnt = 0;
            for(Integer i = 0; i < valueof(FpuMulDivExeNum); i = i+1) begin
                cnt = cnt + coreFix.fpuMulDivExeIfc[i].getPerf(pType);
            end
            return cnt;
        endfunction

        let pType <- toGet(exePerfReqQ).get;
        Data data = (case(pType)
            SupRenameCnt, SpecNoneCycles, SpecNonMemCycles: renameStage.getPerf(pType);
            ExeRedirectBr, ExeRedirectJr, ExeRedirectOther: getAluCnt(pType);
            ExeTlbExcep, ExeScSuccessCnt,
            ExeLrScAmoAcqCnt, ExeLrScAmoRelCnt,
            ExeFenceAcqCnt, ExeFenceRelCnt, ExeFenceCnt,
            ExeLdStallByLd, ExeLdStallBySt, ExeLdStallBySB,
            ExeLdForward, ExeLdMemLat, ExeStMemLat,
            ExeLdToUseLat, ExeLdToUseCnt: coreFix.memExeIfc.getPerf(pType);
            ExeIntMulCnt, ExeIntDivCnt,
            ExeFpFmaCnt, ExeFpDivCnt, ExeFpSqrtCnt: getFpuMulDivCnt(pType);
            default: 0;
        endcase);
        exePerfRespQ.enq(PerfResp {
            pType: pType,
            data: data
        });
    endrule

    // handle perf req: com stage
    rule readPerfCnt_Com;
        let pType <- toGet(comPerfReqQ).get;
        Data data = (case(pType)
            CycleCnt: cycleCnt;
            default: commitStage.getPerf(pType);
        endcase);
        comPerfRespQ.enq(PerfResp {
            pType: pType,
            data: data
        });
    endrule

    // handle perf req: core size
    rule readPerfCnt_Size;
        let pType <- toGet(sizePerfReqQ).get;
        Data data = (case(pType)
            LdQFullCycles: ldqFullCycles;
            StQFullCycles: stqFullCycles;
            ROBFullCycles: robFullCycles;
            AluRS0FullCycles: aluRS0FullCycles;
            AluRS1FullCycles: aluRS1FullCycles;
            FpuMulDivRSFullCycles: fpuMulDivRSFullCycles;
            MemRSFullCycles: memRSFullCycles;
            EpochFullCycles: epochFullCycles;
            SpecTagFullCycles: specTagFullCycles;
            default: 0;
        endcase);
        sizePerfRespQ.enq(PerfResp {
            pType: pType,
            data: data
        });
    endrule

    // gather perf resp
    rule gatherPerfResp;
        Maybe#(ProcPerfResp) resp = Invalid;
        if(iMem.perf.respValid) begin
            let r <- iMem.perf.resp;
            resp = Valid(ProcPerfResp {
                loc: ICache,
                pType: zeroExtend(pack(r.pType)),
                data: r.data
            });
        end
        else if(dMem.perf.respValid) begin
            let r <- dMem.perf.resp;
            resp = Valid(ProcPerfResp {
                loc: DCache,
                pType: zeroExtend(pack(r.pType)),
                data: r.data
            });
        end
        else if(iTlb.perf.respValid) begin
            let r <- iTlb.perf.resp;
            resp = Valid(ProcPerfResp {
                loc: ITlb,
                pType: zeroExtend(pack(r.pType)),
                data: r.data
            });
        end
        else if(dTlb.perf.respValid) begin
            let r <- dTlb.perf.resp;
            resp = Valid(ProcPerfResp {
                loc: DTlb,
                pType: zeroExtend(pack(r.pType)),
                data: r.data
            });
        end
        else if(l2Tlb.perf.respValid) begin
            let r <- l2Tlb.perf.resp;
            resp = Valid(ProcPerfResp {
                loc: L2Tlb,
                pType: zeroExtend(pack(r.pType)),
                data: r.data
            });
        end
        else if(fetchStage.perf.respValid) begin
            let r <- fetchStage.perf.resp;
            resp = Valid(ProcPerfResp {
                loc: DecStage,
                pType: zeroExtend(pack(r.pType)),
                data: r.data
            });
        end
        else if(exePerfRespQ.notEmpty) begin
            let r <- toGet(exePerfRespQ).get;
            resp = Valid(ProcPerfResp {
                loc: ExeStage,
                pType: zeroExtend(pack(r.pType)),
                data: r.data
            });
        end
        else if(comPerfRespQ.notEmpty) begin
            let r <- toGet(comPerfRespQ).get;
            resp = Valid(ProcPerfResp {
                loc: ComStage,
                pType: zeroExtend(pack(r.pType)),
                data: r.data
            });
        end
        else if(sizePerfRespQ.notEmpty) begin
            let r <- toGet(sizePerfRespQ).get;
            resp = Valid (ProcPerfResp {
                loc: CoreSize,
                pType: zeroExtend(pack(r.pType)),
                data: r.data
            });
        end
        // enq to resp Q
        if(resp matches tagged Valid .r) begin
            perfRespQ.enq(r);
        end
    endrule
`endif

`ifdef PERFORMANCE_MONITORING
     // ================================================================
     // Performance counters

     Reg#(EventsCache) events_llc_reg <- mkRegU;
     rule report_events;
         EventsCore events = unpack(pack(commitStage.events));
         events.evt_REDIRECT = zeroExtend(pack(fetchStage.redirect_evt));
         hpm_core_events[1] <= events;
     endrule

     Vector #(1, Bit #(Report_Width)) null_evt = replicate (0);
     Vector #(31, Bit #(Report_Width)) mem_core_evts_vec =  to_large_vector (coreFix.memExeIfc.events);
     Vector #(31, Bit #(Report_Width)) other_core_evts_vec = to_large_vector (hpm_core_events[0]);
     Vector #(31, Bit #(Report_Width)) core_evts_vec = unpack(pack(mem_core_evts_vec) | pack(other_core_evts_vec));
     EventsCache instMem = unpack(pack(iMem.events) | pack(iTlb.events));
     Vector #(16, Bit #(Report_Width)) imem_evts_vec = to_large_vector (instMem);
     EventsCache dataMem = unpack(pack(dMem.events) | pack(dTlb.events));
     Vector #(16, Bit #(Report_Width)) dmem_evts_vec = to_large_vector (dataMem);
     Vector #(32, Bit #(Report_Width)) external_evts_vec = replicate (0);//to_large_vector (w_external_evts);
     Vector #(16, Bit #(Report_Width)) llc_evts_vec = to_large_vector (events_llc_reg);

     let events = append (null_evt, core_evts_vec);
     events = append (events, imem_evts_vec);
     events = append (events, dmem_evts_vec);
     events = append (events, external_evts_vec);
     events = append (events, llc_evts_vec);

     (* fire_when_enabled, no_implicit_conditions *)
     rule rl_send_perf_evts;
          csrf.send_performance_events (events);
     endrule
`endif

`ifdef INCLUDE_GDB_CONTROL
   // ================================================================
   // DEBUG MODULE INTERFACE

   Bool show_DM_interactions = False;    // for debugging the interactions

   // ----------------------------------------------------------------
   // Debug Module GPR read/write

   FIFOF #(DM_CPU_Req #(5, 64)) f_gpr_reqs <- mkFIFOF1;
   FIFOF #(DM_CPU_Rsp #(64))    f_gpr_rsps <- mkFIFOF1;

   rule rl_debug_gpr_read (   (rg_core_run_state == CORE_HALTED)
			   && f_gpr_reqs.notEmpty
			   && (! f_gpr_reqs.first.write));
      let req <- pop (f_gpr_reqs);
      Bit #(5) regnum = req.address;

      let arch_regs = ArchRegs {src1: tagged Valid (tagged Gpr regnum),
				src2: tagged Invalid,
				src3: tagged Invalid,
				dst:  tagged Invalid};
      let rename_result = regRenamingTable.rename[0].getRename (arch_regs);
      let phy_rindx     = fromMaybe (?, rename_result.phy_regs.src1);
      let data_out      = rf.read [debuggerPort].rd1 (phy_rindx);

      let rsp = DM_CPU_Rsp {ok: True, data: data_out};
      f_gpr_rsps.enq (rsp);

      if (show_DM_interactions)
	 $display ("%0d: %m.rl_debug_read_gpr: reg %0d => 0x%0h", cur_cycle, regnum, data_out);
   endrule

   rule rl_debug_gpr_write (   (rg_core_run_state == CORE_HALTED)
			    && f_gpr_reqs.notEmpty
			    && f_gpr_reqs.first.write);
      let req <- pop (f_gpr_reqs);
      Bit #(5) regnum = req.address;
      let data_in = req.data;

      let arch_regs = ArchRegs {src1: tagged Valid (tagged Gpr regnum),
				src2: tagged Invalid,
				src3: tagged Invalid,
				dst:  tagged Invalid};
      let rename_result = regRenamingTable.rename[0].getRename (arch_regs);
      let phy_rindx     = fromMaybe (?, rename_result.phy_regs.src1);
      rf.write [debuggerPort].wr (phy_rindx, data_in);

      let rsp = DM_CPU_Rsp {ok: True, data: ?};
      f_gpr_rsps.enq (rsp);

      if (show_DM_interactions)
	 $display ("%0d: %m.rl_debug_gpr_write: reg %0d <= 0x%0h (phy_rindx = %0d)",
		   cur_cycle, regnum, data_in, phy_rindx);
   endrule

   rule rl_debug_gpr_access_busy (rg_core_run_state == CORE_RUNNING);
      let req <- pop (f_gpr_reqs);
      let rsp = DM_CPU_Rsp {ok: False, data: ?};
      f_gpr_rsps.enq (rsp);

      if (show_DM_interactions)
         $display ("%0d: %m.rl_debug_gpr_access_busy", cur_cycle);
   endrule

`ifdef ISA_F
   // ----------------------------------------------------------------
   // Debug Module FPR read/write

   FIFOF #(DM_CPU_Req #(5,  64)) f_fpr_reqs <- mkFIFOF1;
   FIFOF #(DM_CPU_Rsp #(64))     f_fpr_rsps <- mkFIFOF1;

   rule rl_debug_fpr_read (   (rg_core_run_state == CORE_HALTED)
			   && (! f_gpr_reqs.notEmpty)    // prioritize gpr reqs
			   && (! f_fpr_reqs.first.write));
      let req <- pop (f_fpr_reqs);
      Bit #(5) regnum = req.address;

      let arch_regs = ArchRegs {src1: tagged Valid (tagged Fpu regnum),
				src2: tagged Invalid,
				src3: tagged Invalid,
				dst:  tagged Invalid};
      let rename_result = regRenamingTable.rename[0].getRename (arch_regs);
      let phy_rindx     = fromMaybe (?, rename_result.phy_regs.src1);
      let data_out      = rf.read [debuggerPort].rd1 (phy_rindx);

      let rsp = DM_CPU_Rsp {ok: True, data: data_out};
      f_fpr_rsps.enq (rsp);

      if (show_DM_interactions)
	 $display ("%0d: %m.rl_debug_read_fpr: reg %0d => 0x%0h", cur_cycle, regnum, data_out);
   endrule

   rule rl_debug_fpr_write (   (rg_core_run_state == CORE_HALTED)
			    && (! f_gpr_reqs.notEmpty)    // prioritize gpr reqs
			    && f_fpr_reqs.first.write);
      let req <- pop (f_fpr_reqs);
      Bit #(5) regnum = req.address;
      let data_in = req.data;

      let arch_regs = ArchRegs {src1: tagged Valid (tagged Fpu regnum),
				src2: tagged Invalid,
				src3: tagged Invalid,
				dst:  tagged Invalid};
      let rename_result = regRenamingTable.rename[0].getRename (arch_regs);
      let phy_rindx     = fromMaybe (?, rename_result.phy_regs.src1);
      rf.write [debuggerPort].wr (phy_rindx, data_in);

      let rsp = DM_CPU_Rsp {ok: True, data: ?};
      f_fpr_rsps.enq (rsp);

      if (show_DM_interactions)
	 $display ("%0d: %m.rl_debug_write_fpr: reg %0d <= 0x%0h (phy_rindx %0d)",
		   cur_cycle, regnum, data_in, phy_rindx);
   endrule

   rule rl_debug_fpr_access_busy (   (rg_core_run_state == CORE_RUNNING)
				  && f_fpr_reqs.notEmpty);

      let req <- pop (f_fpr_reqs);
      let rsp = DM_CPU_Rsp {ok: False, data: ?};
      f_fpr_rsps.enq (rsp);

      if (show_DM_interactions)
	 $display ("%0d: %m.rl_debug_fpr_access_busy", cur_cycle);
   endrule
`endif

   // ----------------------------------------------------------------
   // Debug Module CSR read/write

   // Debugger CSR read/write request/response
   FIFOF #(DM_CPU_Req #(12, 64)) f_csr_reqs <- mkFIFOF1;
   FIFOF #(DM_CPU_Rsp #(64))     f_csr_rsps <- mkFIFOF1;

   rule rl_debug_csr_read (   (rg_core_run_state == CORE_HALTED)
			   && (! f_csr_reqs.first.write));
      let req <- pop (f_csr_reqs);
      Bit #(12) csr_addr = req.address;
      let data_out = csrf.rd (unpack (csr_addr));

      let rsp = DM_CPU_Rsp {ok: True, data: data_out};
      f_csr_rsps.enq (rsp);

      if (show_DM_interactions)
	 $display ("%0d: %m.rl_debug_read_csr: csr [%0h] => 0x%0h", cur_cycle, csr_addr, data_out);
   endrule

   rule rl_debug_csr_write (   (rg_core_run_state == CORE_HALTED)
			    && f_csr_reqs.first.write);
      let req <- pop (f_csr_reqs);
      Bit #(12) csr_addr = req.address;
      let data_in = req.data;
      csrf.csrInstWr (unpack (csr_addr), data_in);

      let rsp = DM_CPU_Rsp {ok: True, data: ?};
      f_csr_rsps.enq (rsp);

      if (show_DM_interactions)
	 $display ("%0d: %m.rl_debug_write_csr: csr [%0h] <= 0x%0h", cur_cycle, csr_addr, data_in);
   endrule

   rule rl_debug_csr_access_busy (rg_core_run_state == CORE_RUNNING);
      let req <- pop (f_csr_reqs);
      let rsp = DM_CPU_Rsp {ok: False, data: ?};
      f_csr_rsps.enq (rsp);

      if (show_DM_interactions)
	 $display ("%0d: %m.rl_debug_csr_access_busy", cur_cycle);
   endrule

   // ----------------------------------------------------------------
   // Debug Module run-halt control

   FIFOF #(Bool)  f_run_halt_reqs  <- mkFIFOF;
   FIFOF #(Bool)  f_run_halt_rsps  <- mkFIFOF;

   // ----------------
   // Debug Module Halt control

   rule rl_debug_halt_req (   (rg_core_run_state == CORE_RUNNING)
			   && (f_run_halt_reqs.first == False));
      f_run_halt_reqs.deq;

      // Debugger 'halt' request (e.g., GDB '^C' command)
      // This is initiated just like an interrupt.
`ifdef INCLUDE_GDB_CONTROL
      renameStage.debug_halt_req;
`endif

      if (show_DM_interactions)
	 $display ("%0d: %m.rl_debug_halt_req", cur_cycle);
   endrule

   rule rl_debug_halt_req_already_halted (   (rg_core_run_state != CORE_RUNNING)
					  && (f_run_halt_reqs.first == False));
      f_run_halt_reqs.deq;

      // Notify debugger that we're halted, but otherwise ignore the request
      f_run_halt_rsps.enq (False);

      if (show_DM_interactions)
	 $display ("%0d: %m.rl_debug_halt_req_already_halted", cur_cycle);
   endrule

   // Monitors when we've reached halted state while running
   // (due to halt, step or EBREAK) and notifies DM
   rule rl_debug_halted (rg_core_run_state == CORE_HALTING);
      // Notify debugger that we've halted
      f_run_halt_rsps.enq (False);
      rg_core_run_state <= CORE_HALTED;

      if (show_DM_interactions)
	 $display ("%0d: %m.rl_debug_halted", cur_cycle);
   endrule

   // ----------------
   // Debug Module Resume (run) control

   // Resume command when in debug mode
   rule rl_debug_resume (   (rg_core_run_state == CORE_HALTED)
			 && (f_run_halt_reqs.first == True)

			 // prioritise gpr/fpr/csr read/write requests before resuming
			 && (! f_gpr_reqs.notEmpty)
`ifdef ISA_F
			 && (! f_fpr_reqs.notEmpty)
`endif
			 && (! f_csr_reqs.notEmpty));

      f_run_halt_reqs.deq;

      // In Debug Mode, debugger may have updated DCSR (hence privilege level, DCSR[1:0]),
      // and also other VM-related state.
      // The following TLB actions update to a consistent state.
      iTlb.flush;
      dTlb.flush;
      let vmI = csrf.vmI;
      let vmD = csrf.vmD;
      iTlb.updateVMInfo(vmI);
      dTlb.updateVMInfo(vmD);
      l2Tlb.updateVMInfo(vmI, vmD);

      let startpc = csrf.dpc_read;
      fetchStage.redirect (startpc);
      renameStage.debug_resume;
      commitStage.debug_resume;

      started           <= True;
      rg_core_run_state <= CORE_RUNNING;

      // Notify debugger that we've started running
      f_run_halt_rsps.enq (True);

      if (show_DM_interactions)
         $display ("%0d: %m.rl_debug_resume, dpc = 0x%0h", cur_cycle, startpc);
   endrule

   // Run command when already running
   rule rl_debug_run_redundant (   (rg_core_run_state == CORE_RUNNING)
				&& (f_run_halt_reqs.first == True));
      f_run_halt_reqs.deq;

      // Notify debugger that we're running
      f_run_halt_rsps.enq (True);

      if (show_DM_interactions)
	 $display ("%0d: %m.rl_debug_run_redundant", cur_cycle);
   endrule

   // ================================================================
`endif

   // ================================================================
   // INTERFACE

    interface CoreReq coreReq;
        method Action start(
            Bool running,
            Bit#(64) startpc,
            Addr toHostAddr, Addr fromHostAddr
        );
`ifdef INCLUDE_GDB_CONTROL
            if (!running) renameStage.debug_halt_req;
`endif
	    fetchStage.start(startpc);
            started <= True;
`ifdef INCLUDE_GDB_CONTROL
	   rg_core_run_state <= CORE_RUNNING;
`endif
            mmio.setHtifAddrs(toHostAddr, fromHostAddr);

            commitStage.startRenameDebug;
        endmethod

        method Action perfReq(PerfLocation loc, PerfType t);
            perfReqQ.enq(ProcPerfReq {
                loc: loc,
                pType: t
            });
        endmethod
    endinterface

    interface CoreIndInv coreIndInv;
        method ActionValue#(ProcPerfResp) perfResp;
`ifdef PERF_COUNT
            perfRespQ.deq;
            return perfRespQ.first;
`else
            perfReqQ.deq;
            let r = perfReqQ.first;
            return ProcPerfResp {
                loc: r.loc,
                pType: r.pType,
                data: 0
            };
`endif
        endmethod

        method terminate = csrf.terminate;
    endinterface

    interface dCacheToParent = dMem.to_parent;
    interface iCacheToParent = iMem.to_parent;

    interface tlbToMem = l2Tlb.toMem;

    interface mmioToPlatform = mmio.toP;

    method sendDoStats = csrf.sendDoStats;
    method recvDoStats = csrf.recvDoStats;

    // deadlock check
    interface CoreDeadlock deadlock;
        interface dCacheCRqStuck = dMem.cRqStuck;
        interface dCachePRqStuck = dMem.pRqStuck;
        interface iCacheCRqStuck = iMem.cRqStuck;
        interface iCachePRqStuck = iMem.pRqStuck;
        interface renameInstStuck = renameStage.renameInstStuck;
        interface renameCorrectPathStuck = renameStage.renameCorrectPathStuck;
        interface commitInstStuck = commitStage.commitInstStuck;
        interface commitUserInstStuck = commitStage.commitUserInstStuck;
`ifdef CHECK_DEADLOCK
        interface checkStarted = toGet(deadlockCheckStartedQ);
`else
        interface checkStarted = nullGet;
`endif
    endinterface

    // rename debug
    interface CoreRenameDebug renameDebug;
        interface renameErr = commitStage.renameErr;
    endinterface

   // Bluespec: external interrupt requests targeting Machine and Supervisor modes
    method Action setMEIP (v) = csrf.setMEIP (v);
    method Action setSEIP (v) = csrf.setSEIP (v);

`ifdef INCLUDE_GDB_CONTROL
   interface Server  hart_run_halt_server = toGPServer (f_run_halt_reqs, f_run_halt_rsps);
   interface Server  hart_gpr_mem_server  = toGPServer (f_gpr_reqs, f_gpr_rsps);
`ifdef ISA_F
   interface Server  hart_fpr_mem_server  = toGPServer (f_fpr_reqs, f_fpr_rsps);
`endif
   interface Server  hart_csr_mem_server  = toGPServer (f_csr_reqs, f_csr_rsps);
`endif

`ifdef INCLUDE_TANDEM_VERIF
   interface v_to_TV = map (toGet, v_f_to_TV);
`endif

`ifdef PERFORMANCE_MONITORING
    method events_llc = events_llc_reg._write;
`endif

endmodule
