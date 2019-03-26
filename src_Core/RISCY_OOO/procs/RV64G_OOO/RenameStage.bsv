
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
import GetPut::*;
import Cntrs::*;
import Fifo::*;
import FIFO::*;
import Types::*;
import ProcTypes::*;
import CCTypes::*;
import SynthParam::*;
import Performance::*;
import Exec::*;
import FetchStage::*;
import RenamingTable::*;
import ReorderBuffer::*;
import ReorderBufferSynth::*;
import Scoreboard::*;
import ScoreboardSynth::*;
import CsrFile::*;
import SpecTagManager::*;
import EpochManager::*;
import ReservationStationEhr::*;
import ReservationStationAlu::*;
import ReservationStationMem::*;
import ReservationStationFpuMulDiv::*;
import SplitLSQ::*;

typedef struct {
    FetchDebugState fetch;
    EpochDebugState epoch;
} RenameStuck deriving(Bits, Eq, FShow);

interface RenameInput;
    // func units
    interface FetchStage fetchIfc; // just for debug
    interface ReorderBufferSynth robIfc;
    interface RegRenamingTable rtIfc;
    interface ScoreboardCons sbConsIfc;
    interface ScoreboardAggr sbAggrIfc;
    interface CsrFile csrfIfc;
    interface EpochManager emIfc;
    interface SpecTagManager smIfc;
    interface Vector#(AluExeNum, ReservationStationAlu) rsAluIfc;
    interface Vector#(FpuMulDivExeNum, ReservationStationFpuMulDiv) rsFpuMulDivIfc;
    interface ReservationStationMem rsMemIfc;
    interface SplitLSQ lsqIfc;
    // pending MMIO req from platform
    method Bool pendingMMIOPRq;
    // record that a CSR inst or interrupt is sent to ROB
    method Action issueCsrInstOrInterrupt;
    // deadlock check
    method Bool checkDeadlock;
    // performance
    method Bool doStats;
endinterface

interface RenameStage;
    // performance count
    method Data getPerf(ExeStagePerfType t);
    // deadlock check
    interface Get#(RenameStuck) renameInstStuck;
    interface Get#(RenameStuck) renameCorrectPathStuck;
endinterface

module mkRenameStage#(RenameInput inIfc)(RenameStage);
    Bool verbose = True;

    // func units
    FetchStage fetchStage = inIfc.fetchIfc;
    ReorderBufferSynth rob = inIfc.robIfc;
    RegRenamingTable regRenamingTable = inIfc.rtIfc;
    ScoreboardCons sbCons = inIfc.sbConsIfc;
    ScoreboardAggr sbAggr = inIfc.sbAggrIfc;
    CsrFile csrf = inIfc.csrfIfc;
    EpochManager epochManager = inIfc.emIfc;
    SpecTagManager specTagManager = inIfc.smIfc;
    Vector#(AluExeNum, ReservationStationAlu) reservationStationAlu = inIfc.rsAluIfc;
    Vector#(FpuMulDivExeNum, ReservationStationFpuMulDiv) reservationStationFpuMulDiv = inIfc.rsFpuMulDivIfc;
    ReservationStationMem reservationStationMem = inIfc.rsMemIfc;
    SplitLSQ lsq = inIfc.lsqIfc;

    // performance counter
`ifdef PERF_COUNT
    Count#(Data) supRenameCnt <- mkCount(0);
`ifdef SECURITY
    Count#(Data) specNoneCycles <- mkCount(0);
    Count#(Data) specNonMemCycles <- mkCount(0);
`endif
`endif

    // deadlock check
`ifdef CHECK_DEADLOCK
    // timer to check deadlock
    Reg#(DeadlockTimer) renameInstTimer <- mkReg(0);
    Reg#(DeadlockTimer) renameCorrectPathTimer <- mkReg(0);
    // FIFOs to output deadlock info
    FIFO#(RenameStuck) renameInstStuckQ <- mkFIFO1;
    FIFO#(RenameStuck) renameCorrectPathStuckQ <- mkFIFO1;
    // wires to indicate that deadlock is reported, so reset timers
    PulseWire renameInstStuckSent <- mkPulseWire;
    PulseWire renameCorrectPathStuckSent <- mkPulseWire;
    // wires to reset timers since processor is making progress
    PulseWire renameWrongPath <- mkPulseWire;
    PulseWire renameCorrectPath <- mkPulseWire;

    let renameStuck = RenameStuck {
        fetch: fetchStage.getFetchState,
        epoch: epochManager.getEpochState
    };

    (* fire_when_enabled *)
    rule checkDeadlock_renameInst(inIfc.checkDeadlock && renameInstTimer == maxBound);
        renameInstStuckQ.enq(renameStuck);
        renameInstStuckSent.send;
    endrule

    (* fire_when_enabled *)
    rule checkDeadlock_renameCorrecPath(inIfc.checkDeadlock && renameCorrectPathTimer == maxBound);
        renameCorrectPathStuckQ.enq(renameStuck);
        renameCorrectPathStuckSent.send;
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule incrDeadlockTimer(inIfc.checkDeadlock);
        function DeadlockTimer getNextTimer(DeadlockTimer t);
            return t == maxBound ? maxBound : t + 1;
        endfunction
        renameInstTimer <= (renameCorrectPath || renameWrongPath || renameInstStuckSent) ? 0 : getNextTimer(renameInstTimer);
        renameCorrectPathTimer <= (renameCorrectPath || renameCorrectPathStuckSent) ? 0 : getNextTimer(renameCorrectPathTimer);
    endrule
`endif

    // kill wrong path inst
    // XXX we have to make this a separate rule instead of merging it with rename correct path
    // This is because the rename correct path rule is conflict with other rules that redirect
    // If wrong path inst keeps coming in, the rename rule may only kill wrong path, but blocks the redirect rule
    rule doRenaming_wrongPath(
        !epochManager.checkEpoch[0].check(fetchStage.pipelines[0].first.main_epoch) // first wrong path, so at least kill one
    );
        // we stop when we see a correct path inst
        Bool stop = False;
        for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
            if(!stop && fetchStage.pipelines[i].canDeq) begin
                let x = fetchStage.pipelines[i].first;
                if(epochManager.checkEpoch[i].check(x.main_epoch)) begin
                    // correct path; stop killing
                    stop = True;
                end
                else begin
                    // wrong path, kill it & update prev epoch
                    fetchStage.pipelines[i].deq;
                    epochManager.updatePrevEpoch[i].update(x.main_epoch);
                    if(verbose) $display("[doRenaming - %d] wrong path: pc = %16x", i, x.pc);
                end
            end
        end
`ifdef CHECK_DEADLOCK
        renameWrongPath.send;
`endif
    endrule

    // check for exceptions and interrupts
    function Maybe#(Trap) getTrap(FromFetchStage x);
        Maybe#(Trap) trap = tagged Invalid;
        let csr_state = csrf.decodeInfo;
        let pending_interrupt = csrf.pending_interrupt;
        let new_exception = checkForException(x.dInst, x.regs, csr_state);
        if (isValid(x.cause)) begin
            // previously found exception
            trap = tagged Valid (tagged Exception fromMaybe(?, x.cause));
        end else if (isValid(pending_interrupt)) begin
            // pending interrupt
            trap = tagged Valid (tagged Interrupt fromMaybe(?, pending_interrupt));
        end else if (isValid(new_exception)) begin
            // newly found exception
            trap = tagged Valid (tagged Exception fromMaybe(?, new_exception));
        end
        return trap;
    endfunction

    // trap for first inst to rename
    Maybe#(Trap) firstTrap = getTrap(fetchStage.pipelines[0].first);

    // XXX Stall renaming till ROB is empty if we need to replay this inst (i.e. system inst)
    // This is a rough fix for a bug with the FPU CSR registers.
    // i.e. stall when doReplay(inst) is true

    function Action incrEpochStallFetch;
    action
        epochManager.incrementEpoch;
        // stall fetch until redirect
        fetchStage.setWaitRedirect;
    endaction
    endfunction

    // rename single trap
    rule doRenaming_Trap(
        !inIfc.pendingMMIOPRq // stall when MMIO pRq is pending
        && epochManager.checkEpoch[0].check(fetchStage.pipelines[0].first.main_epoch) // correct path
        && isValid(firstTrap) // take trap
        && rob.isEmpty // stall for ROB empty
    );
        fetchStage.pipelines[0].deq;
        let x = fetchStage.pipelines[0].first;
        let pc = x.pc;
        let ppc = x.ppc;
        let main_epoch = x.main_epoch;
        let dpTrain = x.dpTrain;
        let inst = x.inst;
        let dInst = x.dInst;
        let arch_regs = x.regs;
        let cause = x.cause;
        if(verbose) $display("[doRenaming] trap: ", fshow(x));

        // update prev epoch
        epochManager.updatePrevEpoch[0].update(main_epoch);
        // Flip epoch without redirecting
        // This avoids doing incorrect work
        incrEpochStallFetch;
        // just place it in the reorder buffer
        let y = ToReorderBuffer{pc: pc,
                                iType: dInst.iType,
                                csr: dInst.csr,
                                claimed_phy_reg: False, // no renaming is done
                                trap: firstTrap,
                                // default values of FullResult
                                ppc_vaddr_csrData: PPC (ppc), // default use PPC
                                fflags: 0,
                                ////////
                                will_dirty_fpu_state: False,
                                rob_inst_state: Executed,
                                lsqTag: ?,
                                ldKilled: Invalid,
                                memAccessAtCommit: False,
                                lsqAtCommitNotified: False,
                                nonMMIOStDone: False,
                                epochIncremented: True, // we have incremented epoch
                                spec_bits: specTagManager.currentSpecBits
                               };
        rob.enqPort[0].enq(y);
        // record if we issue an interrupt
        if(firstTrap matches tagged Valid (tagged Interrupt .i)) begin
            inIfc.issueCsrInstOrInterrupt;
        end
`ifdef CHECK_DEADLOCK
        renameCorrectPath.send;
`endif
    endrule

    // print rename info
    function Action printRename(Integer i,
                                RegsReady regs_ready_cons,
                                RegsReady regs_ready_aggr,
                                ArchRegs arch_regs,
                                PhyRegs phy_regs);
    action
        $display("  [doRenaming - %d] regs_ready: cons ", i, fshow(regs_ready_cons), " ; aggr ", fshow(regs_ready_aggr));
        if (arch_regs.src1 matches tagged Valid .valid_src) begin
            if (phy_regs.src1 matches tagged Valid .valid_src_renamed) begin
                $display("    [SRC RENAMING] ", fshow(valid_src), " -> ", fshow(valid_src_renamed));
            end else begin
                $display("    [SRC RENAMING] ERROR: ", fshow(valid_src), " -> INVALID");
                doAssert(False, "rename src1 invalid");
            end
        end
        if (arch_regs.src2 matches tagged Valid .valid_src) begin
            if (phy_regs.src2 matches tagged Valid .valid_src_renamed) begin
                $fdisplay(stdout, "    [SRC RENAMING] ", fshow(valid_src), " -> ", fshow(valid_src_renamed));
            end else begin
                $fdisplay(stdout, "    [SRC RENAMING] ERROR: ", fshow(valid_src), " -> INVALID");
                doAssert(False, "rename src2 invalid");
            end
        end
        if (arch_regs.src3 matches tagged Valid .valid_src) begin
            if (phy_regs.src3 matches tagged Valid .valid_src_renamed) begin
                $fdisplay(stdout, "    [SRC RENAMING] ", fshow(valid_src), " -> ", fshow(valid_src_renamed));
            end else begin
                $fdisplay(stdout, "    [SRC RENAMING] ERROR: ", fshow(valid_src), " -> INVALID");
                doAssert(False, "rename src3 invalid");
            end
        end
        if (arch_regs.dst matches tagged Valid .valid_dst) begin
            if (phy_regs.dst matches tagged Valid .valid_dst_renamed) begin
                $fdisplay(stdout, "    [DST RENAMING] ", fshow(valid_dst), " => ", fshow(valid_dst_renamed));
            end else begin
                $fdisplay(stdout, "    [DST RENAMING] ERROR: ", fshow(valid_dst), " -> INVALID");
                doAssert(False, "rename dst invalid");
            end
        end
    endaction
    endfunction

    // check for system inst that needs to replay
    Bool firstReplay = doReplay(fetchStage.pipelines[0].first.dInst.iType);

    // System inst is renamed only when ROB is empty
    rule doRenaming_SystemInst(
        !inIfc.pendingMMIOPRq // stall when MMIO pRq is pending
        && epochManager.checkEpoch[0].check(fetchStage.pipelines[0].first.main_epoch) // correct path
        && !isValid(firstTrap) // not trap
        && firstReplay // system inst needs replay
        && rob.isEmpty // stall for ROB empty
    );
        fetchStage.pipelines[0].deq;
        let x = fetchStage.pipelines[0].first;
        let pc = x.pc;
        let ppc = x.ppc;
        let main_epoch = x.main_epoch;
        let dpTrain = x.dpTrain;
        let inst = x.inst;
        let dInst = x.dInst;
        let arch_regs = x.regs;
        let cause = x.cause;
        if(verbose) $display("[doRenaming] system inst: ", fshow(x));

        // update prev epoch
        epochManager.updatePrevEpoch[0].update(main_epoch);
        // Flip epoch without redirecting. This avoids doing incorrect work
        incrEpochStallFetch;

        // get spec bits (should be 0), and no need to checkout spec tag
        let spec_bits = specTagManager.currentSpecBits;
        doAssert(spec_bits == 0, "cannot have spec bits");

        // do renaming (renaming is non-speculative)
        let rename_result = regRenamingTable.rename[0].getRename(arch_regs);
        let phy_regs = rename_result.phy_regs;
        regRenamingTable.rename[0].claimRename(arch_regs, spec_bits);

        // scoreboard lookup
        let regs_ready_cons = sbCons.eagerLookup[0].get(phy_regs);
        let regs_ready_aggr = sbAggr.eagerLookup[0].get(phy_regs);
        sbCons.setBusy[0].set(phy_regs.dst);
        sbAggr.setBusy[0].set(phy_regs.dst);

        // print rename info
        if (verbose) begin
            printRename(0, regs_ready_cons, regs_ready_aggr, arch_regs, phy_regs);
        end

        // get ROB tag
        let inst_tag = rob.enqPort[0].getEnqInstTag;

        // CSR inst will be sent to ALU exe pipeline
        Bool to_exec = False;
        if (dInst.execFunc matches tagged Alu .alu) begin
            to_exec = True;
            doAssert(dInst.iType == Csr, "only CSR inst send to exe");
        end
        else begin
            doAssert(dInst.iType == FenceI ||
                     dInst.iType == SFence ||
                     dInst.iType == Sret ||
                     dInst.iType == Mret,
                     "non-CSR inst not send to exe");
            doAssert(dInst.execFunc == tagged Other,
                     "non-exe inst exec func is other");
        end

        // send to ALU reservation station
        if (to_exec) begin
            reservationStationAlu[0].enq(ToReservationStation {
                data: AluRSData {dInst: dInst, dpTrain: dpTrain},
                regs: phy_regs,
                tag: inst_tag,
                spec_bits: spec_bits,
                spec_tag: Invalid,
                regs_ready: regs_ready_aggr // alu will recv bypass
            });
        end

        // send to ROB
        Bool will_dirty_fpu_state = False;
        if (arch_regs.dst matches tagged Valid( tagged Fpu .r )) begin
            will_dirty_fpu_state = True;
            doAssert(False, "system inst never touches FP regs");
        end
        RobInstState rob_inst_state = to_exec ? NotDone : Executed;
        let y = ToReorderBuffer{pc: pc,
                                iType: dInst.iType,
                                csr: dInst.csr,
                                claimed_phy_reg: True, // XXX we always claim a free reg in rename
                                trap: Invalid, // no trap
                                // default values of FullResult
                                ppc_vaddr_csrData: PPC (ppc), // default use PPC
                                fflags: 0,
                                ////////
                                will_dirty_fpu_state: will_dirty_fpu_state,
                                rob_inst_state: rob_inst_state,
                                lsqTag: ?,
                                ldKilled: Invalid,
                                memAccessAtCommit: False,
                                lsqAtCommitNotified: False,
                                nonMMIOStDone: False,
                                epochIncremented: True, // system inst has incremented epoch
                                spec_bits: spec_bits
                               };
        rob.enqPort[0].enq(y);

        // record if we issue an CSR inst
        if(dInst.iType == Csr) begin
            inIfc.issueCsrInstOrInterrupt;
        end

`ifdef CHECK_DEADLOCK
        renameCorrectPath.send;
`endif
    endrule

`ifdef SECURITY
    // speculation control:
    // M mode: turn off speculation for mem inst only
    // non-M mode: controlled by mspec CSR
    Bool machineMode = csrf.decodeInfo.prv == prvM;
    Bool specNone = !machineMode && csrf.rd(CSRmspec) == zeroExtend(mSpecNone);
    Bool specNonMem = machineMode || csrf.rd(CSRmspec) == zeroExtend(mSpecNonMem);

`ifdef PERF_COUNT
    rule incSpecNoneCycles(inIfc.doStats && specNone);
        specNoneCycles.incr(1);
    endrule
    rule incSpecNonMemCycles(inIfc.doStats && specNonMem);
        specNonMemCycles.incr(1);
    endrule
`endif

    // first inst is mem inst
    function Bool isMemInst(ExecFunc f);
        return f matches tagged Mem .m ? True : False;
    endfunction
    Bool firstMem = isMemInst(fetchStage.pipelines[0].first.dInst.execFunc);

    // In case speculation is turned off for mem inst only, we rename mem inst
    // only when ROB is empty
    rule doRenaming_MemInst(
        !inIfc.pendingMMIOPRq // stall when MMIO pRq is pending
        && epochManager.checkEpoch[0].check(fetchStage.pipelines[0].first.main_epoch) // correct path
        && !isValid(firstTrap) // not trap
        && !firstReplay // not system inst
        // turn off speculation for mem inst only, and first inst is mem
        && (specNonMem && firstMem)
        && rob.isEmpty // stall for ROB empty to process mem inst
    );
        fetchStage.pipelines[0].deq;
        let x = fetchStage.pipelines[0].first;
        let pc = x.pc;
        let ppc = x.ppc;
        let main_epoch = x.main_epoch;
        let dpTrain = x.dpTrain;
        let inst = x.inst;
        let dInst = x.dInst;
        let arch_regs = x.regs;
        let cause = x.cause;
        if(verbose) $display("[doRenaming] mem inst: ", fshow(x));

        // update prev epoch
        epochManager.updatePrevEpoch[0].update(main_epoch);

        // get spec bits (should be 0), and no need to checkout spec tag
        let spec_bits = specTagManager.currentSpecBits;
        doAssert(spec_bits == 0, "cannot have spec bits");

        // do renaming (renaming is non-speculative)
        let rename_result = regRenamingTable.rename[0].getRename(arch_regs);
        let phy_regs = rename_result.phy_regs;
        regRenamingTable.rename[0].claimRename(arch_regs, spec_bits);

        // scoreboard lookup
        let regs_ready_cons = sbCons.eagerLookup[0].get(phy_regs);
        let regs_ready_aggr = sbAggr.eagerLookup[0].get(phy_regs);
        sbCons.setBusy[0].set(phy_regs.dst);
        sbAggr.setBusy[0].set(phy_regs.dst);

        // print rename info
        if (verbose) begin
            printRename(0, regs_ready_cons, regs_ready_aggr, arch_regs, phy_regs);
        end

        // get ROB tag
        let inst_tag = rob.enqPort[0].getEnqInstTag;

        // LSQ tag
        LdStQTag lsq_tag = ?;

        // send to MEM reservation station
        if (dInst.execFunc matches tagged Mem .mem_inst) begin
            Bool isLdQ = isLdQMemFunc(mem_inst.mem_func);
            Maybe#(LdStQTag) lsqEnqTag = isLdQ ? lsq.enqLdTag : lsq.enqStTag;
            if (lsqEnqTag matches tagged Valid .lsqTag) begin
                // can process, send to Mem rs and LSQ
                lsq_tag = lsqTag; // record LSQ tag
                if (dInst.iType != Fence) begin // Fence does not go to RS
                    reservationStationMem.enq(ToReservationStation {
                        data: MemRSData {
                            mem_func: mem_inst.mem_func,
                            imm: validValue(dInst.imm),
                            ldstq_tag: lsqTag
                        },
                        regs: phy_regs,
                        tag: inst_tag,
                        spec_bits: spec_bits,
                        spec_tag: Invalid,
                        regs_ready: regs_ready_aggr // mem currently recv bypass
                    });
                end
                doAssert(ppc == pc + 4, "Mem next PC is not PC+4");
                doAssert(!isValid(dInst.csr), "Mem never explicitly read/write CSR");
                doAssert((dInst.iType != Fence) == isValid(dInst.imm),
                         "Mem (non-Fence) needs imm for virtual addr");
                // put in ldstq
                if(isLdQ) begin
                    lsq.enqLd(inst_tag, mem_inst, phy_regs.dst, spec_bits);
                end
                else begin
                    lsq.enqSt(inst_tag, mem_inst, phy_regs.dst, spec_bits);
                end
            end
            else begin
                // cannot process this inst, stall
                when(False, noAction);
            end
        end
        else begin
            doAssert(False, "Must be mem inst");
        end

        // send to ROB
        Bool will_dirty_fpu_state = False;
        if (arch_regs.dst matches tagged Valid( tagged Fpu .r )) begin
            will_dirty_fpu_state = True;
        end
        RobInstState rob_inst_state = NotDone; // mem inst always needs execution
        let y = ToReorderBuffer{pc: pc,
                                iType: dInst.iType,
                                csr: dInst.csr,
                                claimed_phy_reg: True, // XXX we always claim a free reg in rename
                                trap: Invalid, // no trap
                                // default values of FullResult
                                ppc_vaddr_csrData: PPC (ppc), // default use PPC
                                fflags: 0,
                                ////////
                                will_dirty_fpu_state: will_dirty_fpu_state,
                                rob_inst_state: rob_inst_state,
                                lsqTag: lsq_tag,
                                ldKilled: Invalid,
                                memAccessAtCommit: False, // set by ROB in case of fence
                                lsqAtCommitNotified: False,
                                nonMMIOStDone: False,
                                epochIncremented: False,
                                spec_bits: spec_bits
                               };
        rob.enqPort[0].enq(y);

`ifdef CHECK_DEADLOCK
        renameCorrectPath.send;
`endif
    endrule
`endif

    // Count based scheduling in case of $n$ RS for the same inst type. We
    // assume all such RS are of the same size, and prioritize RS with smaller
    // valid (occupied) entries.
    function Maybe#(idxT) scheduleRS(
        Vector#(n, countT) valid_cnt, Vector#(n, Bool) rdy
    ) provisos(
        Ord#(countT), Alias#(idxT, Bit#(TLog#(n))), Add#(1, a__, n)
    );
        function Bit#(TLog#(n)) getRS(idxT a, idxT b);
            if(!rdy[a]) begin
                return b;
            end
            else if(!rdy[b]) begin
                return a;
            end
            else begin
                // prioritize RS with smaller valid-entry count
                return valid_cnt[a] < valid_cnt[b] ? a : b;
            end
        endfunction
        Vector#(n, idxT) idxVec = genWith(fromInteger);
        idxT idx = fold(getRS, idxVec);
        return rdy[idx] ? Valid (idx) : Invalid;
    endfunction

    // rename correct path inst
    rule doRenaming(
        !inIfc.pendingMMIOPRq // stall when MMIO pRq is pending
        && epochManager.checkEpoch[0].check(fetchStage.pipelines[0].first.main_epoch) // correct path
        && !isValid(firstTrap) // not trap
        && !firstReplay // not system inst
`ifdef SECURITY
        // stall for ROB empty if we don't allow speculation at all
        && (!specNone || rob.isEmpty)
        // don't process mem inst if we don't allow speculation for mem inst only
        && !(specNonMem && firstMem)
`endif
    );
        // we stop superscalar rename when an instruction cannot be processed:
        // (a) It has trap
        // (b) It is wrong path
        // (c) It is system inst (we handle system inst in a separate rule)
        // (d) It does not have enough resource
        Bool stop = False;
        // We automatically stop after an inst cannot be deq from fetch stage
        // because canDeq signal for sup-fifo is consecutive

        // Note that epoch will not change in this rule

        // track limited resource usage
        Vector#(AluExeNum, Bool) aluExeUsed = replicate(False);
        Vector#(FpuMulDivExeNum, Bool) fpuMulDivExeUsed = replicate(False);
        Bool memExeUsed = False;
        Bool specTagClaimed = False; // specTagManager

        // track rename activity
        Bool doCorrectPath = False;
        SupCnt renameCnt = 0;

        // initial spec bits at the beginning of this cycle
        // we may update it during the processing
        SpecBits spec_bits = specTagManager.currentSpecBits;

        // ALU RS valid counts
        Vector#(AluExeNum, Bit#(TLog#(TAdd#(`RS_ALU_SIZE, 1)))) aluRSCount;
        for(Integer i = 0; i < valueof(AluExeNum); i = i+1) begin
            aluRSCount[i] = reservationStationAlu[i].approximateCount;
        end
        // FPU/MUL/DIV RS valid counts
        Vector#(FpuMulDivExeNum, Bit#(TLog#(TAdd#(`RS_FPUMULDIV_SIZE, 1)))) fpuMulDivRSCount;
        for(Integer i = 0; i < valueof(FpuMulDivExeNum); i = i+1) begin
            fpuMulDivRSCount[i] = reservationStationFpuMulDiv[i].approximateCount;
        end

        // We apply actions at the end of each iteration
        // We **cannot** apply actions at the end of rule,
        // because intermediate iterations may change state
        for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
            if(!stop && fetchStage.pipelines[i].canDeq) begin
                let x = fetchStage.pipelines[i].first; // don't deq now, inst may not have resource
                let pc = x.pc;
                let ppc = x.ppc;
                let main_epoch = x.main_epoch;
                let dpTrain = x.dpTrain;
                let inst = x.inst;
                let dInst = x.dInst;
                let arch_regs = x.regs;
                let cause = x.cause;

                // check for wrong path, if wrong path, don't process it, leave to the other rule in next cycle
                if(!epochManager.checkEpoch[i].check(main_epoch)) begin
                    stop = True;
                end
                // for correct path
                // check ROB can be enq, otherwise cannot process
                if(!rob.enqPort[i].canEnq) begin
                    stop = True;
                end
                // check trap, if trap, cannot process, leave to the other rule in next cycle
                if(isValid(getTrap(x))) begin
                    stop = True;
                end
                // for system inst, process in next cycle (in a different rule)
                if(doReplay(dInst.iType)) begin
                    stop = True;
                end
`ifdef SECURITY
                // When speculation is not allowed at all, the second inst
                // cannot be processed
                if(specNone && i != 0) begin
                    stop = True;
                end
                // When speculation is not allowed for mem inst only, stop when
                // we seen any mem inst
                if(specNonMem && isMemInst(dInst.execFunc)) begin
                    stop = True;
                end
`endif
                // check renaming table can be enq, otherwise cannot process now
                if(!regRenamingTable.rename[i].canRename) begin
                    stop = True;
                end
                // Figure out if there is new speculation and if there is
                // speculative renaming happening.
                Bool new_speculation = False;
                Bool speculative_renaming = False; // deprecated: this is originally for Ld/St checkpoint
                if (dInst.execFunc matches tagged Br .br) begin
                    // This instruction can cause a redirection due to branch
                    // misprediction. Lets claim a checkpoint for this instruction.
                    // If this instruction is a JAL or JRAL instruction, then the
                    // checkpoint should include the renaming for the destination
                    // register.
                    new_speculation = True;
                    speculative_renaming = False;
                end
                // if need spec tag, check spec tag is available, otherwise cannot process
                if(new_speculation && (specTagClaimed || !specTagManager.canClaim)) begin
                    stop = True;
                end

                if(!stop) begin
                    // we can continue to analyze this inst
                    // Claim a speculation tag from the specTagManager if necessary
                    Maybe#(SpecTag) spec_tag = tagged Invalid;
                    if (new_speculation) begin
                        spec_tag = tagged Valid specTagManager.nextSpecTag;
                    end

                    // get renaming
                    // If the renaming is speculative, then the renaming will
                    // depend on the current spec_tag too.
                    let renaming_spec_bits = spec_bits | (speculative_renaming ? (1 << fromMaybe(?,spec_tag)) : 0);
                    let rename_result = regRenamingTable.rename[i].getRename(arch_regs);
                    let phy_regs = rename_result.phy_regs;

                    // scoreboard lookup
                    let regs_ready_cons = sbCons.eagerLookup[i].get(phy_regs);
                    let regs_ready_aggr = sbAggr.eagerLookup[i].get(phy_regs);

                    // get ROB tag
                    let inst_tag = rob.enqPort[i].getEnqInstTag;

                    // LSQ tag
                    LdStQTag lsq_tag = ?;

                    // check execution pipelines availability
                    // this determines whether this inst can finally be processed
                    // so we will directly take actions on exe pipelines
                    Bool to_exec = False;
                    Bool to_mem = False;
                    Bool to_FpuMulDiv = False;
                    case (dInst.execFunc) matches
                        tagged Alu .alu:        to_exec = True;
                        tagged Br .br:          to_exec = True;
                        tagged MulDiv .muldiv:  to_FpuMulDiv = True;
                        tagged Fpu .fpu:        to_FpuMulDiv = True;
                        tagged Mem .mem:        to_mem = True;
                        default:
                            // no need for execution, directly become Executed
                            noAction;
                    endcase

                    if (to_exec) begin
                        // find an ALU pipeline
                        function Bool aluValid(Integer k) = !aluExeUsed[k] && reservationStationAlu[k].canEnq;
                        Vector#(AluExeNum, Bool) aluReady = map(aluValid, genVector);
                        if(scheduleRS(aluRSCount, aluReady) matches tagged Valid .k) begin
                            // can process, send to ALU rs
                            aluExeUsed[k] = True; // mark resource used
                            reservationStationAlu[k].enq(ToReservationStation {
                                data: AluRSData {dInst: dInst, dpTrain: dpTrain},
                                regs: phy_regs,
                                tag: inst_tag,
                                spec_bits: spec_bits,
                                spec_tag: spec_tag,
                                regs_ready: regs_ready_aggr // alu will recv bypass
                            });
                        end
                        else begin
                            // cannot process this inst, stop
                            stop = True;
                        end
                    end
                    else if (to_FpuMulDiv) begin
                        function Bool fpuMulDivValid(Integer k) = !fpuMulDivExeUsed[k] && reservationStationFpuMulDiv[k].canEnq;
                        Vector#(FpuMulDivExeNum, Bool) fpuMulDivReady = map(fpuMulDivValid, genVector);
                        if(scheduleRS(fpuMulDivRSCount, fpuMulDivReady) matches tagged Valid .k) begin
                            // can process, send to FPU MUL DIV rs
                            fpuMulDivExeUsed[k] = True; // mark resource used
                            reservationStationFpuMulDiv[k].enq(ToReservationStation {
                                data: FpuMulDivRSData {execFunc: dInst.execFunc},
                                regs: phy_regs,
                                tag: inst_tag,
                                spec_bits: spec_bits,
                                spec_tag: spec_tag,
                                regs_ready: regs_ready_aggr // fpu mul div recv bypass
                            });
                            doAssert(ppc == pc + 4, "FpuMulDiv next PC is not PC+4");
                            doAssert(!isValid(dInst.csr), "FpuMulDiv never explicitly read/write CSR");
                            doAssert(!isValid(spec_tag), "should not have spec tag");
                        end
                        else begin
                            // cannot process this inst, stop
                            stop = True;
                        end
                    end
                    else if (to_mem) begin
                        if (dInst.execFunc matches tagged Mem .mem_inst) begin
                            Bool isLdQ = isLdQMemFunc(mem_inst.mem_func);
                            Maybe#(LdStQTag) lsqEnqTag = isLdQ ? lsq.enqLdTag : lsq.enqStTag;
                            if (!memExeUsed &&& reservationStationMem.canEnq &&&
                                lsqEnqTag matches tagged Valid .lsqTag) begin
                                // can process, send to Mem rs and LSQ
                                memExeUsed = True; // mark resource used
                                lsq_tag = lsqTag; // record LSQ tag
                                if (dInst.iType != Fence) begin // fence does not go to RS
                                    reservationStationMem.enq(ToReservationStation {
                                        data: MemRSData {
                                            mem_func: mem_inst.mem_func,
                                            imm: validValue(dInst.imm),
                                            ldstq_tag: lsqTag
                                        },
                                        regs: phy_regs,
                                        tag: inst_tag,
                                        spec_bits: spec_bits,
                                        spec_tag: spec_tag,
                                        regs_ready: regs_ready_aggr // mem currently recv bypass
                                    });
                                end
                                doAssert(ppc == pc + 4, "Mem next PC is not PC+4");
                                doAssert(!isValid(dInst.csr), "Mem never explicitly read/write CSR");
                                doAssert((dInst.iType != Fence) == isValid(dInst.imm),
                                         "Mem (non-Fence) needs imm for virtual addr");
                                doAssert(!isValid(spec_tag), "should not have spec tag");
                                // put in ldstq
                                if(isLdQ) begin
                                    lsq.enqLd(inst_tag, mem_inst, phy_regs.dst, spec_bits);
                                end
                                else begin
                                    lsq.enqSt(inst_tag, mem_inst, phy_regs.dst, spec_bits);
                                end
                            end
                            else begin
                                // cannot process this inst, stop
                                stop = True;
                            end
                        end
                        else begin
                            stop = True;
                            doAssert(False, "non memory instruction has to_mem == True");
                        end
                    end

                    // apply remaining actions if inst can be processed
                    if(!stop) begin
                        if(verbose) $display("[doRenaming - %d] ", i, fshow(x));

                        // deq fetch & update epochs match
                        fetchStage.pipelines[i].deq;
                        epochManager.updatePrevEpoch[i].update(main_epoch);
                        
                        // Claim a speculation tag
                        if (new_speculation) begin
                            specTagClaimed = True; // mark resource used
                            specTagManager.claimSpecTag;
                        end

                        // Do renaming
                        regRenamingTable.rename[i].claimRename(arch_regs, renaming_spec_bits);

                        // Scoreboard Operations
                        sbCons.setBusy[i].set(phy_regs.dst);
                        sbAggr.setBusy[i].set(phy_regs.dst);

                        // display information
                        if (verbose) begin
                            printRename(i, regs_ready_cons, regs_ready_aggr, arch_regs, phy_regs);
                        end

                        // Enqueue into reorder buffer
                        Bool will_dirty_fpu_state = False;
                        if (arch_regs.dst matches tagged Valid( tagged Fpu .r )) begin
                            will_dirty_fpu_state = True;
                        end
                        RobInstState rob_inst_state = (to_exec || to_mem || to_FpuMulDiv) ? NotDone : Executed;

                        let y = ToReorderBuffer{pc: pc,
                                                iType: dInst.iType,
                                                csr: dInst.csr,
                                                claimed_phy_reg: True, // XXX we always claim a free reg in rename
                                                trap: Invalid, // no trap
                                                // default values of FullResult
                                                ppc_vaddr_csrData: PPC (ppc), // default use PPC
                                                fflags: 0,
                                                ////////
                                                will_dirty_fpu_state: will_dirty_fpu_state,
                                                rob_inst_state: rob_inst_state,
                                                lsqTag: lsq_tag,
                                                ldKilled: Invalid,
                                                memAccessAtCommit: False, // set by ROB in case of fence
                                                lsqAtCommitNotified: False,
                                                nonMMIOStDone: False,
                                                epochIncremented: False,
                                                spec_bits: spec_bits
                                               };
                        rob.enqPort[i].enq(y);

                        // record activity
                        doCorrectPath = True;
                        renameCnt = renameCnt + 1;

                        // update spec bits if spec tag is claimed
                        if(spec_tag matches tagged Valid .t) begin
                            spec_bits = spec_bits | (1 << t);
                        end
                    end
                end
            end
        end

        // only fire this rule if we make some progress
        // otherwise this rule may block other rules forever
        when(doCorrectPath, noAction);

`ifdef PERF_COUNT
        if(inIfc.doStats) begin
            if(renameCnt > 1) begin
                supRenameCnt.incr(1);
            end
        end
`endif

`ifdef CHECK_DEADLOCK
        if(doCorrectPath) begin
            renameCorrectPath.send;
        end
`endif
    endrule


`ifdef CHECK_DEADLOCK
    interface renameInstStuck = toGet(renameInstStuckQ);
    interface renameCorrectPathStuck = toGet(renameCorrectPathStuckQ);
`else
    interface renameInstStuck = nullGet;
    interface renameCorrectPathStuck = nullGet;
`endif

    method Data getPerf(ExeStagePerfType t);
        return (case(t)
`ifdef PERF_COUNT
            SupRenameCnt: supRenameCnt;
`ifdef SECURITY
            SpecNoneCycles: specNoneCycles;
            SpecNonMemCycles: specNoneCycles;
`endif
`endif
            default: 0;
        endcase);
    endmethod
endmodule
