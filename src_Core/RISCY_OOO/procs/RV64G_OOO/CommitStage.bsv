
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
import ConfigReg::*;
import FIFO::*;
import Types::*;
import ProcTypes::*;
import CCTypes::*;
import Performance::*;
import ReorderBuffer::*;
import ReorderBufferSynth::*;
import RenamingTable::*;
import CsrFile::*;
import StoreBuffer::*;
import VerificationPacket::*;
import RenameDebugIF::*;

typedef struct {
    // info about the inst blocking at ROB head
    Addr pc;
    IType iType;
    Maybe#(Trap) trap;
    RobInstState state;
    Bool claimedPhyReg;
    Bool ldKilled;
    Bool memAccessAtCommit;
    Bool lsqAtCommitNotified;
    Bool nonMMIOStDone;
    Bool epochIncremented;
    SpecBits specBits;
    // info about LSQ/TLB
    Bool stbEmpty;
    Bool stqEmpty;
    Bool tlbNoPendingReq;
    // CSR info: previlige mode
    Bit#(2) prv;
    // inst count
    Data instCount;
} CommitStuck deriving(Bits, Eq, FShow);

interface CommitInput;
    // func units
    interface ReorderBufferSynth robIfc;
    interface RegRenamingTable rtIfc;
    interface CsrFile csrfIfc;
    // no stores
    method Bool stbEmpty;
    method Bool stqEmpty;
    // notify LSQ that inst has reached commit
    interface Vector#(SupSize, Put#(LdStQTag)) lsqSetAtCommit;
    // TLB has stopped processing now
    method Bool tlbNoPendingReq;
    // set flags
    method Action setFlushTlbs;
    method Action setUpdateVMInfo;
    method Action setFlushReservation;
    method Action setFlushBrPred; // security
    method Action setFlushCaches; // security
    method Action setReconcileI; // recocile I$
    method Action setReconcileD; // recocile D$
    // redirect
    method Action killAll;
    method Action redirectPc(Addr trap_pc);
    method Action setFetchWaitRedirect;
    method Action incrementEpoch;
    // record if we commit a CSR inst or interrupt
    method Action commitCsrInstOrInterrupt;
    // performance
    method Bool doStats;
    // deadlock check
    method Bool checkDeadlock;
endinterface

typedef struct {
    RenameError err;
    Addr pc;
    IType iType;
    Maybe#(Trap) trap;
    SpecBits specBits;
} RenameErrInfo deriving(Bits, Eq, FShow);

interface CommitStage;
    // performance
    method Data getPerf(ComStagePerfType t); 
    // deadlock check
    interface Get#(CommitStuck) commitInstStuck;
    interface Get#(CommitStuck) commitUserInstStuck;
    // rename debug
    method Action startRenameDebug;
    interface Get#(RenameErrInfo) renameErr;
endinterface

// we apply actions the end of commit rule
// use struct to record actions to be done
typedef struct {
    Addr pc;
    Addr addr;
    Trap trap;
} CommitTrap deriving(Bits, Eq, FShow);

module mkCommitStage#(CommitInput inIfc)(CommitStage);
    Bool verbose = True;

    // func units
    ReorderBufferSynth rob = inIfc.robIfc;
    RegRenamingTable regRenamingTable = inIfc.rtIfc;
    CsrFile csrf = inIfc.csrfIfc;

    // FIXME FIXME FIXME wires to set atCommit in LSQ: avoid scheduling cycle.
    // Using wire should be fine, because LSQ does not need to see atCommit
    // signal immediately. XXX The concern is about killAll which checks
    // atCommit in LSQ, but we never call killAll and setAtCommit in the same
    // cycle.
    Vector#(SupSize, RWire#(LdStQTag)) setLSQAtCommit <- replicateM(mkRWire);

    for(Integer i = 0; i< valueof(SupSize); i = i+1) begin
        (* fire_when_enabled, no_implicit_conditions *)
        rule doSetLSQAtCommit(setLSQAtCommit[i].wget matches tagged Valid .tag);
            inIfc.lsqSetAtCommit[i].put(tag);
        endrule
    end

    // commit stage performance counters
`ifdef PERF_COUNT
    // inst
    Count#(Data) instCnt <- mkCount(0);
    Count#(Data) userInstCnt <- mkCount(0);
    Count#(Data) supComUserCnt <- mkCount(0);
    // branch/jump inst
    Count#(Data) comBrCnt <- mkCount(0);
    Count#(Data) comJmpCnt <- mkCount(0);
    Count#(Data) comJrCnt <- mkCount(0);
    // mem inst
    Count#(Data) comLdCnt <- mkCount(0);
    Count#(Data) comStCnt <- mkCount(0);
    Count#(Data) comLrCnt <- mkCount(0);
    Count#(Data) comScCnt <- mkCount(0);
    Count#(Data) comAmoCnt <- mkCount(0);
    // load mispeculation
    Count#(Data) comLdKillByLdCnt <- mkCount(0);
    Count#(Data) comLdKillByStCnt <- mkCount(0);
    Count#(Data) comLdKillByCacheCnt <- mkCount(0);
    // exception/sys inst related
    Count#(Data) comSysCnt <- mkCount(0);
    Count#(Data) excepCnt <- mkCount(0);
    Count#(Data) interruptCnt <- mkCount(0);
    // flush tlb
    Count#(Data) flushTlbCnt <- mkCount(0);
    // flush security
    Count#(Data) flushSecurityCnt <- mkCount(0);
    Count#(Data) flushBPCnt <- mkCount(0);
    Count#(Data) flushCacheCnt <- mkCount(0);
`endif

    // deadlock check
`ifdef CHECK_DEADLOCK
    // timer to check deadlock
    Reg#(DeadlockTimer) commitInstTimer <- mkReg(0);
    Reg#(DeadlockTimer) commitUserInstTimer <- mkReg(0);
    // FIFOs to output deadlock info
    FIFO#(CommitStuck) commitInstStuckQ <- mkFIFO1;
    FIFO#(CommitStuck) commitUserInstStuckQ <- mkFIFO1;
    // wires to indicate that deadlock is reported, so reset timers
    PulseWire commitInstStuckSent <- mkPulseWire;
    PulseWire commitUserInstStuckSent <- mkPulseWire;
    // wires to reset timers since processor is making progress
    PulseWire commitInst <- mkPulseWire;
    PulseWire commitUserInst <- mkPulseWire;

    function CommitStuck commitStuck;
        let x = rob.deqPort[0].deq_data;
        return CommitStuck {
            pc: x.pc,
            iType: x.iType,
            trap: x.trap,
            state: x.rob_inst_state,
            claimedPhyReg: x.claimed_phy_reg,
            ldKilled: isValid(x.ldKilled),
            memAccessAtCommit: x.memAccessAtCommit,
            lsqAtCommitNotified: x.lsqAtCommitNotified,
            nonMMIOStDone: x.nonMMIOStDone,
            epochIncremented: x.epochIncremented,
            specBits: x.spec_bits,
            stbEmpty: inIfc.stbEmpty,
            stqEmpty: inIfc.stqEmpty,
            tlbNoPendingReq: inIfc.tlbNoPendingReq,
            prv: csrf.decodeInfo.prv,
            instCount: csrf.rd(CSRinstret)
        };
    endfunction

    (* fire_when_enabled *)
    rule checkDeadlock_commitInst(inIfc.checkDeadlock && commitInstTimer == maxBound);
        commitInstStuckQ.enq(commitStuck);
        commitInstStuckSent.send;
    endrule

    (* fire_when_enabled *)
    rule checkDeadlock_commitUserInst(inIfc.checkDeadlock && commitUserInstTimer == maxBound);
        commitUserInstStuckQ.enq(commitStuck);
        commitUserInstStuckSent.send;
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule incrDeadlockTimer(inIfc.checkDeadlock);
        function DeadlockTimer getNextTimer(DeadlockTimer t);
            return t == maxBound ? maxBound : t + 1;
        endfunction
        commitInstTimer <= (commitInst || commitInstStuckSent) ? 0 : getNextTimer(commitInstTimer);
        commitUserInstTimer <= (commitUserInst || commitUserInstStuckSent) ? 0 : getNextTimer(commitUserInstTimer);
    endrule
`endif

`ifdef RENAME_DEBUG
    // rename debug
    Reg#(Bool) renameDebugStarted <- mkConfigReg(False);
    Reg#(Maybe#(RenameErrInfo)) renameErrInfo <- mkConfigReg(Invalid);
    Bool canSetRenameErr = renameDebugStarted && renameErrInfo == Invalid; // only set err info once
    // only send 1 error msg
    FIFO#(RenameErrInfo) renameErrQ <- mkFIFO1;

    rule sendRenameErr(renameDebugStarted &&& renameErrInfo matches tagged Valid .info);
        renameErrQ.enq(info);
        renameDebugStarted <= False;
    endrule
`endif

    // we commit trap in two cycles: first cycle deq ROB and flush; second
    // cycle handles trap, redirect and handles system consistency
    Reg#(Maybe#(CommitTrap)) commitTrap <- mkReg(Invalid); // saves new pc here

    // maintain system consistency when system state (CSR) changes or for security
    function Action makeSystemConsistent(Bool flushTlb,
                                         Bool flushSecurity,
                                         Bool reconcileI);
    action
`ifndef SECURITY
        flushSecurity = False;
`endif

`ifndef DISABLE_SECURE_FLUSH_TLB
        if(flushTlb || flushSecurity) begin
`else
        if(flushTlb) begin
`endif
            inIfc.setFlushTlbs;
`ifdef PERF_COUNT
            if(inIfc.doStats) begin
                flushTlbCnt.incr(1);
            end
`endif
        end
        // notify TLB to keep update of CSR changes
        inIfc.setUpdateVMInfo;
        // always wait store buffer and SQ to be empty
        when(inIfc.stbEmpty && inIfc.stqEmpty, noAction);
        // We wait TLB to finish all requests and become sync with memory.
        // Notice that currently TLB is read only, so TLB is always in sync
        // with memory (i.e., there is no write to commit to memory). Since all
        // insts have been killed, nothing can be issued to D TLB at this time.
        // Since fetch stage is set to wait for redirect, fetch1 stage is
        // stalled, and nothing can be issued to I TLB at this time.
        // Therefore, we just need to make sure that I and D TLBs are not
        // handling any miss req. Besides, when I and D TLBs do not have any
        // miss req, L2 TLB must be idling.
        when(inIfc.tlbNoPendingReq, noAction);
        // yield load reservation in cache
        inIfc.setFlushReservation;

        // flush for security, we can delay the stall for fetch-empty and
        // wrong-path-load-empty until we really do the flush. This delay is
        // valid because these wrong path inst/req will not interfere with
        // whatever CSR changes we are making now.
        if(flushSecurity) begin
`ifdef PERF_COUNT
            if(inIfc.doStats) begin
                flushSecurityCnt.incr(1);
            end
`endif

`ifndef DISABLE_SECURE_FLUSH_BP
            inIfc.setFlushBrPred;
`ifdef PERF_COUNT
            if(inIfc.doStats) begin
                flushBPCnt.incr(1);
            end
`endif
`endif

`ifndef DISABLE_SECURE_FLUSH_CACHE
            inIfc.setFlushCaches;
`ifdef PERF_COUNT
            if(inIfc.doStats) begin
                flushCacheCnt.incr(1);
            end
`endif
`endif
        end

`ifdef SELF_INV_CACHE
        // reconcile I$
        if(reconcileI) begin
            inIfc.setReconcileI;
        end
`ifdef SYSTEM_SELF_INV_L1D
        // FIXME is this reconcile of D$ necessary?
        inIfc.setReconcileD;
`endif
`endif
    endaction
    endfunction

    // TODO Currently we don't check spec bits == 0 when we commit an
    // instruction. This is because killings of wrong path instructions are
    // done in a single cycle. However, when we make killings distributed or
    // pipelined, then we need to check spec bits at commit port.

    rule doCommitTrap_flush(
        !isValid(commitTrap) &&&
        rob.deqPort[0].deq_data.trap matches tagged Valid .trap
    );
        rob.deqPort[0].deq;
        let x = rob.deqPort[0].deq_data;
        if(verbose) $display("[doCommitTrap] ", fshow(x));

        // record trap info
        Addr vaddr = ?;
        if(x.ppc_vaddr_csrData matches tagged VAddr .va) begin
            vaddr = va;
        end
        commitTrap <= Valid (CommitTrap {
            trap: trap,
            pc: x.pc,
            addr: vaddr
        });

        // flush everything. Only increment epoch and stall fetch when we haven
        // not done it yet (we may have already done them at rename stage)
        inIfc.killAll;
        if(!x.epochIncremented) begin
            inIfc.incrementEpoch;
            inIfc.setFetchWaitRedirect;
        end

        // faulting mem inst may have claimed phy reg, we should not commit it;
        // instead, we kill the renaming by calling killAll

`ifdef PERF_COUNT
        // performance counter
        if(inIfc.doStats) begin
            if(trap matches tagged Exception .e) begin
                excepCnt.incr(1);
            end
            else begin
                interruptCnt.incr(1);
            end
        end
`endif

        // checks
        doAssert(x.rob_inst_state == Executed, "must be executed");
        doAssert(x.spec_bits == 0, "cannot have spec bits");
    endrule

    rule doCommitTrap_handle(commitTrap matches tagged Valid .trap);
        // reset commitTrap
        commitTrap <= Invalid;

        // notify commit of interrupt (so MMIO pRq may be handled)
        if(trap.trap matches tagged Interrupt .inter) begin
            inIfc.commitCsrInstOrInterrupt;
        end

        // trap handling & redirect
        let new_pc <- csrf.trap(trap.trap, trap.pc, trap.addr);
        inIfc.redirectPc(new_pc);

        // system consistency
        // TODO spike flushes TLB here, but perhaps it is because spike's TLB
        // does not include prv info, and it has to flush when prv changes.
        // XXX As approximation, Trap may cause context switch, so flush for
        // security
        makeSystemConsistent(False, True, False);
    endrule

    // commit misspeculated load
    rule doCommitKilledLd(
        !isValid(commitTrap) &&&
        !isValid(rob.deqPort[0].deq_data.trap) &&&
        rob.deqPort[0].deq_data.ldKilled matches tagged Valid .killBy
    );
        rob.deqPort[0].deq;
        let x = rob.deqPort[0].deq_data;
        if(verbose) $display("[doCommitKilledLd] ", fshow(x));

        // kill everything, redirect, and increment epoch
        inIfc.killAll;
        inIfc.redirectPc(x.pc);
        inIfc.incrementEpoch;

        // the killed Ld should have claimed phy reg, we should not commit it;
        // instead, we have kill the renaming by calling killAll

`ifdef PERF_COUNT
        if(inIfc.doStats) begin
            case(killBy)
                Ld: comLdKillByLdCnt.incr(1);
                St: comLdKillByStCnt.incr(1);
                Cache: comLdKillByCacheCnt.incr(1);
            endcase
        end
`endif

        // checks
        doAssert(!x.epochIncremented, "cannot increment epoch before");
        doAssert(x.rob_inst_state == Executed, "must be executed");
        doAssert(x.spec_bits == 0, "cannot have spec bits");
    endrule

    // commit system inst
    rule doCommitSystemInst(
        !isValid(commitTrap) &&
        !isValid(rob.deqPort[0].deq_data.trap) &&
        !isValid(rob.deqPort[0].deq_data.ldKilled) &&
        rob.deqPort[0].deq_data.rob_inst_state == Executed &&
        isSystem(rob.deqPort[0].deq_data.iType)
    );
        rob.deqPort[0].deq;
        let x = rob.deqPort[0].deq_data;
        if(verbose) $display("[doCommitSystemInst] ", fshow(x));

        // we claim a phy reg for every inst, so commit its renaming
        regRenamingTable.commit[0].commit;

        Bool write_satp = False; // flush tlb when satp csr is modified
        Bool flush_security = False; // flush for security when the flush csr is written
        if(x.iType == Csr) begin
            // notify commit of CSR (so MMIO pRq may be handled)
            inIfc.commitCsrInstOrInterrupt;
            // write CSR
            let csr_idx = validValue(x.csr);
            Data csr_data = ?;
            if(x.ppc_vaddr_csrData matches tagged CSRData .d) begin
                csr_data = d;
            end
            else begin
                doAssert(False, "must have csr data");
            end
            csrf.csrInstWr(csr_idx, csr_data);
            // check if satp is modified or not
            write_satp = csr_idx == CSRsatp;
`ifdef SECURITY
            flush_security = csr_idx == CSRmflush;
`endif
        end

        // redirect (Sret and Mret redirect pc is got from CSRF)
        Addr next_pc = x.ppc_vaddr_csrData matches tagged PPC .ppc ? ppc : (x.pc + 4);
        doAssert(next_pc == x.pc + 4, "ppc must be pc + 4");
        if(x.iType == Sret) begin
            next_pc <- csrf.sret;
        end
        else if(x.iType == Mret) begin
            next_pc <- csrf.mret;
        end
        inIfc.redirectPc(next_pc);

        // rename stage only sends out system inst when ROB is empty, so no
        // need to flush ROB again

        // system consistency
        // flush TLB for SFence.VMA and when SATP CSR is modified
        // XXX as approximation, sret/mret may mean context switch, so flush
        // for security
        makeSystemConsistent(
            x.iType == SFence || write_satp, // TODO flush TLB when change sanctum regs?
            flush_security || x.iType == Sret || x.iType == Mret,
            x.iType == FenceI // reconcile I$ for fence.i
        );

        // incr inst cnt
        csrf.incInstret(1);

`ifdef PERF_COUNT
        if(inIfc.doStats) begin
            comSysCnt.incr(1);
            // inst count stats
            instCnt.incr(1);
            if(csrf.decodeInfo.prv == 0) begin
                userInstCnt.incr(1);
            end
        end
`endif
`ifdef CHECK_DEADLOCK
        commitInst.send;
        if(csrf.decodeInfo.prv == 0) begin
            commitUserInst.send;
        end
`endif

        // checks
        doAssert(x.epochIncremented, "must have already incremented epoch");
        doAssert((x.iType == Csr) == isValid(x.csr), "only CSR has valid csr idx");
        doAssert(x.fflags == 0 && !x.will_dirty_fpu_state, "cannot dirty FPU");
        doAssert(x.spec_bits == 0, "cannot have spec bits");
        doAssert(x.claimed_phy_reg, "must have claimed phy reg");
`ifdef RENAME_DEBUG
        if(!x.claimed_phy_reg && canSetRenameErr) begin
            renameErrInfo <= Valid (RenameErrInfo {
                err: NonTrapCommitLackClaim,
                pc: x.pc,
                iType: x.iType,
                trap: x.trap,
                specBits: x.spec_bits
            });
        end
`endif
    endrule

    // Lr/Sc/Amo/MMIO cannot proceed to executed until we notify LSQ that it
    // has reached the commit stage
    rule notifyLSQCommit(
        !isValid(commitTrap) &&
        !isValid(rob.deqPort[0].deq_data.trap) &&
        !isValid(rob.deqPort[0].deq_data.ldKilled) &&
        rob.deqPort[0].deq_data.rob_inst_state != Executed &&
        rob.deqPort[0].deq_data.memAccessAtCommit &&
        !rob.deqPort[0].deq_data.lsqAtCommitNotified
    );
        let x = rob.deqPort[0].deq_data;
        let inst_tag = rob.deqPort[0].getDeqInstTag;
        if(verbose) $display("[notifyLSQCommit] ", fshow(x), "; ", fshow(inst_tag));

        // notify LSQ, and record in ROB that notification is done
        setLSQAtCommit[0].wset(x.lsqTag);
        rob.setLSQAtCommitNotified(inst_tag);
    endrule

    // commit normal: fire when at least one commit can be done
    rule doCommitNormalInst(
        !isValid(commitTrap) &&
        !isValid(rob.deqPort[0].deq_data.trap) &&
        !isValid(rob.deqPort[0].deq_data.ldKilled) &&
        rob.deqPort[0].deq_data.rob_inst_state == Executed &&
        !isSystem(rob.deqPort[0].deq_data.iType)
    );
        // stop superscalar commit after we
        // 1. see a trap or system inst or killed Ld
        // 2. inst is not ready to commit
        Bool stop = False;

        // We merge writes on FPU csr and apply writes at the end of the rule
        Bit#(5) fflags = 0;
        Bool will_dirty_fpu_state = False;
        // rename error
        Maybe#(RenameErrInfo) renameError = Invalid;
        // incr committed inst cnt at the end of rule
        SupCnt comInstCnt = 0;
        SupCnt comUserInstCnt = 0;
`ifdef PERF_COUNT
        // incr some performance counter at the end of rule
        SupCnt brCnt = 0;
        SupCnt jmpCnt = 0;
        SupCnt jrCnt = 0;
        SupCnt ldCnt = 0;
        SupCnt stCnt = 0;
        SupCnt lrCnt = 0;
        SupCnt scCnt = 0;
        SupCnt amoCnt = 0;
`endif

        // compute what actions to take
        for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
            if(!stop && rob.deqPort[i].canDeq) begin
                let x = rob.deqPort[i].deq_data;
                let inst_tag = rob.deqPort[i].getDeqInstTag;

                // check can be committed or not
                if(x.rob_inst_state != Executed || isValid(x.ldKilled) || isValid(x.trap) || isSystem(x.iType)) begin
                    // inst not ready for commit, or system inst, or trap, or killed, stop here
                    stop = True;
                end
                else begin
                    if (verbose) $display("[doCommitNormalInst - %d] ", i, fshow(inst_tag), " ; ", fshow(x));

                    // inst can be committed, deq it
                    rob.deqPort[i].deq;

                    // every inst here should have been renamed, commit renaming
                    regRenamingTable.commit[i].commit;
                    doAssert(x.claimed_phy_reg, "should have renamed");

`ifdef RENAME_DEBUG
                    // send debug msg for rename error
                    if(!x.claimed_phy_reg && !isValid(renameError)) begin
                        renameError = Valid (RenameErrInfo {
                            err: NonTrapCommitLackClaim,
                            pc: x.pc,
                            iType: x.iType,
                            trap: x.trap,
                            specBits: x.spec_bits
                        });
                    end
`endif

                    // cumulate writes to FPU csr
                    fflags = fflags | x.fflags;
                    will_dirty_fpu_state = will_dirty_fpu_state || x.will_dirty_fpu_state;

                    // for non-mmio st, notify SQ that store is committed
                    if(x.nonMMIOStDone) begin
                        setLSQAtCommit[i].wset(x.lsqTag);
                    end

                    // inst commit counter
                    comInstCnt = comInstCnt + 1;
                    if(csrf.decodeInfo.prv == 0) begin
                        comUserInstCnt = comUserInstCnt + 1; // user space inst
                    end

`ifdef PERF_COUNT
                    // performance counter
                    case(x.iType)
                        Br: brCnt = brCnt + 1;
                        J : jmpCnt = jmpCnt + 1;
                        Jr: jrCnt = jrCnt + 1;
                        Ld: ldCnt = ldCnt + 1;
                        St: stCnt = stCnt + 1;
                        Lr: lrCnt = lrCnt + 1;
                        Sc: scCnt = scCnt + 1;
                        Amo: amoCnt = amoCnt + 1;
                    endcase
`endif
                end
            end
        end

        // write FPU csr
        if(csrf.fpuInstNeedWr(fflags, will_dirty_fpu_state)) begin
            csrf.fpuInstWr(fflags);
        end

        // incr inst cnt
        csrf.incInstret(comInstCnt);

`ifdef RENAME_DEBUG
        // set rename error
        if(canSetRenameErr && isValid(renameError)) begin
            renameErrInfo <= renameError;
        end
`endif

`ifdef CHECK_DEADLOCK
        commitInst.send; // ROB head is removed
        if(comUserInstCnt > 0) begin
            commitUserInst.send;
        end
`endif

`ifdef PERF_COUNT
        // performance counter
        if(inIfc.doStats) begin
            // branch stats
            comBrCnt.incr(zeroExtend(brCnt));
            comJmpCnt.incr(zeroExtend(jmpCnt));
            comJrCnt.incr(zeroExtend(jrCnt));
            // mem stats
            comLdCnt.incr(zeroExtend(ldCnt));
            comStCnt.incr(zeroExtend(stCnt));
            comLrCnt.incr(zeroExtend(lrCnt));
            comScCnt.incr(zeroExtend(scCnt));
            comAmoCnt.incr(zeroExtend(amoCnt));
            // inst count stats
            instCnt.incr(zeroExtend(comInstCnt));
            userInstCnt.incr(zeroExtend(comUserInstCnt));
            if(comUserInstCnt > 1) begin
                supComUserCnt.incr(1);
            end
        end
`endif
    endrule


    method Data getPerf(ComStagePerfType t);
        return (case(t)
`ifdef PERF_COUNT
            InstCnt: instCnt;
            UserInstCnt: userInstCnt;
            SupComUserCnt: supComUserCnt;
            ComBrCnt: comBrCnt;
            ComJmpCnt: comJmpCnt;
            ComJrCnt: comJrCnt;
            ComLdCnt: comLdCnt;
            ComStCnt: comStCnt;
            ComLrCnt: comLrCnt;
            ComScCnt: comScCnt;
            ComAmoCnt: comAmoCnt;
            ComLdKillByLd: comLdKillByLdCnt;
            ComLdKillBySt: comLdKillByStCnt;
            ComLdKillByCache: comLdKillByCacheCnt;
            ComSysCnt: comSysCnt;
            ExcepCnt: excepCnt;
            InterruptCnt: interruptCnt;
            FlushTlbCnt: flushTlbCnt;
            FlushSecurityCnt: flushSecurityCnt;
            FlushBPCnt: flushBPCnt;
            FlushCacheCnt: flushCacheCnt;
`endif
            default: 0;
        endcase);
    endmethod

`ifdef CHECK_DEADLOCK
    interface commitInstStuck = toGet(commitInstStuckQ);
    interface commitUserInstStuck = toGet(commitUserInstStuckQ);
`else
    interface commitInstStuck = nullGet;
    interface commitUserInstStuck = nullGet;
`endif

`ifdef RENAME_DEBUG
    method Action startRenameDebug if(!renameDebugStarted);
        renameDebugStarted <= True;
    endmethod
    interface renameErr = toGet(renameErrQ);
`else
    method Action startRenameDebug;
        noAction;
    endmethod
    interface renameErr = nullGet;
`endif
endmodule
