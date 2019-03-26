
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
// ReorderBuffer
import Types::*;
import ProcTypes::*;
import HasSpecBits::*;
import Vector::*;
import Assert::*;
import Ehr::*;
import RevertingVirtualReg::*;

// right after execution, full_result has more up-to-date data (e.g. ppc of mispredicted branch)
// some parts of full_result are for verification
// but some are truly used for execution

// ppc is only used by iType = BR/J/JR
// csrData is only used by iType = Csr
// vaddr is only used by mem inst in page fault
typedef union tagged {
    Addr PPC; // at default store ppc
    Addr VAddr; // for mem inst, store vaddr
    Data CSRData; // for Csr inst, store csr_data
} PPCVAddrCSRData deriving(Bits, Eq, FShow);

typedef struct {
    Addr               pc;
    IType              iType;
    Maybe#(CSR)        csr;
    Bool               claimed_phy_reg; // whether we need to commmit renaming
    Maybe#(Trap)       trap;
    PPCVAddrCSRData    ppc_vaddr_csrData;
    Bit#(5)            fflags;
    Bool               will_dirty_fpu_state; // True means 2'b11 will be written to FS
    RobInstState       rob_inst_state; // was executed (i.e. can commit)
    LdStQTag           lsqTag; // tag for LSQ
    Maybe#(LdKilledBy) ldKilled; // mispeculative load + reason for the kill
    // Fence and some mem access are only performed at commit time, so ROB
    // should notify LSQ that the instrution arrives at commit stage and access
    // can start. XXX For fence, this bit is set at rename stage. For mem
    // accesses like Lr/Sc/Amo/MMIO, this bit is set by mem exe pipeline.
    Bool               memAccessAtCommit;
    // we have notified LSQ that inst is at commit
    Bool               lsqAtCommitNotified;
    // a successfully translated non-MMIO store needs ROB to notify the commit
    // from ROB, so that it can be dequeud from SQ
    Bool               nonMMIOStDone;
    // We detect some traps at rename stage, and increment epoch to kill
    // everything from fetch. So we should not increment epoch again when
    // committing the trap. Therefore we record the epoch increment at rename.
    Bool               epochIncremented;

    // speculation
    SpecBits           spec_bits;
} ToReorderBuffer deriving(Bits, Eq, FShow);

typedef enum {
    NotDone,
    Executed // i.e. ready to commit
} RobInstState deriving (Bits, Eq, FShow);

interface Row_setExecuted_doFinishAlu;
    method Action set(Maybe#(Data) csrData, ControlFlow cf);
endinterface

interface Row_setExecuted_doFinishFpuMulDiv;
    method Action set(Bit#(5) fflags);
endinterface

interface ReorderBufferRowEhr#(numeric type aluExeNum, numeric type fpuMulDivExeNum);
    method Action write_enq(ToReorderBuffer x);
    method ToReorderBuffer read_deq;
    method Action setLSQAtCommitNotified;
    // deqLSQ rules set ROB state: set execeptions, load mispeculation, and becomes Executed
    method Action setExecuted_deqLSQ(Maybe#(Exception) cause, Maybe#(LdKilledBy) ld_killed);
    // doFinish rules set ROB state for ALU and FPU/MUL/DIV (always become Executed)
    interface Vector#(aluExeNum, Row_setExecuted_doFinishAlu) setExecuted_doFinishAlu;
    interface Vector#(fpuMulDivExeNum, Row_setExecuted_doFinishFpuMulDiv) setExecuted_doFinishFpuMulDiv;
    // mem addr translation done: record virtual addr (for possible faults),
    // whether the access needs to be performed at commit stage (NOTE page
    // faulting inst cannot have this set, since there is no access to
    // perform), and non-MMIO St can become Executed (NOTE faulting
    // instructions are not Executed, they are set at deqLSQ time)
    method Action setExecuted_doFinishMem(Addr vaddr, Bool access_at_commit, Bool non_mmio_st_done);
`ifdef INORDER_CORE
    // in-order core sets LSQ tag after getting out of issue queue
    method Action setLSQTag(LdStQTag t, Bool isFence);
`endif
    // get original PC/PPC before execution, EHR port 0 will suffice
    method Addr getOrigPC;
    method Addr getOrigPredPC;
    // speculation
    method Bool dependsOn_wrongSpec(SpecTag tag);
    method Action correctSpeculation(SpecBits mask);
endinterface

module mkReorderBufferRowEhr(ReorderBufferRowEhr#(aluExeNum, fpuMulDivExeNum)) provisos(
    Add#(1, a__, aluExeNum), Add#(1, b__, fpuMulDivExeNum)
);
    Integer trap_deq_port = 0;
    Integer trap_deqLSQ_port = 0; // write trap
    Integer trap_finishMem_port = 1; // write trap
    Integer trap_enq_port = 2; // write trap

    Integer pvc_deq_port = 0;
    function Integer pvc_finishAlu_port(Integer i) = i; // write ppc_vaddr_csrData
    Integer pvc_finishMem_port = valueof(aluExeNum); // write ppc_vaddr_csrData
    Integer pvc_enq_port = 1 + valueof(aluExeNum); // write ppc_vaddr_csrData

    Integer fflags_deq_port = 0;
    function Integer fflags_finishFpuMulDiv_port(Integer i) = i; // write fflags
    Integer fflags_enq_port = valueof(fpuMulDivExeNum); // write fflags

    Integer ldKill_deq_port = 0;
    Integer ldKill_deqLSQ_port = 0; // set ldKilled
    Integer ldKill_enq_port = 1; // init ldKilled

    Integer accessCom_deq_port = 0;
    Integer accessCom_finishMem_port = 0; // set memAccessAtCommit
`ifdef INORDER_CORE
    Integer accessCom_setLSQTag_port = 1; // enq to LSQ
    Integer accessCom_enq_port = 2; // init
`else
    Integer accessCom_enq_port = 1; // init
`endif

    Integer lsqNotified_deq_port = 0;
    Integer lsqNotified_setNotified_port = 0; // set True
    Integer lsqNotified_enq_port = 1;

    Integer nonMMIOSt_deq_port = 0;
    Integer nonMMIOSt_finishMem_port = 0;
    Integer nonMMIOSt_enq_port = 1;

    Integer state_deq_port = 0;
    function Integer state_finishAlu_port(Integer i) = i; // write state
    function Integer state_finishFpuMulDiv_port(Integer i) = valueof(aluExeNum) + i; // write state
    Integer state_deqLSQ_port = valueof(fpuMulDivExeNum) + valueof(aluExeNum); // write state
    Integer state_finishMem_port = 1 + state_deqLSQ_port; // write state
    Integer state_enq_port = 1 + state_finishMem_port; // write state

    Integer sb_deq_port = 0;
    Integer sb_wrongSpec_port = 0;
    Integer sb_enq_port = 1; // write spec_bits
    Integer sb_correctSpec_port = 2; // write spec_bits

    Reg#(Addr)                                                      pc                   <- mkRegU;
    Reg#(IType)                                                     iType                <- mkRegU;
    Reg#(Maybe#(CSR))                                               csr                  <- mkRegU;
    Reg#(Bool)                                                      claimed_phy_reg      <- mkRegU;
    Ehr#(3, Maybe#(Trap))                                           trap                 <- mkEhr(?);
    Ehr#(TAdd#(2, aluExeNum), PPCVAddrCSRData)                      ppc_vaddr_csrData    <- mkEhr(?);
    Ehr#(TAdd#(1, fpuMulDivExeNum), Bit#(5))                        fflags               <- mkEhr(?);
    Reg#(Bool)                                                      will_dirty_fpu_state <- mkRegU;
    Ehr#(TAdd#(3, TAdd#(fpuMulDivExeNum, aluExeNum)), RobInstState) rob_inst_state       <- mkEhr(?);
    Reg#(LdStQTag)                                                  lsqTag               <- mkRegU;
    Ehr#(2, Maybe#(LdKilledBy))                                     ldKilled             <- mkEhr(?);
    Ehr#(3, Bool)                                                   memAccessAtCommit    <- mkEhr(?);
    Ehr#(2, Bool)                                                   lsqAtCommitNotified  <- mkEhr(?);
    Ehr#(2, Bool)                                                   nonMMIOStDone        <- mkEhr(?);
    Reg#(Bool)                                                      epochIncremented     <- mkRegU;
    Ehr#(3, SpecBits)                                               spec_bits            <- mkEhr(?);

    // wires to get stale (EHR port 0) values of PPC
    Wire#(Addr) predPcWire <- mkBypassWire;
    (* fire_when_enabled, no_implicit_conditions *)
    rule setPcWires;
        predPcWire <= ppc_vaddr_csrData[0] matches tagged PPC .a ? a : 0;
    endrule

    Vector#(aluExeNum, Row_setExecuted_doFinishAlu) aluSetExe;
    for(Integer i = 0; i < valueof(aluExeNum); i = i+1) begin
        aluSetExe[i] = (interface Row_setExecuted_doFinishAlu;
            method Action set(Maybe#(Data) csrData, ControlFlow cf);
                // inst is done
                rob_inst_state[state_finishAlu_port(i)] <= Executed; 
                // update PPC or csrData (vaddr is always useless for ALU results)
                if(csrData matches tagged Valid .d) begin
                    ppc_vaddr_csrData[pvc_finishAlu_port(i)] <= CSRData (d);
                end
                else begin
                    ppc_vaddr_csrData[pvc_finishAlu_port(i)] <= PPC (cf.nextPc);
                end
                doAssert(isValid(csr) == isValid(csrData), "csr valid should match");
            endmethod
        endinterface);
    end
    
    Vector#(fpuMulDivExeNum, Row_setExecuted_doFinishFpuMulDiv) fpuMulDivExe;
    for(Integer i = 0; i < valueof(fpuMulDivExeNum); i = i+1) begin
        fpuMulDivExe[i] = (interface Row_setExecuted_doFinishFpuMulDiv;
            method Action set(Bit#(5) new_fflags);
                // inst is done
                rob_inst_state[state_finishFpuMulDiv_port(i)] <= Executed; 
                // update fflags
                fflags[fflags_finishFpuMulDiv_port(i)] <= new_fflags;
            endmethod
        endinterface);
    end

    method Addr getOrigPC = pc;
    method Addr getOrigPredPC = predPcWire;

    interface setExecuted_doFinishAlu = aluSetExe;

    interface setExecuted_doFinishFpuMulDiv = fpuMulDivExe;

    method Action setExecuted_doFinishMem(Addr vaddr, Bool access_at_commit, Bool non_mmio_st_done);
        doAssert(!(access_at_commit && non_mmio_st_done),
                 "cannot both be true");
        // update ROB state
        if(non_mmio_st_done) begin
            rob_inst_state[state_finishMem_port] <= Executed;
            doAssert(iType == St, "must be St");
        end
        // update VAddr
        ppc_vaddr_csrData[pvc_finishMem_port] <= VAddr (vaddr);
        // update access at commit
        memAccessAtCommit[accessCom_finishMem_port] <= access_at_commit;
        // udpate non mmio st
        nonMMIOStDone[nonMMIOSt_finishMem_port] <= non_mmio_st_done;
    endmethod

`ifdef INORDER_CORE
    method Action setLSQTag(LdStQTag t, Bool isFence);
        lsqTag <= t;
        memAccessAtCommit[accessCom_setLSQTag_port] <= isFence;
        doAssert(isFence == (iType == Fence), "fence should match");
    endmethod
`endif

    method Action write_enq(ToReorderBuffer x);
        pc <= x.pc;
        iType <= x.iType;
        csr <= x.csr;
        claimed_phy_reg <= x.claimed_phy_reg;
        trap[trap_enq_port] <= x.trap;
        ppc_vaddr_csrData[pvc_enq_port] <= x.ppc_vaddr_csrData;
        fflags[fflags_enq_port] <= x.fflags;
        will_dirty_fpu_state <= x.will_dirty_fpu_state;
        rob_inst_state[state_enq_port] <= x.rob_inst_state;
        epochIncremented <= x.epochIncremented;
        spec_bits[sb_enq_port] <= x.spec_bits;
`ifdef INORDER_CORE
        // in-order core enqs to LSQ later, so don't set LSQ tag; and other
        // flags should default to false
        memAccessAtCommit[accessCom_enq_port] <= False;
`else
        lsqTag <= x.lsqTag;
        memAccessAtCommit[accessCom_enq_port] <= x.iType == Fence;
`endif
        ldKilled[ldKill_enq_port] <= Invalid;
        lsqAtCommitNotified[lsqNotified_enq_port] <= False;
        nonMMIOStDone[nonMMIOSt_enq_port] <= False;
        // check
        doAssert(!isValid(x.ldKilled), "ld killed must be false");
        doAssert(x.memAccessAtCommit == False, "mem access at commit must be false");
        doAssert(!x.lsqAtCommitNotified, "lsq notified must be false");
        doAssert(!x.nonMMIOStDone, "non mmio st must be false");
    endmethod

    method ToReorderBuffer read_deq;
        return ToReorderBuffer {
            pc: pc,
            iType: iType,
            csr: csr,
            claimed_phy_reg: claimed_phy_reg,
            trap: trap[trap_deq_port],
            ppc_vaddr_csrData: ppc_vaddr_csrData[pvc_deq_port],
            fflags: fflags[fflags_deq_port],
            will_dirty_fpu_state: will_dirty_fpu_state,
            rob_inst_state: rob_inst_state[state_deq_port],
            lsqTag: lsqTag,
            ldKilled: ldKilled[ldKill_deq_port],
            memAccessAtCommit: memAccessAtCommit[accessCom_deq_port],
            lsqAtCommitNotified: lsqAtCommitNotified[lsqNotified_deq_port],
            nonMMIOStDone: nonMMIOStDone[nonMMIOSt_deq_port],
            epochIncremented: epochIncremented,
            spec_bits: spec_bits[sb_deq_port]
        };
    endmethod

    method Action setLSQAtCommitNotified;
        lsqAtCommitNotified[lsqNotified_setNotified_port] <= True;
    endmethod

    method Action setExecuted_deqLSQ(Maybe#(Exception) cause, Maybe#(LdKilledBy) ld_killed);
        // inst becomes Executed
        rob_inst_state[state_deqLSQ_port] <= Executed;
        // record trap
        doAssert(!isValid(trap[trap_deqLSQ_port]), "cannot have trap");
        if(cause matches tagged Valid .e) begin
            trap[trap_deqLSQ_port] <= Valid (Exception (e));
        end
        // record ld misspeculation
        ldKilled[ldKill_deqLSQ_port] <= ld_killed;
    endmethod

    method Bool dependsOn_wrongSpec(SpecTag tag);
        return spec_bits[sb_wrongSpec_port][tag] == 1;
    endmethod

    method Action correctSpeculation(SpecBits mask);
        SpecBits sb = spec_bits[sb_correctSpec_port];
        spec_bits[sb_correctSpec_port] <= sb & mask;
    endmethod
endmodule

interface ROB_SpeculationUpdate;
    // when killing wrong path inst, we directly move enqP to inst_tag + 1
    // assumption is that any entry within (inst_tag, enqP) will be killed
    // (i.e. any such entry has spec_bits[spec_tag] == 1, so valid bit is reset)
    // notice that inst_tag itself may be killed! (e.g. a Ld killed by older Ld/St)
    // also note that inst_tag itself may be already dequeued just in this cycle
    method Action incorrectSpeculation(Bool kill_all, SpecTag spec_tag, InstTag inst_tag);
    method Action correctSpeculation(SpecBits mask);
endinterface

////////////////////////////////////////////////////////
////////// Superscalar ehrized reorder buffer //////////
////////////////////////////////////////////////////////

interface ROB_EnqPort;
    method Bool canEnq;
    method Action enq(ToReorderBuffer x);
    method InstTag getEnqInstTag;
endinterface

interface ROB_DeqPort;
    method Bool canDeq;
    method Action deq;
    method InstTag getDeqInstTag;
    method ToReorderBuffer deq_data;
endinterface

// XXX guards of enq and deq ifc do not check that enq and deq are done consecutively
// This is the responsibility of outside world

// XXX enq and deq becomes a vector of interfaces
// We do not make them a single method with a vector as input,
// because synth boundary will make the guard conservative (i.e. ignoring input)
// thus requiring all ways to be able to enq/deq, which may cause deadlock
// However, synth boundary is needed to keep the atomicity of methods
// Having a vector of interfaces makes compiler unable to detect that different enq/deq
// interfaces are actually accessing different ways
// We have to use wires to perform the real actions in one rule so that compiler will
// not raise false conflicts between the superscalar enq/deq actions

interface ROB_setExecuted_doFinishAlu;
    method Action set(InstTag x, Maybe#(Data) csrData, ControlFlow cf);
endinterface

interface ROB_setExecuted_doFinishFpuMulDiv;
    method Action set(InstTag x, Bit#(5) fflags);
endinterface

interface ROB_getOrigPC;
    method Addr get(InstTag x);
endinterface

interface ROB_getOrigPredPC;
    method Addr get(InstTag x);
endinterface

interface SupReorderBuffer#(numeric type aluExeNum, numeric type fpuMulDivExeNum);
    interface Vector#(SupSize, ROB_EnqPort) enqPort;
    method Bool isEmpty; // empty signal for enq port (for FENCE/System inst etc.)

    interface Vector#(SupSize, ROB_DeqPort) deqPort;

    // record that we have notified LSQ about inst reaching commit 
    method Action setLSQAtCommitNotified(InstTag x);
    // deqLSQ rules set ROB state
    method Action setExecuted_deqLSQ(InstTag x, Maybe#(Exception) cause, Maybe#(LdKilledBy) ld_killed);
    // doFinish rules set ROB state in ALU and FPU/MUL/DIV
    interface Vector#(aluExeNum, ROB_setExecuted_doFinishAlu) setExecuted_doFinishAlu;
    interface Vector#(fpuMulDivExeNum, ROB_setExecuted_doFinishFpuMulDiv) setExecuted_doFinishFpuMulDiv;
    // doFinishMem, after addr translation
    method Action setExecuted_doFinishMem(InstTag x, Addr vaddr, Bool access_at_commit, Bool non_mmio_st_done);
`ifdef INORDER_CORE
    // in-order core sets LSQ tag after getting out of issue queue
    method Action setLSQTag(InstTag x, LdStQTag t, Bool isFence);
`endif

    // get original PC/PPC before execution, EHR port 0 will suffice
    interface Vector#(TAdd#(1, aluExeNum), ROB_getOrigPC) getOrigPC;
    interface Vector#(aluExeNum, ROB_getOrigPredPC) getOrigPredPC;

    // get enq time for reservation station dispatch
    method InstTime getEnqTime;

    method Bool isEmpty_ehrPort0;
    method Bool isFull_ehrPort0;

    interface ROB_SpeculationUpdate specUpdate;
endinterface

typedef struct {
    Bool killAll;
    // below are only meaningful when killAll is False
    SpecTag specTag;
    InstTag killInstTag;
} ROBWrongSpecInput deriving(Bits, Eq, FShow);

typedef struct {
    SupWaySel firstEnqWay;
    Vector#(SupSize, SingleScalarPtr) enqP;
    InstTime enqTime;
} ROBWrongSpecEnqUpdate deriving(Bits, Eq, FShow);

module mkSupReorderBuffer#(
    Bool lazyEnq,
    module#(ReorderBufferRowEhr#(aluExeNum, fpuMulDivExeNum)) mkRobRow
)(SupReorderBuffer#(aluExeNum, fpuMulDivExeNum)) provisos(
    Add#(TExp#(TLog#(SupSize)), 0, SupSize), // require SupSize to be power of 2
    Add#(1, a__, aluExeNum), Add#(1, b__, fpuMulDivExeNum)
);
    // doCommit rule: deq < wrongSpec (overwrite deq in doCommit) < doRenaming rule: enq
    Integer valid_deq_port = 0;
    Integer valid_wrongSpec_port = 1;
    Integer valid_enq_port = 1;

    // doFinishXXX, doDeqLSQ_XXX: setExecute_XXX, correctSpeculation
    // these are handled in mkReorderBufferRowEhr

    // wrong speculation: make wrong speculation conflict with enq
    Vector#(SupSize, RWire#(void)) wrongSpec_enq_conflict <- replicateM(mkRWire);

    // SupSize number of FIFOs
    Vector#(SupSize, Vector#(SingleScalarSize, ReorderBufferRowEhr#(aluExeNum, fpuMulDivExeNum))) row <- replicateM(replicateM(mkRobRow));
    Vector#(SupSize, Vector#(SingleScalarSize, Ehr#(2, Bool))) valid <- replicateM(replicateM(mkEhr(False)));
    Vector#(SupSize, Reg#(SingleScalarPtr)) enqP <- replicateM(mkReg(0));
    Vector#(SupSize, Ehr#(2, SingleScalarPtr)) deqP_ehr <- replicateM(mkEhr(0));
    let deqP = getVEhrPort(deqP_ehr, 0);
    let deqP_wrongSpec = getVEhrPort(deqP_ehr, 1); // for overwrite deqP when killing all

    // enq/deq port will operate on above FIFOs in a rotating manner
    // We distinguish between enq/deq port and FIFO way
    // FIFO way is the selection of the static FIFO array
    // enq/deq port is the port selection exposed to the outside world
    // (e.g. enq/deq port 0 is for oldest inst in program order)
    // The mapping of enq/deq ports to FIFO ways changes dynamically (i.e. rotating)

    // firstEnq/DeqWay: which FIFO of row, valid, etc. that enq/deq port 0 should use
    Reg#(SupWaySel) firstEnqWay <- mkReg(0);
    Ehr#(2, SupWaySel) firstDeqWay_ehr <- mkEhr(0);
    Reg#(SupWaySel) firstDeqWay = firstDeqWay_ehr[0];
    Reg#(SupWaySel) firstDeqWay_wrongSpec = firstDeqWay_ehr[1];

    // time of inst: enq & deq ptr as if ROB is just a FIFO of size 2^log(NumInstTag)
    Reg#(InstTime) enqTime <- mkReg(0);
    Ehr#(2, InstTime) deqTime_ehr <- mkEhr(0);
    Reg#(InstTime) deqTime = deqTime_ehr[0];
    Reg#(InstTime) deqTime_wrongSpec = deqTime_ehr[1]; // for overwrite deqTime when killing all

    // wires for recording actions on enq & deq ports
    Vector#(SupSize, RWire#(ToReorderBuffer)) enqEn <- replicateM(mkRWire);
    Vector#(SupSize, PulseWire) deqEn <- replicateM(mkPulseWire);
    // wire for recording action of wrongSpec
    RWire#(ROBWrongSpecInput) wrongSpecEn <- mkRWire;

    // ordering regs: deq sequence < setExecuted_XXX is maintained by each row
    // BUT setExecuted_XXX, setLSQAtCommitNotified < enq, deq < enq, and deq <
    // wrongSpec NEEDs explicit ordering
    Reg#(Bool) deq_SB_wrongSpec <- mkRevertingVirtualReg(True);
    Vector#(SupSize, Reg#(Bool)) deq_SB_enq <- replicateM(mkRevertingVirtualReg(True));
    Vector#(SupSize, Reg#(Bool)) setExeAlu_SB_enq <- replicateM(mkRevertingVirtualReg(True));
    Vector#(SupSize, Reg#(Bool)) setExeMem_SB_enq <- replicateM(mkRevertingVirtualReg(True));
    Vector#(SupSize, Reg#(Bool)) setExeLSQ_SB_enq <- replicateM(mkRevertingVirtualReg(True));
    Vector#(SupSize, Reg#(Bool)) setExeFpuMulDiv_SB_enq <- replicateM(mkRevertingVirtualReg(True));
    Vector#(SupSize, Reg#(Bool)) setNotified_SB_enq <- replicateM(mkRevertingVirtualReg(True));

    function SingleScalarPtr getNextPtr(SingleScalarPtr p);
        return p == fromInteger(valueOf(SingleScalarSize)-1) ? 0 : p + 1;
    endfunction

    // convert enq/deq port -> fifo way
    function SupWaySel getEnqFifoWay(SupWaySel enqPort) = firstEnqWay + enqPort;
    function SupWaySel getDeqFifoWay(SupWaySel deqPort) = firstDeqWay + deqPort;
    // convert fifo way -> enq/deq port
    function SupWaySel getEnqPort(SupWaySel fifoWay) = fifoWay - firstEnqWay;
    function SupWaySel getDeqPort(SupWaySel fifoWay) = fifoWay - firstDeqWay;
    // XXX above 4 functions require SupSize to be power of 2

    // do deq & update firstDeqWay
    (* fire_when_enabled, no_implicit_conditions *)
    rule canon_deq;
        // -- apply effects of deq --
        for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
            // for FIFO way i (looping for FIFO way should save area than looping for deq port)
            SupWaySel deqPort = getDeqPort(fromInteger(i));
            doAssert(getDeqFifoWay(deqPort) == fromInteger(i), "deq port matches FIFO way");
            if(deqEn[deqPort]) begin
                doAssert(valid[i][deqP[i]][valid_deq_port], "deq entry must be valid");
                // move deqP & reset valid
                deqP[i] <= getNextPtr(deqP[i]);
                valid[i][deqP[i]][valid_deq_port] <= False;
            end
        end
        // update firstDeqWay: find the first deq port that is not enabled
        Vector#(SupSize, SupWaySel) idxVec = genWith(fromInteger);
        function Bool notDeq(SupWaySel i);
            return !deqEn[i];
        endfunction
        if(find(notDeq, idxVec) matches tagged Valid .idx) begin
            // idx is the first port that does not deq
            // update firstWay (XXX we require SupSize to be power of 2)
            firstDeqWay <= firstDeqWay + idx;
            // update deq day
            deqTime <= deqTime + zeroExtend(idx);
            // sanity check: deq ports[idx..max] are not enabled
            for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
                doAssert((fromInteger(i) < idx) == deqEn[i], "Deq must be consective");
            end
        end
        else begin
            // all ports are dequeued, so firstWay keeps the same
            // update deq day
            deqTime <= deqTime + fromInteger(valueof(SupSize));
        end
    endrule

    // process wrongSpec: clear valid bits & compute update for enq regs
    (* fire_when_enabled, no_implicit_conditions *)
    rule canon_wrongSpec(wrongSpecEn.wget matches tagged Valid .x);
        if(x.killAll) begin
            // kill everything
            for(Integer w = 0; w < valueof(SupSize); w = w+1) begin
                for(Integer i = 0; i < valueof(SingleScalarSize); i = i+1) begin
                    valid[w][i][valid_wrongSpec_port] <= False;
                end
            end
            // reset all ptrs to 0
            for(Integer w = 0; w < valueof(SupSize); w = w+1) begin
                enqP[w] <= 0;
                deqP_wrongSpec[w] <= 0;
            end
            firstEnqWay <= 0;
            firstDeqWay_wrongSpec <= 0;
            enqTime <= 0;
            deqTime_wrongSpec <= 0;
        end
        else begin
            SpecTag specTag = x.specTag;
            InstTag killInstTag = x.killInstTag;

            // only update valid, no need to change spec bits
            for(Integer w = 0; w < valueof(SupSize); w = w+1) begin
                for(Integer i = 0; i < valueof(SingleScalarSize); i = i+1) begin
                    if(row[w][i].dependsOn_wrongSpec(specTag)) begin
                        valid[w][i][valid_wrongSpec_port] <= False;
                    end
                end
            end

            // move enqP to be right after (or just) the inst that initiates the kill
            // To do this, we need to figure out the number of inst killed in each FIFO way
            // Notice that each FIFO way is enq in round-robin order
            // Since we know the number of killed inst in the FIFO that contains the kill-initiating inst,
            // we can deduce the number of killed inst in other FIFOs
            // For simplicity, we map the way select of FIFOs to a virtual way select
            // such that firstEnqWay will be 0 in the virtual way select
            // i.e. virtual way = way - firstEnqWay XXX requires SupSize to be power of 2
            function SupWaySel toVirtualWay(SupWaySel realWay) = realWay - firstEnqWay;
            // In terms of virtual ways, if we align the enqP of all FIFOs,
            // we know the program order of inst when we move the ptr backward from enqP
            // Thus we can deduce how many inst are killed in each FIFO
            // First consider the FIFO that contains the kill-initiating entry
            // We get virtual way and the distance from kill-initiating entry to enqP
            // With this distance, we can deduce how much we should decrement the enqP for all FIFOs
            SupWaySel virtualKillWay = toVirtualWay(killInstTag.way);
            SingleScalarPtr killEnqP = enqP[killInstTag.way];
            SingleScalarLen killDistToEnqP = killInstTag.ptr < killEnqP ? // if >=, then FIFO must wrap around
                zeroExtend(killEnqP - killInstTag.ptr) :
                zeroExtend(killEnqP) + fromInteger(valueof(SingleScalarSize)) - zeroExtend(killInstTag.ptr);
            doAssert(killDistToEnqP > 0, "distance to enqP must be > 0");
            // helper function to decr enqP
            function SingleScalarPtr decrPtr(SingleScalarPtr ptr, SingleScalarLen len);
                if(zeroExtend(ptr) < len) begin
                    SingleScalarLen extendedPtr = zeroExtend(ptr) + fromInteger(valueof(SingleScalarSize));
                    return truncate(extendedPtr - len);
                end
                else begin
                    return ptr - truncate(len);
                end
            endfunction
            // the kill-initiating inst should not kill itself
            // (in fact it must be a branch, Ld mis-speculation is handled by kill all at commit stage)
            // so the ROB entry entry right after the kill-initiating entry should be the enq position
            // it must be in the way right after the way that contians the kill-initiating entry
            SupWaySel firstEnqWayNext = killInstTag.way + 1;
            InstTime enqTimeNext = killInstTag.t + 1;
            Vector#(SupSize, SingleScalarPtr) enqPNext;
            Vector#(SupSize, SingleScalarLen) distToEnqP; // amount to decr enqP to get enqPNext, record for debugging
            for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
                // consider virtual way of FIFO i, and get the distance that enqP should decr
                // virtual way >  kill virtual way: enqP decr by killDistToEnqP
                // virtual way <= kill virtual way: enqP decr by killDistToEnqP - 1
                SupWaySel virtualWay = toVirtualWay(fromInteger(i));
                distToEnqP[i] = virtualWay > virtualKillWay ? killDistToEnqP : killDistToEnqP - 1;
                enqPNext[i] = decrPtr(enqP[i], distToEnqP[i]);
            end

            // state update
            firstEnqWay <= firstEnqWayNext;
            enqTime <= enqTimeNext;
            for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
                enqP[i] <= enqPNext[i];
            end

            // check kill-initiating inst not killing itself
            Bool killSelf = valid[killInstTag.way][killInstTag.ptr][valid_wrongSpec_port] &&
                            row[killInstTag.way][killInstTag.ptr].dependsOn_wrongSpec(specTag);
            doAssert(!killSelf, "cannot kill itself");
            //if(killSelf) begin
            //    // the kill-initiating inst also kills itself (e.g. a Ld)
            //    // so the kill-initiating entry becomes the next enq position
            //    // first enq way will become the way that contains the kill-initialting entry
            //    firstEnqWayNext = killInstTag.way;
            //    enqTimeNext = killInstTag.t;
            //    // get the enq pointers for all FIFOs
            //    for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
            //        // consider virtual way of FIFO i, and get the distance that enqP should decr
            //        // virtual way >= kill virtual way: enqP decr by killDistToEnqP
            //        // virtual way <  kill virtual way: enqP decr by killDistToEnqP - 1
            //        SupWaySel virtualWay = toVirtualWay(fromInteger(i));
            //        distToEnqP[i] = virtualWay >= virtualKillWay ? killDistToEnqP : killDistToEnqP - 1;
            //        enqPNext[i] = decrPtr(enqP[i], distToEnqP[i]);
            //    end
            //end

            // wrong spec is conflicting with enq, so enqEn must be all false
            for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
                doAssert(!isValid(enqEn[i].wget), "when wrongSpec, enq cannot fire");
            end

`ifdef BSIM
            // sanity check in simulation
            Vector#(SupSize, SingleScalarPtr) deqPVec = readVReg(deqP_wrongSpec);
            Vector#(SupSize, Vector#(SingleScalarSize, Bool)) depVec;
            Vector#(SupSize, Vector#(SingleScalarSize, Bool)) validVec;
            for(Integer w = 0; w < valueof(SupSize); w = w+1) begin
                validVec[w] = readVEhr(valid_wrongSpec_port, valid[w]);
                function Bool getDepOn(Integer i) = row[w][i].dependsOn_wrongSpec(specTag);
                depVec[w] = map(getDepOn, genVector);
            end
            $display("[ROB incorrectSpec] ",
                fshow(specTag), " ; ",
                fshow(killInstTag), " ; ",
                fshow(firstEnqWay), " ; ",
                fshow(firstDeqWay_wrongSpec), " ; ",
                fshow(readVReg(enqP)), " ; ",
                fshow(deqPVec), " ; ",
                fshow(validVec), " ; ",
                fshow(depVec), " ; ",
                fshow(firstEnqWayNext), " ; ",
                fshow(enqPNext), " ; ",
                fshow(distToEnqP)
            );
            // valid entries within [enqPNext, enqP) are killed
            for(Integer w = 0; w < valueof(SupSize); w = w+1) begin
                function Bool in_kill_range(SingleScalarPtr i);
                    if(distToEnqP[w] == 0) begin
                        return False;
                    end
                    else begin
                        if(enqPNext[w] < enqP[w]) begin
                            return enqPNext[w] <= i && i < enqP[w];
                        end
                        else begin
                            return enqPNext[w] <= i || i < enqP[w];
                        end
                    end
                endfunction
                for(Integer i = 0; i < valueof(SingleScalarSize); i = i+1) begin
                    doAssert(
                        in_kill_range(fromInteger(i)) ==
                        (row[w][i].dependsOn_wrongSpec(specTag) && valid[w][i][valid_wrongSpec_port]),
                        "valid entries inside [enqPNext, enqP) must be killed, outsiders must not"
                    );
                end
            end
            // kill-initiating entry may be just dequeued
            if(!valid[killInstTag.way][killInstTag.ptr][valid_wrongSpec_port]) begin
                doAssert(getNextPtr(killInstTag.ptr) == deqPVec[killInstTag.way],
                    "if the kill-initiating entry is invalid, it must be just dequeued"
                );
            end
`endif
        end
    endrule

    // Apply enq effects. This rule cannot be merged with canon_deq, because
    // many other methods access ROB row contents are sandwiched between
    // canon_deq and this rule.
    (* fire_when_enabled, no_implicit_conditions *)
    rule canon_enq(!isValid(wrongSpecEn.wget));
        for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
            // for FIFO way i (looping for FIFO way should save area than looping for enq port)
            SupWaySel enqPort = getEnqPort(fromInteger(i));
            doAssert(getEnqFifoWay(enqPort) == fromInteger(i), "enq port matches FIFO way");
            if(enqEn[enqPort].wget matches tagged Valid .x) begin
                doAssert(!valid[i][enqP[i]][valid_enq_port], "enq entry must be invalid");
                // update row, set valid, move enqP
                enqP[i] <= getNextPtr(enqP[i]);
                row[i][enqP[i]].write_enq(x);
                valid[i][enqP[i]][valid_enq_port] <= True;
            end
        end
        // update firstEnqWay: find the first enq port that is not enabled
        Vector#(SupSize, SupWaySel) idxVec = genWith(fromInteger);
        function Bool notEnq(SupWaySel i);
            return !isValid(enqEn[i].wget);
        endfunction
        if(find(notEnq, idxVec) matches tagged Valid .idx) begin
            // idx is the first port that does not enq
            // update firstWay (XXX we require SupSize to be power of 2)
            firstEnqWay <= firstEnqWay + idx;
            // update enq day
            enqTime <= enqTime + zeroExtend(idx);
            // sanity check: enq ports[idx..max] are not enabled
            for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
                doAssert((fromInteger(i) < idx) == isValid(enqEn[i].wget), "Enq must be consecutive");
            end
        end
        else begin
            // all ports enq, so firstWay keeps the same
            // update enq day
            enqTime <= enqTime + fromInteger(valueof(SupSize));
        end
    endrule

`ifdef BSIM
    // sanity check in simulation
    // all valid entry are within [deqP, enqP), outsiders are invalid entries
    (* fire_when_enabled, no_implicit_conditions *)
    rule sanityCheck;
        for(Integer w = 0; w < valueof(SupSize); w = w+1) begin
            Bool empty = all( \== (False), readVEhr(0, valid[w]) );
            function Bool in_range(SingleScalarPtr i);
                // i is within [deqP, enqP)
                if(empty) begin
                    return False;
                end
                else begin
                    if(deqP[w] < enqP[w]) begin
                        return deqP[w] <= i && i < enqP[w];
                    end
                    else begin
                        return deqP[w] <= i || i < enqP[w];
                    end
                end
            endfunction
            for(Integer i = 0; i < valueof(SingleScalarSize); i = i+1) begin
                doAssert(in_range(fromInteger(i)) == valid[w][i][0],
                    "entries inside [deqP, enqP) should be valid, otherwise invalid"
                );
            end
        end
    endrule
`endif

    // get enq ifc
    // we compute can_enq signal for each FIFO[i], and all FIFO empty signal lazily
    staticAssert(lazyEnq, "Only support lazy enq");
    Vector#(SupSize, Wire#(Bool)) can_enq_fifo <- replicateM(mkBypassWire); // FIFO[i] can enq (enq slot invalid)
    Wire#(Bool) empty_for_enq <- mkBypassWire; // all FIFOs empty

    (* fire_when_enabled, no_implicit_conditions *)
    rule setEnqWires;
        Vector#(SupSize, Integer) idxVec = genVector;
        // get all empty
        function Bool isEmptyFunc(Integer i);
            return !valid[i][enqP[i]][0] && enqP[i] == deqP[i];
        endfunction
        empty_for_enq <= all(isEmptyFunc, idxVec);
        // get can enq
        function Action setCanEnq(Integer i);
        action
            can_enq_fifo[i] <= !valid[i][enqP[i]][0];
        endaction
        endfunction
        joinActions(map(setCanEnq, idxVec));
    endrule

    Vector#(SupSize, ROB_EnqPort) enqIfc;
    for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
        SupWaySel way = getEnqFifoWay(fromInteger(i)); // FIFO[way] is used by enq port i
        Bool can_enq = can_enq_fifo[way];
        enqIfc[i] = (interface ROB_EnqPort;
            method Bool canEnq = can_enq;
            method Action enq(ToReorderBuffer x) if(can_enq);
                doAssert(getEnqPort(way) == fromInteger(i), "enq FIFO way matches enq port");
                // record enq action, real action is applied later
                enqEn[i].wset(x);
                // make it conflict with wrong speculation
                wrongSpec_enq_conflict[i].wset(?);
                // ordering: sequence after many other methods
                deq_SB_enq[i] <= False;
                setExeAlu_SB_enq[i] <= False;
                setExeMem_SB_enq[i] <= False;
                setExeFpuMulDiv_SB_enq[i] <= False;
                setExeLSQ_SB_enq[i] <= False;
                setNotified_SB_enq[i] <= False;
            endmethod
            method InstTag getEnqInstTag;
                return InstTag {
                    way: way,
                    ptr: enqP[way],
                    t: enqTime + fromInteger(i)
                };
            endmethod
        endinterface);
    end

    // get deq ifc
    // get canDeq & first for each FIFO[i]
    function Bool getFifoCanDeq(Integer i) = valid[i][deqP[i]][valid_deq_port];
    function ToReorderBuffer getFifoFirst(Integer i) = row[i][deqP[i]].read_deq;
    Vector#(SupSize, Bool) can_deq_fifo = map(getFifoCanDeq, genVector);
    Vector#(SupSize, ToReorderBuffer) fifo_first = map(getFifoFirst, genVector);

    Vector#(SupSize, ROB_DeqPort) deqIfc;
    for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
        SupWaySel way = getDeqFifoWay(fromInteger(i)); // FIFO[way] is used by deq port i
        Bool can_deq = can_deq_fifo[way] &&
                       deq_SB_wrongSpec && // ordering: < wrongSpec
                       all(id, readVReg(deq_SB_enq)); // ordering: < enq
        deqIfc[i] = (interface ROB_DeqPort;
            method Bool canDeq = can_deq;
            method Action deq if(can_deq);
                doAssert(getDeqPort(way) == fromInteger(i), "deq FIFO way matches deq port");
                deqEn[i].send; // record deq action, real action is applied later
            endmethod
            method ToReorderBuffer deq_data if(can_deq);
                return fifo_first[way];
            endmethod
            method InstTag getDeqInstTag;
                return InstTag {
                    way: way,
                    ptr: deqP[way],
                    t: deqTime + fromInteger(i)
                };
            endmethod
        endinterface);
    end

    // set alu exe ifc
    Vector#(aluExeNum, ROB_setExecuted_doFinishAlu) aluSetExeIfc;
    for(Integer i = 0; i < valueof(aluExeNum); i = i+1) begin
        aluSetExeIfc[i] = (interface ROB_setExecuted_doFinishAlu;
            method Action set(
                InstTag x, Maybe#(Data) csrData, ControlFlow cf
            ) if(
                all(id, readVReg(setExeAlu_SB_enq)) // ordering: < enq
            );
                row[x.way][x.ptr].setExecuted_doFinishAlu[i].set(csrData, cf);
            endmethod
        endinterface);
    end

    Vector#(fpuMulDivExeNum, ROB_setExecuted_doFinishFpuMulDiv) fpuMulDivSetExeIfc;
    for(Integer i = 0; i < valueof(fpuMulDivExeNum); i = i+1) begin
        fpuMulDivSetExeIfc[i] = (interface ROB_setExecuted_doFinishFpuMulDiv;
            method Action set(
                InstTag x, Bit#(5) fflags
            ) if(
                all(id, readVReg(setExeFpuMulDiv_SB_enq)) // ordering: < enq
            );
                row[x.way][x.ptr].setExecuted_doFinishFpuMulDiv[i].set(fflags);
            endmethod
        endinterface);
    end

    // get pc/ppc ifc used by alu exe (also one pc for mem exe)
    Vector#(TAdd#(1, aluExeNum), ROB_getOrigPC) getOrigPCIfc;
    Vector#(aluExeNum, ROB_getOrigPredPC) getOrigPredPCIfc;
    for(Integer i = 0; i < valueof(aluExeNum) + 1; i = i+1) begin
        getOrigPCIfc[i] = (interface ROB_getOrigPC;
            method Addr get(InstTag x) = row[x.way][x.ptr].getOrigPC;
        endinterface);
    end
    for(Integer i = 0; i < valueof(aluExeNum); i = i+1) begin
        getOrigPredPCIfc[i] = (interface ROB_getOrigPredPC;
            method Addr get(InstTag x) = row[x.way][x.ptr].getOrigPredPC;
        endinterface);
    end

    interface enqPort = enqIfc;

    method Bool isEmpty;
        return empty_for_enq;
    endmethod

    interface deqPort = deqIfc;

    method Bool isEmpty_ehrPort0;
        function Bool isEmptyFunc(Integer i);
            return !valid[i][enqP[i]][0] && enqP[i] == deqP[i];
        endfunction
        Vector#(SupSize, Integer) idxVec = genVector;
        return all(isEmptyFunc, idxVec);
    endmethod

    method Bool isFull_ehrPort0;
        function Bool isFullFunc(Integer i);
            return valid[i][enqP[i]][0] && enqP[i] == deqP[i];
        endfunction
        Vector#(SupSize, Integer) idxVec = genVector;
        return all(isFullFunc, idxVec);
    endmethod

    method Action setLSQAtCommitNotified(InstTag x) if(
        all(id, readVReg(setNotified_SB_enq)) // ordering: < enq
    );
        row[x.way][x.ptr].setLSQAtCommitNotified;
    endmethod

    method Action setExecuted_deqLSQ(InstTag x, Maybe#(Exception) cause, Maybe#(LdKilledBy) ld_killed) if(
        all(id, readVReg(setExeLSQ_SB_enq)) // ordering: < enq
    );
        row[x.way][x.ptr].setExecuted_deqLSQ(cause, ld_killed);
    endmethod

    interface setExecuted_doFinishAlu = aluSetExeIfc;

    interface setExecuted_doFinishFpuMulDiv = fpuMulDivSetExeIfc;

    method Action setExecuted_doFinishMem(
        InstTag x, Addr vaddr, Bool access_at_commit, Bool non_mmio_st_done
    ) if(
        all(id, readVReg(setExeMem_SB_enq)) // ordering: < enq
    );
        row[x.way][x.ptr].setExecuted_doFinishMem(vaddr, access_at_commit, non_mmio_st_done);
    endmethod

`ifdef INORDER_CORE
    method Action setLSQTag(InstTag x, LdStQTag t, Bool isFence);
        row[x.way][x.ptr].setLSQTag(t, isFence);
    endmethod
`endif

    interface getOrigPC = getOrigPCIfc;
    interface getOrigPredPC = getOrigPredPCIfc;

    method InstTime getEnqTime = enqTime;

    interface ROB_SpeculationUpdate specUpdate;
        method Action correctSpeculation(SpecBits mask);
            for(Integer w = 0; w < valueof(SupSize); w = w+1) begin
                for(Integer i = 0; i < valueof(SingleScalarSize); i = i+1) begin
                    row[w][i].correctSpeculation(mask);
                end
            end
        endmethod

        method Action incorrectSpeculation(Bool killAll, SpecTag specTag, InstTag killInstTag);
            // record wrongSpec action
            wrongSpecEn.wset(ROBWrongSpecInput {
                killAll: killAll,
                specTag: specTag,
                killInstTag: killInstTag
            });
            // order after deq
            deq_SB_wrongSpec <= False;
            // make it conflict with enq
            for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
                wrongSpec_enq_conflict[i].wset(?);
            end
        endmethod
    endinterface
endmodule
