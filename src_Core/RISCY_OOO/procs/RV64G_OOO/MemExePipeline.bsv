
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
import BuildVector::*;
import GetPut::*;
import ClientServer::*;
import Cntrs::*;
import Fifos::*;
import Types::*;
import ProcTypes::*;
import MemoryTypes::*;
import SynthParam::*;
import Exec::*;
import Performance::*;
import ReservationStationEhr::*;
import ReservationStationMem::*;
import ReorderBuffer::*;
import TlbTypes::*;
import DTlb::*;
import SplitLSQ::*;
//import SerialLSQ::*;
import StoreBuffer::*;
import HasSpecBits::*;
import SpecFifo::*;
import SpecPoisonFifo::*;
import CCTypes::*;
import L1CoCache::*;
import Bypass::*;
import LatencyTimer::*;
import CacheUtils::*;
`ifdef PERFORMANCE_MONITORING
import PerformanceMonitor::*;
import SpecialRegs::*;
`endif

import Cur_Cycle :: *;

typedef struct {
    // inst info
    MemFunc mem_func;
    ImmData imm;
    PhyRegs regs;
    InstTag tag;
    LdStQTag ldstq_tag;
} MemDispatchToRegRead deriving(Bits, Eq, FShow);

typedef struct {
    // inst info
    MemFunc mem_func;
    ImmData imm;
    InstTag tag;
    LdStQTag ldstq_tag;
    // src reg vals
    Data rVal1;
    Data rVal2;
} MemRegReadToExe deriving(Bits, Eq, FShow);

typedef struct {
    // inst info
    MemFunc mem_func;
    InstTag tag;
    LdStQTag ldstq_tag;
    // result
    ByteEn shiftedBE;
    Addr vaddr;         // virtual addr
`ifdef INCLUDE_TANDEM_VERIF
    // for those mem instrs that store data
    Data    store_data;
    ByteEn  store_data_BE;
`endif
    Bool misaligned;
} MemExeToFinish deriving(Bits, Eq, FShow);

// bookkeeping when waiting for MMIO resp which may cause exception
typedef struct {
    Bool isLd;
} WaitMMIOResp deriving(Bits, Eq, FShow);

typedef union tagged {
    void Invalid;
    void Lr;
    void ScAmo;
    WaitMMIOResp MMIO;
} WaitLrScAmoMMIOResp deriving(Bits, Eq, FShow);

typedef enum {
    None, Lr, LdMMIO, Fence, ScAmo, StMMIO, System
} WaitReconcile deriving(Bits, Eq, FShow);

typedef struct {
    LineDataOffset offset;
    ByteEn shiftedBE;
    Data shiftedData;
} WaitStResp deriving(Bits, Eq, FShow);

// synthesized pipeline fifos
typedef SpecFifo_SB_deq_enq_C_deq_enq#(1, MemDispatchToRegRead) MemDispToRegFifo;
(* synthesize *)
module mkMemDispToRegFifo(MemDispToRegFifo);
    let m <- mkSpecFifo_SB_deq_enq_C_deq_enq(False);
    return m;
endmodule

typedef SpecFifo_SB_deq_enq_C_deq_enq#(1, MemRegReadToExe) MemRegToExeFifo;
(* synthesize *)
module mkMemRegToExeFifo(MemRegToExeFifo);
    let m <- mkSpecFifo_SB_deq_enq_C_deq_enq(False);
    return m;
endmodule

typedef DTlb#(MemExeToFinish) DTlbSynth;
(* synthesize *)
module mkDTlbSynth(DTlbSynth);
    function TlbReq getTlbReq(MemExeToFinish x);
        return TlbReq {
            addr: x.vaddr,
            write: (case(x.mem_func)
                        St, Sc, Amo: True;
                        default: False;
                    endcase)
        };
    endfunction
    let m <- mkDTlb(getTlbReq);
    return m;
endmodule

interface MemExeInput;
    // conservative scoreboard check in reg read stage
    method RegsReady sbCons_lazyLookup(PhyRegs r);
    // Phys reg file
    method Data rf_rd1(PhyRIndx rindx);
    method Data rf_rd2(PhyRIndx rindx);
    // CSR file
    method Data csrf_rd(CSR csr);
    // ROB
    method Addr rob_getPC(InstTag t);
    method Action rob_setExecuted_doFinishMem(InstTag t,
					      Addr vaddr,
					      Data store_data, ByteEn store_data_BE,
					      Bool access_at_commit, Bool non_mmio_st_done);
`ifdef INCLUDE_TANDEM_VERIF
    method Action rob_setExecuted_doFinishMem_RegData (InstTag t, Data dst_data);
`endif
    method Action rob_setExecuted_deqLSQ(InstTag t, Maybe#(Exception) cause, Maybe#(LdKilledBy) ld_killed);
    // MMIO
    method Bool isMMIOAddr(Addr a);
    method Action mmioReq(MMIOCRq r);
    method MMIODataPRs mmioRespVal;
    method Action mmioRespDeq;

    // global broadcast methods
    // set aggressive sb & wake up RS 
    method Action setRegReadyAggr_mem(PhyRIndx dst);
    method Action setRegReadyAggr_forward(PhyRIndx dst);
    // write reg file & set conservative sb
    method Action writeRegFile(PhyRIndx dst, Data data);

    // performance
    method Bool doStats;
endinterface

interface MemExePipeline;
    // recv bypass from exe and finish stages of each ALU pipeline
    interface Vector#(TMul#(2, AluExeNum), RecvBypass) recvBypass;
    interface ReservationStationMem rsMemIfc;
    interface DTlbSynth dTlbIfc;
    interface SplitLSQ lsqIfc;
    interface StoreBuffer stbIfc;
    interface DCoCache dMemIfc;
    interface SpeculationUpdate specUpdate;
`ifdef SELF_INV_CACHE
    interface Server#(void, void) reconcile;
`endif
    method Data getPerf(ExeStagePerfType t);
`ifdef PERFORMANCE_MONITORING
    method EventsCoreMem events;
`endif
endinterface

module mkMemExePipeline#(MemExeInput inIfc)(MemExePipeline);
    Bool verbose = False;

    // we change cache request in case of single core, becaues our MSI protocol
    // is not good with single core
    Bool multicore = valueof(CoreNum) > 1;

    // load/store memory total latency (max 1K cycle latency for 1 Ld/St)
    // These are always included as they are used by both stat counter systems.
    LatencyTimer#(LdQSize, 10) ldMemLatTimer <- mkLatencyTimer;
    LatencyTimer#(SBSize, 10) stMemLatTimer <- mkLatencyTimer;
`ifdef PERF_COUNT
    // load issue stall
    Count#(Data) exeLdStallByLdCnt <- mkCount(0);
    Count#(Data) exeLdStallByStCnt <- mkCount(0);
    Count#(Data) exeLdStallBySBCnt <- mkCount(0);
    // load forward count
    Count#(Data) exeLdForwardCnt <- mkCount(0);
    // load/store memory total latency (max 1K cycle latency for 1 Ld/St)
    Count#(Data) exeLdMemLat <- mkCount(0);
    Count#(Data) exeStMemLat <- mkCount(0);
    // load to use latency: dispatch to resp
    LatencyTimer#(LdQSize, 10) ldToUseLatTimer <- mkLatencyTimer;
    Count#(Data) exeLdToUseLat <- mkCount(0);
    Count#(Data) exeLdToUseCnt <- mkCount(0); // number of Ld resp written to reg file
    // address translate exception
    Count#(Data) exeTlbExcepCnt <- mkCount(0);
    // successful store-cond
    Count#(Data) exeScSuccessCnt <- mkCount(0);
    // fence count
    Count#(Data) exeLrScAmoAcqCnt <- mkCount(0);
    Count#(Data) exeLrScAmoRelCnt <- mkCount(0);
    Count#(Data) exeFenceCnt <- mkCount(0);
    Count#(Data) exeFenceAcqCnt <- mkCount(0);
    Count#(Data) exeFenceRelCnt <- mkCount(0);
`endif

`ifdef PERFORMANCE_MONITORING
    Array #(Reg #(EventsCoreMem)) events_reg <- mkDRegOR (5, unpack (0));
`endif

    // reservation station
    ReservationStationMem rsMem <- mkReservationStationMem;

    // pipeline fifos
    let dispToRegQ <- mkMemDispToRegFifo;
    let regToExeQ <- mkMemRegToExeFifo;

    // wire to recv bypass
    Vector#(TMul#(2, AluExeNum), RWire#(Tuple2#(PhyRIndx, Data))) bypassWire <- replicateM(mkRWire);

    // TLB
    DTlbSynth dTlb <- mkDTlbSynth;

    // store buffer only used in WEAK model
`ifdef TSO_MM
    StoreBuffer stb <- mkDummyStoreBuffer;
`else
    StoreBuffer stb <- mkStoreBufferEhr;
`endif
    // LSQ
    SplitLSQ lsq <- mkSplitLSQ;
    // wire to issue Ld which just finish addr tranlation
    RWire#(LSQIssueLdInfo) issueLd <- mkRWire;

    // waiting bit for Lr/Sc/Amo/MMIO resp
    Reg#(WaitLrScAmoMMIOResp) waitLrScAmoMMIOResp <- mkReg(Invalid);
`ifdef TSO_MM
    // TSO only: waiting for store resp; use **1-element** CF FIFO to make
    // store blocking and avoid conflict between pipelineResp_cRq and
    // doDeqStQ_St_Mem_issue
    Fifo#(1, WaitStResp) waitStRespQ <- mkCFFifo;
`endif
    // fifo for req mem
    Fifo#(1, Tuple2#(LdQTag, Addr)) reqLdQ <- mkBypassFifo;
    Fifo#(1, ProcRq#(DProcReqId)) reqLrScAmoQ <- mkBypassFifo;
`ifdef TSO_MM
    Fifo#(1, Addr) reqStQ <- mkBypassFifo;
`else
    Fifo#(1, Tuple2#(SBIndex, Addr)) reqStQ <- mkBypassFifo;
`endif
    // fifo for load result
    Fifo#(2, Tuple2#(LdQTag, MemResp)) forwardQ <- mkCFFifo;
    Fifo#(2, Tuple2#(LdQTag, MemResp)) memRespLdQ <- mkCFFifo;
    // fifo for Lr/Sc/Amo resp
    Fifo#(1, MemResp) respLrScAmoQ <- mkCFFifo;
    // resp ifc to D$
    L1ProcResp#(DProcReqId) procRespIfc = (interface L1ProcResp;
        method Action respLd(DProcReqId id, Data d);
            LdQTag tag = truncate(id);
            memRespLdQ.enq(tuple2(tag, d));
            // early wake up RS and set SB
            // this is done only when the resp is not wrong path
            LSQHitInfo info <- lsq.getHit(Ld (tag));
            if(info.dst matches tagged Valid .dst &&& !info.waitWPResp) begin
                inIfc.setRegReadyAggr_mem(dst.indx);
            end
            if(verbose) begin
                $display("[Ld resp] ", fshow(id), "; ", fshow(d), "; ", fshow(info));
            end
            // perf: load mem latency
            let lat <- ldMemLatTimer.done(tag);
`ifdef PERF_COUNT
            if(inIfc.doStats) begin
                exeLdMemLat.incr(zeroExtend(lat));
            end
`endif
`ifdef PERFORMANCE_MONITORING
            EventsCoreMem events = unpack(0);
            events.evt_LOAD_WAIT = saturating_truncate(lat);
            events_reg[1] <= events;
`endif
        endmethod
        method Action respLrScAmo(DProcReqId id, Data d);
            respLrScAmoQ.enq(d);
            if(verbose) begin
                $display("[Lr/Sc/Amo resp] ", fshow(id), "; ", fshow(d));
            end
        endmethod
`ifdef TSO_MM
        method ActionValue#(Tuple2#(LineByteEn, Line)) respSt(DProcReqId id);
            lsq.deqSt; // deq here
            let waitSt <- toGet(waitStRespQ).get;
            if(verbose) begin
                $display("[Store resp] idx ", fshow(id),
                         ", ", fshow(waitSt));
            end
            // perf: store mem latency
            let lat <- stMemLatTimer.done(0);
`ifdef PERF_COUNT
            if(inIfc.doStats) begin
                exeStMemLat.incr(zeroExtend(lat));
            end
`endif
`ifdef PERFORMANCE_MONITORING
            EventsCoreMem events = unpack(0);
            if (waitSt.shiftedBE == -1) events.evt_MEM_CAP_STORE = 1;
            events.evt_STORE_WAIT = saturating_truncate(lat);
            events_reg[2] <= events;
`endif
            // now figure out the data to be written
            Vector#(LineSzData, ByteEn) be = replicate(replicate(False));
            Line data = replicate(0);
            be[waitSt.offset] = waitSt.shiftedBE;
            data[waitSt.offset] = waitSt.shiftedData; //XXX I guess this doesn't work with capabilities?  Maybe we don't build TSO?
            return tuple2(unpack(pack(be)), data);
        endmethod
`else
        method ActionValue#(Tuple2#(LineByteEn, Line)) respSt(DProcReqId id);
            SBIndex idx = truncate(id);
            let e <- stb.deq(idx); // deq SB
            lsq.wakeupLdStalledBySB(idx); // wake up loads
            if(verbose) $display("[Store resp] idx = %x, ", idx, fshow(e));
            // perf: store mem latency
            let lat <- stMemLatTimer.done(idx);
`ifdef PERF_COUNT
            if(inIfc.doStats) begin
                exeStMemLat.incr(zeroExtend(lat));
            end
`endif
`ifdef PERFORMANCE_MONITORING
            EventsCoreMem events = unpack(0);
            if (pack(e.byteEn) == -1) events.evt_MEM_CAP_STORE = 1;
            events.evt_STORE_WAIT = saturating_truncate(lat);
            events_reg[2] <= events;
`endif
            return tuple2(e.byteEn, unpack(e.data)); // return SB entry
        endmethod
`endif
        method Action evict(LineAddr lineAddr);
`ifdef TSO_MM
            if(verbose) $display("[cache evict] ", fshow(lineAddr));
            lsq.cacheEvict(lineAddr);
`else
            noAction;
`endif
        endmethod
    endinterface);
    // non-blocking coherent D$
    DCoCache dMem <- mkDCoCache(procRespIfc);

`ifdef SELF_INV_CACHE
    // Waiting bit for reconcile to be performed. We set the bit and start
    // reconcile when we are about to deq an acquire from LSQ. The deq happens
    // only after reconcile is done. XXX Note that mem resp of Lr/Sc/Amo/MMIO
    // will not be dequeued unitl cache is reconciled if the Lr/Sc/Amo/MMIO
    // carries the .aq bit. However, since we only have 1 Lr/Sc/Amo/MMIO in
    // flight (which is always non-speculative), this won't block the cache or
    // network.
    Reg#(WaitReconcile) waitReconcile <- mkReg(None);
`endif

    //=======================================================
    // Reservation Station Stuff
    //=======================================================

    rule doDispatchMem;
        rsMem.doDispatch;
        let x = rsMem.dispatchData;
        if(verbose) $display("[doDispatchMem] ", fshow(x));

        // check store not having dst reg: this is for setting store to be
        // executed after address transation
        doAssert(!(x.data.mem_func == St && isValid(x.regs.dst)),
                 "St cannot have dst reg");
        
        // go to next stage
        dispToRegQ.enq(ToSpecFifo {
            data: MemDispatchToRegRead {
                mem_func: x.data.mem_func,
                imm: x.data.imm,
                regs: x.regs,
                tag: x.tag,
                ldstq_tag: x.data.ldstq_tag
            },
            spec_bits: x.spec_bits
        });

`ifdef PERF_COUNT
        // perf: load to use latency
        if(x.data.ldstq_tag matches tagged Ld .idx) begin
            ldToUseLatTimer.start(idx);
        end
`endif
    endrule

    rule doRegReadMem;
        dispToRegQ.deq;
        let dispToReg = dispToRegQ.first;
        let x = dispToReg.data;
        if(verbose) $display("[doRegReadMem] ", fshow(dispToReg));

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

        // go to next stage
        regToExeQ.enq(ToSpecFifo {
            data: MemRegReadToExe {
                mem_func: x.mem_func,
                imm: x.imm,
                tag: x.tag,
                ldstq_tag: x.ldstq_tag,
                rVal1: rVal1,
                rVal2: rVal2
            },
            spec_bits: dispToReg.spec_bits
        });
    endrule

    rule doExeMem;
        regToExeQ.deq;
        let regToExe = regToExeQ.first;
        let x = regToExe.data;
        if(verbose) $display("[doExeMem] ", fshow(regToExe));

        // get virtual addr & St/Sc/Amo data
        Addr vaddr = x.rVal1 + signExtend(x.imm);
        Data data = x.rVal2;

        // get shifted data and BE
        // we can use virtual addr to shift, since page size > dword size
        ByteEn origBE = lsq.getOrigBE(x.ldstq_tag);
        function Tuple2#(ByteEn, Data) getShiftedBEData(Addr addr, ByteEn be, Data d);
            Bit#(TLog#(NumBytes)) byteOffset = truncate(addr);
            return tuple2(unpack(pack(be) << byteOffset), d << {byteOffset, 3'b0});
        endfunction
        let {shiftBE, shiftData} = getShiftedBEData(vaddr, origBE, data);

        // update LSQ data now
        if(x.ldstq_tag matches tagged St .stTag) begin
            Data d = x.mem_func == Amo ? data : shiftData; // XXX don't shift for AMO
            lsq.updateData(stTag, d);
`ifdef PERFORMANCE_MONITORING
            EventsCoreMem events = unpack(0);
            events_reg[4] <= events;
`endif
        end

        // go to next stage by sending to TLB
        dTlb.procReq(DTlbReq {
            inst: MemExeToFinish {
                mem_func: x.mem_func,
                tag: x.tag,
                ldstq_tag: x.ldstq_tag,
                shiftedBE: shiftBE,
                vaddr: vaddr,
`ifdef INCLUDE_TANDEM_VERIF
	        store_data: data,
	        store_data_BE: origBE,
`endif
                misaligned: memAddrMisaligned(vaddr, origBE)
            },
            specBits: regToExe.spec_bits
        });
    endrule

    rule doFinishMem;
        dTlb.deqProcResp;
        let dTlbResp = dTlb.procResp;
        let x = dTlbResp.inst;
        let {paddr, cause} = dTlbResp.resp;

        if(verbose) $display("[doFinishMem] ", fshow(dTlbResp));
        if(isValid(cause) && verbose) $display("  [doFinishMem - dTlb response] PAGEFAULT!");

        Data store_data = ?;
        ByteEn store_data_BE = ?;
`ifdef INCLUDE_TANDEM_VERIF
        store_data    = x.store_data;
        store_data_BE = x.store_data_BE;
`endif

        // check misalignment
        if(!isValid(cause) && x.misaligned) begin
            case(x.mem_func)
                Ld, Lr: begin
                    cause = Valid (LoadAddrMisaligned);
                end
                default: begin
                    cause = Valid (StoreAddrMisaligned);
                end
            endcase
        end

        // check if addr is MMIO (only valid in case of no page fault)
        Bool isMMIO = inIfc.isMMIOAddr(paddr);
        // raise access fault in case of MMIO Lr/Sc
        if(!isValid(cause) && isMMIO) begin
            case(x.mem_func)
                Lr: begin
                    cause = Valid (LoadAccessFault);
                end
                Sc: begin
                    cause = Valid (StoreAccessFault);
                end
            endcase
        end

        // update ROB (access at commit and non-mmio st done can only be true
        // when there is no exceptio)
        Bool isLrScAmo = (case(x.mem_func)
            Lr, Sc, Amo: True;
            default: False;
        endcase);
        Bool access_at_commit = !isValid(cause) && (isMMIO || isLrScAmo);
        Bool non_mmio_st_done = !isValid(cause) && !isMMIO && x.mem_func == St;
        inIfc.rob_setExecuted_doFinishMem(x.tag, x.vaddr, store_data, store_data_BE,
                                          access_at_commit, non_mmio_st_done);

        // update LSQ
        LSQUpdateAddrResult updRes <- lsq.updateAddr(
            x.ldstq_tag, cause, paddr, isMMIO, x.shiftedBE
        );

        // issue non-MMIO Ld which has no exception and is not waiting for
        // wrong path resp
        if (x.mem_func == Ld && !isMMIO &&
            !isValid(cause) && !updRes.waitWPResp) begin
            LdQTag ldTag = ?;
            if(x.ldstq_tag matches tagged Ld .t) begin
                ldTag = t;
            end
            else begin
                doAssert(False, "must be in LdQ");
            end
            issueLd.wset(LSQIssueLdInfo {
                tag: ldTag,
                paddr: paddr,
                shiftedBE: x.shiftedBE
            });
        end

`ifdef PERF_COUNT
        if(isValid(cause) && inIfc.doStats) begin
            exeTlbExcepCnt.incr(1);
        end
`endif
    endrule

    //=======================================================
    // End of Reservation Station Stuff
    //=======================================================

    //=======================================================
    // Load/Store Queue Stuff
    //=======================================================

    // send Ld to forward or memory
    function Action doIssueLd(LSQIssueLdInfo info, Bool fromIssueQ);
    action
        // search SB only in WEAK model
`ifdef TSO_MM
        let sbRes = SBSearchRes {
            matchIdx: Invalid,
            forwardData: Invalid
        };
`else
        SBSearchRes sbRes = stb.search(info.paddr, info.shiftedBE);
`endif
`ifdef PERFORMANCE_MONITORING
        EventsCoreMem events = unpack(0);
        if (pack(info.shiftedBE) == -1) events.evt_MEM_CAP_LOAD = 1;
`endif
        // search LSQ
        LSQIssueLdResult issRes <- lsq.issueLd(info.tag, info.paddr, info.shiftedBE, sbRes);
        if(verbose) begin
            $display("[doIssueLd] fromIssueQ: ", fshow(fromIssueQ), " ; ",
                     fshow(info), " ; ", fshow(sbRes), " ; ", fshow(issRes));
        end
        // summarize
        if(issRes matches tagged Forward .forward) begin
            forwardQ.enq(tuple2(info.tag, forward.data));
            // early wake up
            if(forward.dst matches tagged Valid .dst) begin
                inIfc.setRegReadyAggr_forward(dst.indx);
            end
`ifdef PERF_COUNT
            // perf: load forward
            if(inIfc.doStats) begin
                exeLdForwardCnt.incr(1);
            end
`endif
        end
        else if(issRes == ToCache) begin
            reqLdQ.enq(tuple2(zeroExtend(info.tag), info.paddr));
            // perf: load mem latency
            ldMemLatTimer.start(info.tag);
        end
        else if(issRes matches tagged Stall .stallBy) begin
`ifdef PERF_COUNT
            // perf: load stall
            if(inIfc.doStats) begin
                case(stallBy)
                    LdQ: exeLdStallByLdCnt.incr(1);
                    StQ: exeLdStallByStCnt.incr(1);
                    SB: exeLdStallBySBCnt.incr(1);
                    default: doAssert(False, "unknow stall reason");
                endcase
            end
`endif
        end
        else begin
            doAssert(False, "load is stalled");
        end
`ifdef PERFORMANCE_MONITORING
        events_reg[0] <= events;
`endif
    endaction
    endfunction

    rule doIssueLdFromIssueQ;
        // get issue entry from LSQ
        LSQIssueLdInfo info <- lsq.getIssueLd;
        doIssueLd(info, True);
    endrule

    // we have ordered setRegReadyAggr_forward < setRegReadyAggr_mem to make
    // issue rule and cache resp rule to fire concurrently in weak model.
    // However, in TSO, when doAssert is removed in FPGA synthesis, lsq.deqLd
    // and lsq.issueLd are conflict-free with each other. This makes
    // doDeqLdQ_XX_deq rules ordered after doIssueLdFromXX rules, and leads to
    // schedule cycles (because bluespec compiler picks sub-optimal conflicts
    // to resolve some cycles). Therefore we manually create conflict and
    // precedence here using preempts.
    (* preempts = "doDeqLdQ_Lr_deq, doIssueLdFromUpdate" *)
    (* preempts = "doDeqLdQ_Lr_deq, doIssueLdFromIssueQ" *)
    (* preempts = "doDeqLdQ_MMIO_deq, doIssueLdFromUpdate" *)
    (* preempts = "doDeqLdQ_MMIO_deq, doIssueLdFromIssueQ" *)

    (* descending_urgency = "doIssueLdFromIssueQ, doIssueLdFromUpdate" *) // prioritize older load
    rule doIssueLdFromUpdate(issueLd.wget matches tagged Valid .info);
        // issue the entry that just updates LSQ this cycle
        doIssueLd(info, False);
    endrule

    // handle load resp
    function Action doRespLd(LdQTag tag, Data data, String rule_name);
    action
        LSQRespLdResult res <- lsq.respLd(tag, data);
        if(verbose) $display(rule_name, " ", fshow(tag), "; ", fshow(data), "; ", fshow(res));
        if(res.dst matches tagged Valid .dst) begin
            inIfc.writeRegFile(dst.indx, res.data);

`ifdef INCLUDE_TANDEM_VERIF
	    inIfc.rob_setExecuted_doFinishMem_RegData (res.instTag, res.data);
`endif

`ifdef PERF_COUNT
            // perf: load to use latency
            let lat <- ldToUseLatTimer.done(tag);
            if(inIfc.doStats) begin
                exeLdToUseLat.incr(zeroExtend(lat));
                exeLdToUseCnt.incr(1);
            end
`endif
        end
        if(res.wrongPath) begin
            doAssert(res.dst == Invalid, "wrong path resp cannot write reg");
        end
    endaction
    endfunction

    rule doRespLdMem;
        memRespLdQ.deq;
        let {t, d} = memRespLdQ.first;
        doRespLd(t, d, "[doRespLdMem]");
    endrule

    (* descending_urgency = "doRespLdMem, doRespLdForward" *) // prioritize mem resp
    rule doRespLdForward;
        forwardQ.deq;
        let {t, d} = forwardQ.first;
        doRespLd(t, d, "[doRespLdForward]");
    endrule

    // deqStQ
    LdQDeqEntry lsqDeqLd = lsq.firstLd;

    // deq fault/killed ld
    rule doDeqLdQ_fault(isValid(lsqDeqLd.fault));
        if(verbose) $display("[doDeqLdQ_fault] ", fshow(lsqDeqLd));
        lsq.deqLd;
        inIfc.rob_setExecuted_deqLSQ(lsqDeqLd.instTag, lsqDeqLd.fault, Invalid);
        // check
        doAssert(!isValid(lsqDeqLd.killed), "cannot be killed");
    endrule

    // deq non-MMIO Ld without fault (but may be killed)
    rule doDeqLdQ_Ld_Mem(
        !isValid(lsqDeqLd.fault) &&
        lsqDeqLd.memFunc == Ld && !lsqDeqLd.isMMIO
    );
        if(verbose) $display("[doDeqLdQ_Ld] ", fshow(lsqDeqLd));
        lsq.deqLd;
        // normal load should not have .rl, so no need to check SB empty
        doAssert(!lsqDeqLd.rel, "normal Ld cannot have .rl");
        // set ROB as Executed (may be killed)
        inIfc.rob_setExecuted_deqLSQ(lsqDeqLd.instTag, Invalid, lsqDeqLd.killed);
    endrule

    // issue non-MMIO Lr wihtout fault when
    // (1) not waiting for Lr/Sc/Amo/MMIO resp
    // (2) WEAK: SB does not match that addr
    // (3) WEAK: if .rl bit is set, SB is empty
    rule doDeqLdQ_Lr_issue(
        !isValid(lsqDeqLd.fault)
        && !lsqDeqLd.isMMIO
        && lsqDeqLd.memFunc == Lr
        && waitLrScAmoMMIOResp == Invalid
`ifndef TSO_MM
        && stb.noMatchLdQ(lsqDeqLd.paddr, lsqDeqLd.shiftedBE)
        && (!lsqDeqLd.rel || stb.isEmpty)
`endif
    );
        // set wait bit
        waitLrScAmoMMIOResp <= Lr;
        // send to mem
        ProcRq#(DProcReqId) req = ProcRq {
            id: 0, // id does not matter
            addr: lsqDeqLd.paddr,
            // Fetch Lr to E cannot guarantee forward progress of Lr/Sc, so by
            // default we only request Lr for S (in multicore). However, in
            // self-inv cache, Lr must fetch data to E, because parent does not
            // track S copies.
`ifdef SELF_INV_CACHE
            toState: E,
`else // !SELF_INV_CACHE
`ifdef LR_UP_TO_E
            toState: E, // makefile macro forces Lr to upgrade to E
`else
            toState: multicore ? S : E,
`endif
`endif // SELF_INV_CACHE
            op: Lr,
            byteEn: ?,
            data: ?,
            amoInst: ?
        };
        reqLrScAmoQ.enq(req);
        if(verbose) $display("[doDeqLdQ_Lr_issue] ", fshow(lsqDeqLd), "; ", fshow(req));
        // check
        doAssert(!isValid(lsqDeqLd.killed), "cannot be killed");
`ifdef PERF_COUNT
        if(inIfc.doStats) begin
            if(lsqDeqLd.acq) begin
                exeLrScAmoAcqCnt.incr(1);
            end
            if(lsqDeqLd.rel) begin
                exeLrScAmoRelCnt.incr(1);
            end
        end
`endif
    endrule

`ifdef SELF_INV_CACHE
    // issue reconcile to D$ in case of .aq
    rule doDeqLdQ_Lr_reconcile(
        waitLrScAmoMMIOResp == Lr &&
        lsqDeqLd.acq && waitReconcile == None
    );
        dMem.reconcile.request.put(?);
        waitReconcile <= Lr;
    endrule
`endif

    rule doDeqLdQ_Lr_deq(waitLrScAmoMMIOResp == Lr);
`ifdef SELF_INV_CACHE
        // wait reconcile to be done
        if(lsqDeqLd.acq) begin
            when(waitReconcile == Lr, noAction);
            let unused <- dMem.reconcile.response.get;
            waitReconcile <= None;
        end
`endif
        // deq LSQ & reset wait bit
        lsq.deqLd;
        waitLrScAmoMMIOResp <= Invalid;
        // get resp data (need shifting)
        let d <- toGet(respLrScAmoQ).get;
        Data resp = gatherLoad(lsqDeqLd.paddr, lsqDeqLd.byteEn, lsqDeqLd.unsignedLd, d); 
        // write reg file & set ROB as Executed & wakeup rs
        if(lsqDeqLd.dst matches tagged Valid .dst) begin
            inIfc.writeRegFile(dst.indx, resp);
            inIfc.setRegReadyAggr_mem(dst.indx);
`ifdef INCLUDE_TANDEM_VERIF
	    inIfc.rob_setExecuted_doFinishMem_RegData (lsqDeqLd.instTag, resp);
`endif
        end
        inIfc.rob_setExecuted_deqLSQ(lsqDeqLd.instTag, Invalid, Invalid);
        if(verbose) $display("[doDeqLdQ_Lr_deq] ", fshow(lsqDeqLd), "; ", fshow(d), "; ", fshow(resp));
        // check
        doAssert(lsqDeqLd.memFunc == Lr && !lsqDeqLd.isMMIO, "must be non-MMIO Lr");
        doAssert(!isValid(lsqDeqLd.fault) && !isValid(lsqDeqLd.killed), "no fualt or kill");
    endrule

    // issue MMIO Ld without fault when
    // (1) not waiting for Lr/Sc/Amo/MMIO resp
    // (2) WEAK: if .rl bit is set, SB is empty
    rule doDeqLdQ_MMIO_issue(
        !isValid(lsqDeqLd.fault)
        && lsqDeqLd.isMMIO
        && waitLrScAmoMMIOResp == Invalid
`ifndef TSO_MM
        && (!lsqDeqLd.rel || stb.isEmpty)
`endif
    );
        // set wait bit
        waitLrScAmoMMIOResp <= MMIO (WaitMMIOResp {
            isLd: True
        });
        // send to MMIO
        let req = MMIOCRq {
            addr: lsqDeqLd.paddr,
            func: Ld,
            byteEn: lsqDeqLd.shiftedBE, // BE is LSQ is always shifted
            data: ?
        };
        inIfc.mmioReq(req);
        if(verbose) $display("[doDeqLdQ_MMIO_issue] ", fshow(lsqDeqLd), "; ", fshow(req));
        // check
        doAssert(!isValid(lsqDeqLd.killed), "cannot be killed");
        doAssert(lsqDeqLd.memFunc == Ld, "LdQ MMIO is only Ld");
    endrule

`ifdef SELF_INV_CACHE
    // issue reconcile to D$ in case of .aq
    // This is in fact useless because MMIO can only be normal Ld which cannot
    // have .aq bit
    rule doDeqLdQ_MMIO_reconcile(
        waitLrScAmoMMIOResp matches tagged MMIO .waitMMIO &&&
        waitMMIO.isLd &&&
        lsqDeqLd.acq &&& waitReconcile == None
    );
        dMem.reconcile.request.put(?);
        waitReconcile <= LdMMIO;
    endrule
`endif

    rule doDeqLdQ_MMIO_deq(
        waitLrScAmoMMIOResp matches tagged MMIO .waitMMIO &&&
        waitMMIO.isLd &&&
        inIfc.mmioRespVal.valid
    );
`ifdef SELF_INV_CACHE
        // wait reconcile to be done
        if(lsqDeqLd.acq) begin
            when(waitReconcile == LdMMIO, noAction);
            let unused <- dMem.reconcile.response.get;
            waitReconcile <= None;
        end
`endif
        inIfc.mmioRespDeq;
        // deq LSQ & reset wait bit
        lsq.deqLd;
        waitLrScAmoMMIOResp <= Invalid;
        // get resp (need to shift data)
        let d = inIfc.mmioRespVal.data;
        Data resp = gatherLoad(lsqDeqLd.paddr, lsqDeqLd.byteEn, lsqDeqLd.unsignedLd, d);
        // write reg file & wakeup rs (this wakeup is late but MMIO is rare) & set ROB as Executed
        if(lsqDeqLd.dst matches tagged Valid .dst) begin
            inIfc.writeRegFile(dst.indx, resp);
            inIfc.setRegReadyAggr_mem(dst.indx);
`ifdef INCLUDE_TANDEM_VERIF
	    inIfc.rob_setExecuted_doFinishMem_RegData (lsqDeqLd.instTag, resp);
`endif
        end
        inIfc.rob_setExecuted_deqLSQ(lsqDeqLd.instTag, Invalid, Invalid);
        if(verbose) $display("[doDeqLdQ_MMIO_deq] ", fshow(lsqDeqLd), "; ", fshow(d), "; ", fshow(resp));
        // check
        doAssert(lsqDeqLd.memFunc == Ld && lsqDeqLd.isMMIO, "must be MMIO Ld");
        doAssert(!isValid(lsqDeqLd.fault) && !isValid(lsqDeqLd.killed), "no fualt or kill");
    endrule

    rule doDeqLdQ_MMIO_fault(
        waitLrScAmoMMIOResp matches tagged MMIO .waitMMIO &&&
        waitMMIO.isLd &&&
        !inIfc.mmioRespVal.valid
    );
`ifdef SELF_INV_CACHE
        // wait reconcile to be done
        if(lsqDeqLd.acq) begin
            when(waitReconcile == LdMMIO, noAction);
            let unused <- dMem.reconcile.response.get;
            waitReconcile <= None;
        end
`endif
        inIfc.mmioRespDeq;
        // deq LSQ & reset wait bit
        lsq.deqLd;
        waitLrScAmoMMIOResp <= Invalid;
        // set ROB to raise access fault
        inIfc.rob_setExecuted_deqLSQ(lsqDeqLd.instTag, Valid (LoadAccessFault), Invalid);
        if(verbose) $display("[doDeqLdQ_MMIO_fault] ", fshow(lsqDeqLd));
        // check
        doAssert(lsqDeqLd.memFunc == Ld && lsqDeqLd.isMMIO, "must be MMIO Ld");
        doAssert(!isValid(lsqDeqLd.fault) && !isValid(lsqDeqLd.killed), "no fualt or kill");
    endrule

    // deq StQ
    StQDeqEntry lsqDeqSt = lsq.firstSt;

    rule doDeqStQ_fault(isValid(lsqDeqSt.fault));
        if(verbose) $display("[doDeqStQ_fault] ", fshow(lsqDeqSt));
        lsq.deqSt;
        inIfc.rob_setExecuted_deqLSQ(lsqDeqSt.instTag, lsqDeqSt.fault, Invalid);
    endrule

`ifdef TSO_MM
    // TSO: issue non-MMIO St to memory. Since waitStRespQ is an 1-elem fifo,
    // if we can enq to it, then we are not waiting for store resp (i.e., this
    // store has not been issued yet)
    rule doDeqStQ_St_Mem_issue(
        !isValid(lsqDeqSt.fault) &&
        lsqDeqSt.memFunc == St && !lsqDeqSt.isMMIO
    );
        // send to mem
        Addr addr = lsqDeqSt.paddr;
        reqStQ.enq(addr);
        // record waiting for store resp
        LineDataOffset offset = getLineDataOffset(addr);
        waitStRespQ.enq(WaitStResp {
            offset: getLineDataOffset(addr),
            shiftedBE: lsqDeqSt.shiftedBE,
            shiftedData: lsqDeqSt.stData
        });
        // we leave deq to resp time
        // ROB should have already been set to executed
        if(verbose) $display("[doDeqStQ_St] ", fshow(lsqDeqSt));
        // perf: store mem latency
        stMemLatTimer.start(0);
    endrule

`else

    // WEAK: deq non-MMIO St when (1) no spec bit (2) can send to SB
    rule doDeqStQ_St_Mem(
        !isValid(lsqDeqSt.fault) &&&
        lsqDeqSt.memFunc == St &&& !lsqDeqSt.isMMIO &&&
        stb.getEnqIndex(lsqDeqSt.paddr) matches tagged Valid .sbIdx
    );
        lsq.deqSt;
        // send to SB
        stb.enq(sbIdx, lsqDeqSt.paddr, lsqDeqSt.shiftedBE, lsqDeqSt.stData);
        // ROB should have already been set to executed
        if(verbose) $display("[doDeqStQ_St] ", fshow(lsqDeqSt));
        // normal store should not have .rl, so no need to check SB empty
        doAssert(!lsqDeqSt.rel, "no .rl");
    endrule

    // send store to mem
    rule doIssueSB;
        let {sbIdx, en} <- stb.issue;
        reqStQ.enq(tuple2(sbIdx, {en.addr, 0}));
        // perf: store mem latency
        stMemLatTimer.start(sbIdx);
    endrule
`endif

    // deq Fence from SQ. need to check .aq and .rl.
`ifdef SELF_INV_CACHE
    // reconcile should be issued after release is done
    rule doDeqStQ_Fence_reconicle(
        !isValid(lsqDeqSt.fault)
        && lsqDeqSt.memFunc == Fence
        && (!lsqDeqSt.rel || stb.isEmpty) // wait release first
        && lsqDeqSt.acq && waitReconcile == None
    );
        dMem.reconcile.request.put(?);
        waitReconcile <= Fence;
    endrule
`endif

    rule doDeqStQ_Fence(
        !isValid(lsqDeqSt.fault)
        && lsqDeqSt.memFunc == Fence
`ifndef TSO_MM
        && (!lsqDeqSt.rel || stb.isEmpty) // check SB in case of release
`endif
    );
`ifdef SELF_INV_CACHE
        // wait reconcile to be done
        if(lsqDeqSt.acq) begin
            when(waitReconcile == Fence, noAction);
            let unused <- dMem.reconcile.response.get;
            waitReconcile <= None;
        end
`endif
        lsq.deqSt;
        // set ROB executed
        inIfc.rob_setExecuted_deqLSQ(lsqDeqSt.instTag, Invalid, Invalid);
        if(verbose) $display("[doDeqStQ_Fence] ", fshow(lsqDeqSt));
`ifdef PERF_COUNT
        if(inIfc.doStats) begin
            exeFenceCnt.incr(1);
            if(lsqDeqSt.acq) begin
                exeFenceAcqCnt.incr(1);
            end
            if(lsqDeqSt.rel) begin
                exeFenceRelCnt.incr(1);
            end
        end
`endif
    endrule

    // issue non-MMIO Sc/Amo without fault when
    // (1) not waiting for Lr/Sc/Amo/MMIO resp
    // (2) WEAK: SB does not match that addr
    // (3) WEAK: if .rl bit is set, SB is empty
    rule doDeqStQ_ScAmo_issue(
        !isValid(lsqDeqSt.fault)
        && !lsqDeqSt.isMMIO
        && (lsqDeqSt.memFunc == Sc || lsqDeqSt.memFunc == Amo)
        && waitLrScAmoMMIOResp == Invalid
`ifndef TSO_MM
        && stb.noMatchStQ(lsqDeqSt.paddr, lsqDeqSt.shiftedBE)
        && (!lsqDeqSt.rel || stb.isEmpty)
`endif
    );
        // set wait bit
        waitLrScAmoMMIOResp <= ScAmo;
        // send to mem
        ProcRq#(DProcReqId) req = ProcRq {
            id: 0, // id does not matter
            addr: lsqDeqSt.paddr,
            toState: M,
            op: lsqDeqSt.memFunc == Sc ? Sc : Amo,
            // XXX Amo uses **original** data (firstSt.stData is the original
            // data for Amo). AMO doesn't use BE. Sc uses **shifted** BE and
            // data (firstSt.stData is shifted for Sc).
            byteEn: lsqDeqSt.shiftedBE,
            data: lsqDeqSt.stData,
            amoInst: AmoInst {
                func: lsqDeqSt.amoFunc,
                doubleWord: lsqDeqSt.shiftedBE == replicate(True),
                aq: lsqDeqSt.acq,
                rl: lsqDeqSt.rel
            }
        };
        reqLrScAmoQ.enq(req);
        if(verbose) $display("[doDeqStQ_ScAmo_issue] ", fshow(lsqDeqSt), "; ", fshow(req));
`ifdef PERF_COUNT
        if(inIfc.doStats) begin
            if(lsqDeqSt.acq) begin
                exeLrScAmoAcqCnt.incr(1);
            end
            if(lsqDeqSt.rel) begin
                exeLrScAmoRelCnt.incr(1);
            end
        end
`endif
    endrule

`ifdef SELF_INV_CACHE
    rule doDeqStQ_ScAmo_reconcile(
        waitLrScAmoMMIOResp == ScAmo &&
        lsqDeqSt.acq && waitReconcile == None
    );
        dMem.reconcile.request.put(?);
        waitReconcile <= ScAmo;
    endrule
`endif

    // deq non-MMIO Sc/Amo from LSQ when resp comes
    rule doDeqStQ_ScAmo_deq(waitLrScAmoMMIOResp == ScAmo);
`ifdef SELF_INV_CACHE
        // wait reconcile to be done
        if(lsqDeqSt.acq) begin
            when(waitReconcile == ScAmo, noAction);
            let unused <- dMem.reconcile.response.get;
            waitReconcile <= None;
        end
`endif
        // deq LSQ & reset wait bit
        lsq.deqSt;
        waitLrScAmoMMIOResp <= Invalid;
        // get resp data (no need to shift for Sc and Amo)
        Data resp <- toGet(respLrScAmoQ).get;
        // write reg file & set ROB as Executed & wake up rs
        if(lsqDeqSt.dst matches tagged Valid .dst) begin
            inIfc.writeRegFile(dst.indx, resp);
            inIfc.setRegReadyAggr_mem(dst.indx);
`ifdef INCLUDE_TANDEM_VERIF
	    inIfc.rob_setExecuted_doFinishMem_RegData (lsqDeqSt.instTag, resp);
`endif
        end
        inIfc.rob_setExecuted_deqLSQ(lsqDeqSt.instTag, Invalid, Invalid);
        if(verbose) $display("[doDeqStQ_ScAmo_deq] ", fshow(lsqDeqSt), "; ", fshow(resp));
        // check
        doAssert((lsqDeqSt.memFunc == Sc || lsqDeqSt.memFunc == Amo) &&
                 !lsqDeqSt.isMMIO, "must be non-MMIO Sc/Amo");
        doAssert(!isValid(lsqDeqSt.fault), "no fault");
        // stats for successful SC
`ifdef PERF_COUNT
        if(inIfc.doStats && lsqDeqSt.memFunc == Sc && resp == fromInteger(valueof(ScSuccVal))) begin
            exeScSuccessCnt.incr(1);
        end
`endif
`ifdef PERFORMANCE_MONITORING
        EventsCoreMem events = unpack(0);
        events.evt_SC_SUCCESS = 1;
        events_reg[3] <= events;
`endif
    endrule

    // issue MMIO St/Amo when
    // (0) XXX Not a fence. MMIO bit of an fence entry in SQ may be
    // uninitialized. For other entries, MMIO bit must be initialized because
    // the deq entry is computed.
    // (1) not waiting for Lr/Sc/Amo/MMIO resp
    // (2) WEAK: if .rl bit is set, SB is empty
    rule doDeqStQ_MMIO_issue(
        !isValid(lsqDeqSt.fault)
        && lsqDeqSt.memFunc != Fence
        && lsqDeqSt.isMMIO
        && waitLrScAmoMMIOResp == Invalid
`ifndef TSO_MM
        && (!lsqDeqSt.rel || stb.isEmpty)
`endif
    );
        // set wait bit
        waitLrScAmoMMIOResp <= MMIO (WaitMMIOResp {
            isLd: False
        });
        // send to MMIO
        let req = MMIOCRq {
            addr: lsqDeqSt.paddr,
            func: (case(lsqDeqSt.memFunc)
                       St: (St);
                       Amo: (Amo (lsqDeqSt.amoFunc));
                       default: ?;
                   endcase),
            byteEn: lsqDeqSt.shiftedBE, // BE is LSQ is always shifted
            data: lsqDeqSt.stData // stData in LSQ is not shifted for AMO but for St
        };
        inIfc.mmioReq(req);
        if(verbose) $display("[doDeqStQ_MMIO_issue] ", fshow(lsqDeqSt), "; ", fshow(req));
        // MMIO may cause exception, must have spec tag, and only can be St/Amo
        doAssert(lsqDeqSt.memFunc == St || lsqDeqSt.memFunc == Amo, "must be St/Amo");
`ifdef PERF_COUNT
        if(inIfc.doStats) begin
            if(lsqDeqSt.acq) begin
                exeLrScAmoAcqCnt.incr(1);
            end
            if(lsqDeqSt.rel) begin
                exeLrScAmoRelCnt.incr(1);
            end
        end
`endif
    endrule

`ifdef SELF_INV_CACHE
    rule doDeqStQ_MMIO_reconcile(
        waitLrScAmoMMIOResp matches tagged MMIO .waitMMIO &&&
        !waitMMIO.isLd &&&
        lsqDeqSt.acq &&& waitReconcile == None
    );
        dMem.reconcile.request.put(?);
        waitReconcile <= StMMIO;
    endrule
`endif

    // deq MMIO from StQ when valid resp comes
    rule doDeqStQ_MMIO_deq(
        waitLrScAmoMMIOResp matches tagged MMIO .waitMMIO &&&
        !waitMMIO.isLd &&&
        inIfc.mmioRespVal.valid
    );
`ifdef SELF_INV_CACHE
        // wait reconcile to be done
        if(lsqDeqSt.acq) begin
            when(waitReconcile == StMMIO, noAction);
            let unused <- dMem.reconcile.response.get;
            waitReconcile <= None;
        end
`endif
        inIfc.mmioRespDeq;
        // deq LSQ & reset wait bit
        lsq.deqSt;
        waitLrScAmoMMIOResp <= Invalid;
        // get resp (no need to shift for AMO)
        Data resp = inIfc.mmioRespVal.data;
        // write reg file & wakeup rs (this wakeup is late but MMIO is rare) & set ROB as Executed
        if(lsqDeqSt.dst matches tagged Valid .dst) begin
            inIfc.writeRegFile(dst.indx, resp);
            inIfc.setRegReadyAggr_mem(dst.indx);
`ifdef INCLUDE_TANDEM_VERIF
	    inIfc.rob_setExecuted_doFinishMem_RegData (lsqDeqSt.instTag, resp);
`endif
        end
        inIfc.rob_setExecuted_deqLSQ(lsqDeqSt.instTag, Invalid, Invalid);
        if(verbose) $display("[doDeqStQ_MMIO_deq] ", fshow(lsqDeqSt), "; ", fshow(resp));
        // check
        doAssert(lsqDeqSt.memFunc == St || lsqDeqSt.memFunc == Amo, "must be St/Amo");
        doAssert(!isValid(lsqDeqSt.fault), "no fault");
    endrule

    rule doDeqStQ_MMIO_fault(
        waitLrScAmoMMIOResp matches tagged MMIO .waitMMIO &&&
        !waitMMIO.isLd &&&
        !inIfc.mmioRespVal.valid
    );
`ifdef SELF_INV_CACHE
        // wait reconcile to be done
        if(lsqDeqSt.acq) begin
            when(waitReconcile == StMMIO, noAction);
            let unused <- dMem.reconcile.response.get;
            waitReconcile <= None;
        end
`endif
        inIfc.mmioRespDeq;
        // deq LSQ & reset wait bit
        lsq.deqSt;
        waitLrScAmoMMIOResp <= Invalid;
        // set ROB to raise access fault
        inIfc.rob_setExecuted_deqLSQ(lsqDeqSt.instTag, Valid (StoreAccessFault), Invalid);
        if(verbose) $display("[doDeqStQ_MMIO_fault] ", fshow(lsqDeqSt));
        // check
        doAssert(lsqDeqSt.memFunc == St || lsqDeqSt.memFunc == Amo, "must be St/Amo");
        doAssert(!isValid(lsqDeqSt.fault), "no fault");
    endrule

    // send req to D$
    rule sendLdToMem;
        let {lsqTag, addr} <- toGet(reqLdQ).get;
        dMem.procReq.req(ProcRq {
            id: zeroExtend(lsqTag),
            addr: addr,
            toState: multicore ? S : E, // in case of single core, just fetch to E
            op: Ld,
            byteEn: ?,
            data: ?,
            amoInst: ?
        });
    endrule
    (* descending_urgency = "sendLdToMem, sendStToMem" *) // prioritize Ld over St
    rule sendStToMem;
`ifdef TSO_MM
        let addr <- toGet(reqStQ).get;
        DProcReqId id = 0;
`else
        let {sbIdx, addr} <- toGet(reqStQ).get;
        DProcReqId id = zeroExtend(sbIdx);
`endif
        dMem.procReq.req(ProcRq {
            id: id,
            addr: addr,
            toState: M,
            op: St,
            byteEn: ?,
            data: ?,
            amoInst: ?
        });
    endrule
    (* descending_urgency = "sendLrScAmoToMem, sendStToMem" *) // prioritize Lr/Sc/Amo over St
    rule sendLrScAmoToMem;
        let r <- toGet(reqLrScAmoQ).get;
        dMem.procReq.req(r);
    endrule

    //=======================================================
    // End of Load/Store Queue Stuff
    //=======================================================

    interface recvBypass = map(getRecvBypassIfc, bypassWire);
    interface rsMemIfc = rsMem;
    interface dTlbIfc = dTlb;
    interface lsqIfc = lsq;
    interface stbIfc = stb;
    interface dMemIfc = dMem;
    interface specUpdate = joinSpeculationUpdate(vec(
        rsMem.specUpdate,
        dispToRegQ.specUpdate,
        regToExeQ.specUpdate,
        dTlb.specUpdate,
        lsq.specUpdate
    ));

`ifdef SELF_INV_CACHE
    interface Server reconcile;
        interface Put request;
            method Action put(void x) if(waitReconcile == None);
                dMem.reconcile.request.put(?);
                waitReconcile <= System;
            endmethod
        endinterface
        interface Get response;
            method ActionValue#(void) get if(waitReconcile == System);
                let unused <- dMem.reconcile.response.get;
                waitReconcile <= None;
                return ?;
            endmethod
        endinterface
    endinterface
`endif

    method Data getPerf(ExeStagePerfType t);
        return (case(t)
`ifdef PERF_COUNT
            ExeLdStallByLd: exeLdStallByLdCnt;
            ExeLdStallBySt: exeLdStallByStCnt;
            ExeLdStallBySB: exeLdStallBySBCnt;
            ExeLdForward: exeLdForwardCnt;
            ExeLdMemLat: exeLdMemLat;
            ExeStMemLat: exeStMemLat;
            ExeLdToUseLat: exeLdToUseLat;
            ExeLdToUseCnt: exeLdToUseCnt;
            ExeTlbExcep: exeTlbExcepCnt;
            ExeScSuccessCnt: exeScSuccessCnt;
            ExeLrScAmoAcqCnt: exeLrScAmoAcqCnt;
            ExeLrScAmoRelCnt: exeLrScAmoRelCnt;
            ExeFenceAcqCnt: exeFenceAcqCnt;
            ExeFenceRelCnt: exeFenceRelCnt;
            ExeFenceCnt: exeFenceCnt;
`endif
            default: 0;
        endcase);
    endmethod
`ifdef PERFORMANCE_MONITORING
    method events = events_reg[0];
`endif
endmodule
