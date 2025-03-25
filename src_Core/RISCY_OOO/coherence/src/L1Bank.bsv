// Copyright (c) 2017 Massachusetts Institute of Technology
//
//-
// RVFI_DII + CHERI modifications:
//     Copyright (c) 2020 Alexandre Joannou
//     Copyright (c) 2020 Jonathan Woodruff
//     All rights reserved.
//
//     This software was developed by SRI International and the University of
//     Cambridge Computer Laboratory (Department of Computer Science and
//     Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
//     DARPA SSITH research programme.
//
//     This work was supported by NCSC programme grant 4212611/RFA 15971 ("SafeBet").
//-
// Prefetcher modifications:
//     Copyright (c) 2023 Karlis Susters
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

import Types::*;
import MemoryTypes::*;
import Amo::*;

import Cntrs::*;
import Vector::*;
import ConfigReg::*;
import BuildVector::*;
import FIFO::*;
import GetPut::*;
import CCTypes::*;
import L1CRqMshr::*;
import L1PRqMshr::*;
import CCPipe::*;
import L1Pipe ::*;
import FShow::*;
import DefaultValue::*;
import Ehr::*;
import Fifos::*;
import CacheUtils::*;
import CrossBar::*;
import Performance::*;
import LatencyTimer::*;
import RandomReplace::*;
import Prefetcher::*;
`ifdef PERFORMANCE_MONITORING
import PerformanceMonitor::*;
import StatCounters::*;
import BlueUtils::*;
`endif

export L1CRqStuck(..);
export L1PRqStuck(..);
export L1Bank(..);
export mkL1Bank;
export mkL1Cache;

// although pRq never appears in dependency chain
// we still need pRq MSHR to limit the number of pRq
// and thus limit the size of rsToPIndexQ

typedef struct {
    Addr addr;
    MemOp op;
    L1CRqState state;
    Msi slotCs;
    Bool waitP;
} L1CRqStuck deriving(Bits, Eq, FShow);

typedef L1PRqMshrStuck L1PRqStuck;

interface L1Bank#(
    numeric type lgBankNum,
    numeric type wayNum,
    numeric type indexSz,
    numeric type tagSz,
    numeric type cRqNum,
    numeric type pRqNum,
    type procRqIdT // id of req from processor core
);
    interface ChildCacheToParent#(Bit#(TLog#(wayNum)), void) to_parent;
    interface L1ProcReq#(procRqIdT) procReq;
    // reset link addr
    method Action resetLinkAddr;
    // detect deadlock: only in use when macro CHECK_DEADLOCK is defined
    interface Get#(L1CRqStuck) cRqStuck;
    interface Get#(L1PRqStuck) pRqStuck;
    // security: flush
    method Action flush;
    method Bool flush_done;
    // performance
    method Action setPerfStatus(Bool stats);
    method Data getPerfData(L1DPerfType t);
`ifdef PERFORMANCE_MONITORING
    method EventsL1D events;
`endif
endinterface

typedef struct {
    cRqIdxT n; // AMO req MSHR idx
    cRqT req; // AMO req
    Maybe#(cRqIdxT) succ; // same-addr-successor of AMO req
    Maybe#(cRqIdxT) nextInQueue;
} AmoHitInfo#(type cRqIdxT, type cRqT) deriving(Bits, Eq, FShow);

module mkL1Bank#(
    Bit#(lgBankNum) bankId,
    module#(L1CRqMshr#(cRqNum, indexT, wayT, tagT, procRqT)) mkL1CRqMshrLocal,
    module#(L1PRqMshr#(pRqNum)) mkL1PRqMshrLocal,
    module#(L1Pipe#(lgBankNum, wayNum, indexT, tagT, cRqIdxT, pRqIdxT)) mkL1Pipeline,
    L1ProcResp#(procRqIdT) procResp
)(
    L1Bank#(lgBankNum, wayNum, indexSz, tagSz, cRqNum, pRqNum, procRqIdT)
) provisos(
    Alias#(wayT, Bit#(TLog#(wayNum))),
    Alias#(indexT, Bit#(indexSz)),
    Alias#(tagT, Bit#(tagSz)),
    Alias#(cRqIdxT, Bit#(TLog#(cRqNum))),
    Alias#(pRqIdxT, Bit#(TLog#(pRqNum))),
    Alias#(cacheOwnerT, Maybe#(cRqIdxT)), // actually owner cannot be pRq
    Alias#(cacheSetAuxT, Maybe#(cRqIdxT)),
    Alias#(cacheInfoT, CacheInfo#(tagT, Msi, void, cacheOwnerT, void)),
    Alias#(ramDataT, RamData#(tagT, Msi, void, cacheOwnerT, void, Line)),
    Alias#(procRqT, ProcRq#(procRqIdT)),
    Alias#(cRqToPT, CRqMsg#(wayT, void)),
    Alias#(cRsToPT, CRsMsg#(void)),
    Alias#(pRqFromPT, PRqMsg#(void)),
    Alias#(pRsFromPT, PRsMsg#(wayT, void)),
    Alias#(pRqRsFromPT, PRqRsMsg#(wayT, void)),
    Alias#(cRqSlotT, L1CRqSlot#(wayT, tagT)), // cRq MSHR slot
    Alias#(l1CmdT, L1Cmd#(indexT, cRqIdxT, pRqIdxT)),
    Alias#(pipeOutT, PipeOut#(wayT, tagT, Msi, void, cacheOwnerT, void, RandRepInfo, Line, cacheSetAuxT, l1CmdT)),
    // requirements
    Bits#(procRqIdT, _procRqIdT),
    FShow#(procRqIdT),
    FShow#(pipeOutT),
    Add#(tagSz, a__, AddrSz),
    // make sure: cRqNum <= wayNum
    Add#(cRqNum, b__, wayNum),
    Add#(TAdd#(tagSz, indexSz), TAdd#(lgBankNum, LgLineSzBytes), AddrSz)
);

    Bool verbose = False;

    L1CRqMshr#(cRqNum, indexT, wayT, tagT, procRqT) cRqMshr <- mkL1CRqMshrLocal;

    L1PRqMshr#(pRqNum) pRqMshr <- mkL1PRqMshrLocal;

    L1Pipe#(lgBankNum, wayNum, indexT, tagT, cRqIdxT, pRqIdxT) pipeline <- mkL1Pipeline;

    Fifo#(1, procRqT) rqFromCQ <- mkBypassFifo;

    Fifo#(2, cRsToPT) rsToPQ <- mkCFFifo;
    Fifo#(2, cRqToPT) rqToPQ <- mkCFFifo;
    Fifo#(2, pRqRsFromPT) fromPQ <- mkCFFifo;

    FIFO#(MshrIndex#(cRqIdxT, pRqIdxT)) rsToPIndexQ <- mkSizedFIFO(valueOf(TAdd#(cRqNum, pRqNum)));

    // mshr index of req that is waken up Sc fails early (before hit)
    Fifo#(cRqNum, cRqIdxT) cRqRetryIndexQ <- mkCFFifo;

    FIFO#(cRqIdxT) rqToPIndexQ <- mkSizedFIFO(valueOf(cRqNum));
    // temp fifo for pipelineResp & sendRsToP (reduce conflict)
    FIFO#(cRqIdxT) rqToPIndexQ_pipelineResp <- mkFIFO;
    FIFO#(cRqIdxT) rqToPIndexQ_sendRsToP <- mkFIFO;

    Ehr#(2, Maybe#(LineAddr)) linkAddrEhr <- mkEhr(Invalid); // load reservation
    Reg#(Maybe#(LineAddr)) linkAddr = linkAddrEhr[0]; // normal processing use port 0
    Reg#(Maybe#(LineAddr)) linkAddrRst = linkAddrEhr[1]; // reset by outside use port 1

    // we process AMO resp in a new cycle to cut critical path
    Reg#(Maybe#(AmoHitInfo#(cRqIdxT, procRqT))) processAmo <- mkReg(Invalid);

    Vector#(cRqNum, Reg#(Bool)) cRqIsPrefetch <- replicateM(mkReg(?));
    let prefetcher <- mkL1DPrefetcher;
    let llcPrefetcher <- mkLLDPrefetcherInL1D;

    // security flush
`ifdef SECURITY_CACHES
    Reg#(Bool) flushDone <- mkReg(True);
    Reg#(Bool) flushReqStart <- mkReg(False);
    Reg#(Bool) flushReqDone <- mkReg(False);
    Reg#(Bool) flushRespDone <- mkReg(False);
    Reg#(indexT) flushIndex <- mkReg(0);
    Reg#(wayT) flushWay <- mkReg(0);
`else
    Bool flushDone = True;
`endif

    // performance
    LatencyTimer#(cRqNum, 10) latTimer <- mkLatencyTimer; // max 1K cycle latency
`ifdef PERF_COUNT
    Reg#(Bool) doStats <- mkConfigReg(False);
    Count#(Data) ldCnt <- mkCount(0);
    Count#(Data) stCnt <- mkCount(0);
    Count#(Data) amoCnt <- mkCount(0);
    Count#(Data) ldMissCnt <- mkCount(0);
    Count#(Data) stMissCnt <- mkCount(0);
    Count#(Data) amoMissCnt <- mkCount(0);
    Count#(Data) ldMissLat <- mkCount(0);
    Count#(Data) stMissLat <- mkCount(0);
    Count#(Data) amoMissLat <- mkCount(0);
`endif
`ifdef PERFORMANCE_MONITORING
    Array #(Reg #(EventsL1D)) perf_events <- mkDRegOR (2, unpack (0));
`endif
function Action incrReqCnt(MemOp op);
action
`ifdef PERF_COUNT
    if(doStats) begin
        case(op)
            Ld: ldCnt.incr(1);
            St: stCnt.incr(1);
            Lr, Sc, Amo: amoCnt.incr(1);
        endcase
    end
`endif
`ifdef PERFORMANCE_MONITORING
    EventsL1D events = unpack (0);
    case(op)
        Ld: events.evt_LD = 1;
        St: events.evt_ST = 1;
        Lr, Sc, Amo: events.evt_AMO = 1;
    endcase
    perf_events[0] <= events;
`endif
    noAction;
endaction
endfunction

function Action incrMissCnt(MemOp op, cRqIdxT idx);
action
    let lat <- latTimer.done(idx);
`ifdef PERF_COUNT
    if(doStats) begin
        case(op)
            Ld: begin
                ldMissLat.incr(zeroExtend(lat));
                ldMissCnt.incr(1);
            end
            St: begin
                stMissLat.incr(zeroExtend(lat));
                stMissCnt.incr(1);
            end
            Lr, Sc, Amo: begin
                amoMissLat.incr(zeroExtend(lat));
                amoMissCnt.incr(1);
            end
        endcase
    end
`endif
`ifdef PERFORMANCE_MONITORING
    EventsL1D events = unpack (0);
    case(op)
        Ld: begin
            events.evt_LD_MISS_LAT = saturating_truncate(lat);
            events.evt_LD_MISS = 1;
        end
        St: begin
            events.evt_ST_MISS_LAT = saturating_truncate(lat);
            events.evt_ST_MISS = 1;
        end
        Lr, Sc, Amo: begin
            events.evt_AMO_MISS_LAT = saturating_truncate(lat);
            events.evt_AMO_MISS = 1;
        end
    endcase
    perf_events[1] <= events;
`endif
    noAction;
endaction
endfunction


    function tagT getTag(Addr a) = truncateLSB(a);

    // send retrying cRq (which follows early failed Sc) to pipeline
    rule cRqTransfer_retry(cRqRetryIndexQ.notEmpty);
        cRqRetryIndexQ.deq;
        cRqIdxT n = cRqRetryIndexQ.first;
        // XXX don't change MSHR entry to Init
        // later cRq to same addr needs to be appended after this one, and we
        // need to know that we should not append this one to other cRq to the
        // same addr
        // send to pipeline
        procRqT req = cRqMshr.cRqTransfer.getRq(n);
        pipeline.send(CRq (L1PipeRqIn {
            addr: req.addr,
            mshrIdx: n
        }));
        cRqIsPrefetch[n] <= False;
       if (verbose)
        $display("%t L1 %m cRqTransfer_retry: ", $time,
            fshow(n), " ; ",
            fshow(req)
        );
    endrule

    // although D$ may not have cRq at every cycle
    // we still make cRq has lower priorty than pRq/pRs
    // we stop accepting cRq when we need to flush for security
    rule cRqTransfer_new(!cRqRetryIndexQ.notEmpty && flushDone);
        procRqT r <- toGet(rqFromCQ).get;
        cRqIdxT n <- cRqMshr.cRqTransfer.getEmptyEntryInit(r);
        // send to pipeline
        pipeline.send(CRq (L1PipeRqIn {
            addr: r.addr,
            mshrIdx: n
        }));
        cRqIsPrefetch[n] <= False;
        // performance counter: cRq type
        incrReqCnt(r.op);
       if (verbose)
        $display("%t L1 %m cRqTransfer_new: ", $time,
            fshow(n), " ; ",
            fshow(r)
        );
    endrule

    (* descending_urgency = "pRqTransfer, cRqTransfer_retry, cRqTransfer_new" *)
    rule pRqTransfer(fromPQ.first matches tagged PRq .req);
        fromPQ.deq;
        pRqIdxT n <- pRqMshr.getEmptyEntryInit(req);
        // send to pipeline
        pipeline.send(PRq (L1PipeRqIn {
            addr: req.addr,
            mshrIdx: n
        }));
       if (verbose)
        $display("%t L1 %m pRqTransfer: ", $time,
            fshow(n), " ; ",
            fshow(req)
        );
    endrule

    (* descending_urgency = "pRsTransfer, cRqTransfer_retry, cRqTransfer_new" *)
    rule pRsTransfer(fromPQ.first matches tagged PRs .resp);
        fromPQ.deq;
        pipeline.send(PRs (L1PipePRsIn {
            addr: resp.addr,
            toState: resp.toState,
            data: resp.data,
            way: resp.id
        }));
       if (verbose)
        $display("%t L1 %m pRsTransfer: ", $time, fshow(resp));
    endrule


    (* descending_urgency = "pRsTransfer, cRqTransfer_retry, cRqTransfer_new, createPrefetchRq" *)
    (* descending_urgency = "pRqTransfer, cRqTransfer_retry, cRqTransfer_new, createPrefetchRq" *)
    rule createPrefetchRq(flushDone);
        Addr addr <- prefetcher.getNextPrefetchAddr;
        procRqT r = ProcRq {
            id: ?, //Or maybe do 0 here
            addr: addr,
            toState: S,
            op: Ld,
            byteEn: ?,
            data: ?,
            amoInst: ?,
            loadTags: ?,
            pcHash: ?
        };
        cRqIdxT n <- cRqMshr.cRqTransfer.getEmptyEntryInit(r);
        // send to pipeline
        pipeline.send(CRq (L1PipeRqIn {
            addr: r.addr,
            mshrIdx: n
        }));
        cRqIsPrefetch[n] <= True;
        // performance counter: cRq type
       if (verbose)
        $display("%t L1 %m createPrefetchRq: ", $time,
            fshow(n), " ; ",
            fshow(r)
        );
    endrule

`ifdef SECURITY_CACHES
    // start flush when cRq MSHR is empty
    rule startFlushReq(!flushDone && !flushReqStart && cRqMshr.emptyForFlush);
        flushReqStart <= True;
    endrule

    (* descending_urgency = "pRsTransfer, flushTransfer" *)
    (* descending_urgency = "pRqTransfer, flushTransfer" *)
    rule flushTransfer(!flushDone && flushReqStart && !flushReqDone);
        // We allocate a pRq MSHR entry for 2 reasons:
        // (1) reuse the pRq logic to send resp to parent
        // (2) control the number of downgrade resp to avoid stalling the cache
        // pipeline
        pRqIdxT n <- pRqMshr.getEmptyEntryInit(PRqMsg {
            addr: ?,
            toState: I,
            child: ?
        });
        pipeline.send(Flush (L1PipeFlushIn {
            index: flushIndex,
            way: flushWay,
            mshrIdx: n
        }));
        // increment flush index/way
        if (flushWay < fromInteger(valueof(wayNum) - 1)) begin
            flushWay <= flushWay + 1;
        end
        else begin
            flushWay <= 0;
            flushIndex <= flushIndex + 1; // index num should be power of 2
            if (flushIndex == maxBound) begin
                flushReqDone <= True;
            end
        end
       if (verbose)
        $display("%t L1 %m flushTransfer: ", $time, fshow(n), " ; ",
                 fshow(flushIndex), " ; ", fshow(flushWay));
    endrule
`endif

    rule sendRsToP_cRq(rsToPIndexQ.first matches tagged CRq .n);
        rsToPIndexQ.deq;
        // get cRq replacement info
        procRqT req = cRqMshr.sendRsToP_cRq.getRq(n);
        cRqSlotT slot = cRqMshr.sendRsToP_cRq.getSlot(n);
        Maybe#(Line) data = cRqMshr.sendRsToP_cRq.getData(n);
        L1CRqState state = cRqMshr.sendRsToP_cRq.getState(n);
        doAssert(state == WaitNewTag,
            "send replacement resp to parent, state should be WaitNewTag"
        );
        // send resp to parent
        cRsToPT resp = CRsMsg {
            addr: {slot.repTag, truncate(req.addr)}, // get bank id & index from req
            toState: I,
            data: data,
            child: ?
        };
        rsToPQ.enq(resp);
        // req parent for upgrade & change state
        rqToPIndexQ_sendRsToP.enq(n);
        cRqMshr.sendRsToP_cRq.setWaitSt_setSlot_clearData(n, L1CRqSlot {
            way: slot.way,
            cs: I, // replacement, so I (get ready for rqToIndex.deq)
            repTag: ?,
            waitP: True // we have req parent at the same time
        });
        // inform processor of line eviction
        procResp.evict(getLineAddr(resp.addr));
       if (verbose)
        $display("%t L1 %m sendRsToP: ", $time,
            fshow(rsToPIndexQ.first)," ; ",
            fshow(req), " ; ",
            fshow(resp)
        );
    endrule

    rule sendRsToP_pRq(rsToPIndexQ.first matches tagged PRq .n);
        rsToPIndexQ.deq;
        // get pRq info & send resp & release MSHR entry
        pRqFromPT req = pRqMshr.sendRsToP_pRq.getRq(n);
        Maybe#(Line) data = pRqMshr.sendRsToP_pRq.getData(n);
        cRsToPT resp = CRsMsg {
            addr: req.addr,
            toState: req.toState,
            data: data,
            child: ?
        };
        rsToPQ.enq(resp);
        pRqMshr.sendRsToP_pRq.releaseEntry(n); // mshr entry released
        // inform processor of line eviction
        procResp.evict(getLineAddr(resp.addr));
       if (verbose)
        $display("%t L1 %m sendRsToP: ", $time,
            fshow(rsToPIndexQ.first), " ; ",
            fshow(req), " ; ",
            fshow(resp)
        );
    endrule

    (* descending_urgency = "sendRqToP, sendPrefetchRqToP" *)
    rule sendPrefetchRqToP;
        let addr <- llcPrefetcher.getNextPrefetchAddr;
        cRqToPT cRqToP = CRqMsg {
            addr: addr,
            fromState: ?,
            toState: S,
            canUpToE: True,
            id: 0,
            child: ?,
            isPrefetchRq: True
        };
        rqToPQ.enq(cRqToP);
        if (verbose)
            $display("%t L1 %m sendPrefetchRqToP: ", $time,
                fshow(cRqToP)
            );
    endrule
    rule sendRqToP;
        rqToPIndexQ.deq;
        cRqIdxT n = rqToPIndexQ.first;
        procRqT req = cRqMshr.sendRqToP.getRq(n);
        cRqSlotT slot = cRqMshr.sendRqToP.getSlot(n);
        cRqToPT cRqToP = CRqMsg {
            addr: req.addr,
            fromState: slot.cs,
            toState: req.toState,
            canUpToE: True,
            id: slot.way,
            child: ?,
            isPrefetchRq: False
        };
        rqToPQ.enq(cRqToP);
       if (verbose)
        $display("%t L1 %m sendRqToP: ", $time,
            fshow(n), " ; ",
            fshow(req), " ; ",
            fshow(slot), " ; ",
            fshow(cRqToP)
        );
        // performance counter: start miss timer
        latTimer.start(n);
    endrule

    // last stage of pipeline: process req

    // XXX: in L1, pRq cannot exist in dependency chain
    // because there are only two ways to include pRq into chain
    // (1) append to a cRq that could finish, but such cRq must have been directly reponded
    // (2) overtake cRq (S->M), but such downgrade can be done instaneously without the need of chaining
    // Thus, dependency chain in L1 only contains cRq

    // pipeline outputs
    pipeOutT pipeOut = pipeline.first;
    ramDataT ram = pipeOut.ram;
    // figure out procRq MSHR idx in pipeline output (since there is only one
    // port to select from MSHR)
    cRqIdxT pipeOutCRqIdx = (case(pipeOut.cmd) matches
        tagged L1CRq .n: (n);
        default: (fromMaybe(0, ram.info.owner)); // L1PRs and L1PRq
    endcase);
    procRqT pipeOutCRq = cRqMshr.pipelineResp.getRq(pipeOutCRqIdx);
    L1CRqState pipeOutCState = cRqMshr.pipelineResp.getState(pipeOutCRqIdx);
    cRqSlotT pipeOutCSlot = cRqMshr.pipelineResp.getSlot(pipeOutCRqIdx);
    Maybe#(cRqIdxT) pipeOutSucc = cRqMshr.pipelineResp.getSucc(pipeOutCRqIdx);
    Maybe#(cRqIdxT) pipeOutNextInQueue = pipeOut.setAuxData;
    Maybe#(cRqIdxT) pipeOutSecondInQueue = isValid(pipeOutNextInQueue) ? cRqMshr.pipelineResp.getSucc2(fromMaybe(?, pipeOutNextInQueue)) : Invalid;

    // function to process cRq hit (MSHR slot may have garbage)
    function Action cRqHit(cRqIdxT n, procRqT req);
    action
       if (verbose)
        $display("%t L1 %m pipelineResp: Hit func: ", $time,
            fshow(n), " ; ",
            fshow(req)
        );
        // check tag & cs: even this function is called by pRs, tag should match,
        // because tag is written into cache before sending req to parent
        doAssert(ram.info.tag == getTag(req.addr) && enoughCacheState(ram.info.cs, req.toState),
            "cRqHit but tag or cs incorrect"
        );
        // process req: resp processor and get new cache line
        // TODO when we have MESI, cache state may also need update
        Line curLine = ram.line;
        Line newLine = curLine;
        LineMemDataOffset dataSel = getLineMemDataOffset(req.addr);
        case(req.op) matches
            Ld: begin
                if (!cRqIsPrefetch[n]) begin
                    if (req.loadTags) begin
                        procResp.respLd(req.id, getTagsAt(curLine));
                    end else begin
                        procResp.respLd(req.id, getTaggedDataAt(curLine, dataSel));
                    end
                end
            end
            Lr: begin
                procResp.respLrScAmo(req.id, getTaggedDataAt(curLine, dataSel));
                // set link addr
                linkAddr <= Valid (getLineAddr(req.addr));
            end
            Amo: begin
                noAction;
            end
            Sc: begin
                // check Sc succeeds or not
                Bool succeed = linkAddr == Valid (getLineAddr(req.addr));
                // resp to proc
                MemTaggedData respVal = succeed ? fromInteger(valueof(ScSuccVal)) : fromInteger(valueof(ScFailVal));
                procResp.respLrScAmo(req.id, respVal);
                // calculate new data to write
                if(succeed) begin
                    let taggedData = getTaggedDataAt(curLine, dataSel);
                    let newTaggedData =
                      mergeMemTaggedDataBE(taggedData, req.data, zeroExtend(pack(req.byteEn)));
                    newLine = setTaggedDataAt( newLine, dataSel, newTaggedData);
                end
                // reset link addr
                linkAddr <= Invalid;
            end
            St: begin
                // resp processor, get write data & BE
                let {be, wrLine} <- procResp.respSt(req.id);
                // calculate new data to write
                newLine = getUpdatedLine(curLine, be, wrLine);
            end
            default: begin
                doAssert(False, "unknown mem op");
            end
        endcase
        // deq pipeline or swap in successor ONLY when not AMO: to cut critical
        // path for AMO
        Maybe#(cRqIdxT) succ = pipeOutSucc;
        if(req.op != Amo) begin
            pipeline.deqWrite(succ, RamData {
                info: CacheInfo {
                    tag: getTag(req.addr), // should be the same as original tag
                    // use max here. ram.info.cs > req.toState is possible in
                    // may cache hit cases (e.g., req S and hit in M).
                    // req.toState > ram.info.cs is also possible in case of
                    // req M and hit E.
                    cs: max(ram.info.cs, req.toState),
                    dir: ?,
                    owner: succ,
                    other: ?
                },
                line: newLine // write new data into cache
            }, isValid(succ) ? pipeOutNextInQueue : pipeOutSecondInQueue, True); // hit, so update rep info
            if (!isValid(succ) &&& pipeOutNextInQueue matches tagged Valid .nextInQueue) begin
                $display("%t L1D dequeuing queued req: mshr: %d, queueSucc: ",
                    $time,
                    nextInQueue,
                    fshow(pipeOutSecondInQueue)
                );
                cRqRetryIndexQ.enq(nextInQueue);
                cRqMshr.manageQueue.resetEntry(nextInQueue);
            end
            if (!cRqIsPrefetch[n]) begin
                prefetcher.reportAccess(req.addr, req.pcHash, HIT);
                llcPrefetcher.reportAccess(req.addr, req.pcHash, HIT);
            end
           if (verbose)
            $display("%t L1 %m pipelineResp: Hit func: update ram: ", $time,
                fshow(newLine), " ; ",
                fshow(succ)
            );
            // release MSHR entry
            cRqMshr.pipelineResp.releaseEntry(n);
        end
        else begin
            processAmo <= Valid (AmoHitInfo {
                n: n,
                req: req,
                succ: succ,
                nextInQueue: pipeOutNextInQueue
            });
           if (verbose)
            $display("%t L1 %m pipelineResp: Hit func: AMO process in next cycle", $time);
        end
    endaction
    endfunction

    rule doProcessAmo(processAmo matches tagged Valid .amoHit);
        // extract amo req
        cRqIdxT n = amoHit.n;
        procRqT req = amoHit.req;
        Maybe#(cRqIdxT) succ = amoHit.succ;
        // get line and sel
        Line curLine = ram.line;
        Line newLine = curLine;
        LineMemDataOffset dataSel = getLineMemDataOffset(req.addr);
        MemTaggedData current = getTaggedDataAt(curLine, dataSel);
        Vector#(2, Bit#(64)) dwordData = current.data;
        Vector#(4, Bit#(32))  wordData = unpack(pack(current.data));
        Bit#(1) dwordIdx = req.addr[3]; 
        Bit#(2)  wordIdx = req.addr[3:2];
        // resp processor
        MemTaggedData resp = case (req.amoInst.width)
          QWord: current;
          DWord: MemTaggedData {
            tag: False,
            data: unpack(signExtend(dwordData[dwordIdx]))
          };
          Word: MemTaggedData {
            tag: False,
            data: unpack(signExtend(wordData[wordIdx]))
          };
        endcase;
        procResp.respLrScAmo(req.id, resp);
        // calculate new data to write
        let newData = amoExec(req.amoInst, wordIdx, current, req.data);
        newLine = setTaggedDataAt(newLine, dataSel, newData);
        // deq pipeline or swap in successor
        pipeline.deqWrite(succ, RamData {
            info: CacheInfo {
                tag: getTag(req.addr), // should be the same as original tag
                cs: M, // AMO always gets to M
                dir: ?,
                owner: succ,
                other: ?
            },
            line: newLine // write new data into cache
        }, amoHit.nextInQueue, True); // hit, so update rep info
        doAssert(req.toState == M, "AMO must req for M");
       if (verbose)
        $display("%t L1 %m processAmo: update ram: ", $time,
            fshow(newLine), " ; ",
            fshow(succ)
        );
        // release MSHR entry
        cRqMshr.pipelineResp.releaseEntry(n);
        // reset state
        processAmo <= Invalid;
    endrule

    rule pipelineResp_cRq(!isValid(processAmo) &&& pipeOut.cmd matches tagged L1CRq .n);
       if (verbose)
        $display("%t L1 %m pipelineResp: ", $time, fshow(pipeOut));

        procRqT procRq = pipeOutCRq;
       if (verbose)
        $display("%t L1 %m pipelineResp: cRq: ", $time, fshow(n), " ; ", fshow(procRq));

        // find end of dependency chain
        Maybe#(cRqIdxT) cRqDependEOC = cRqMshr.pipelineResp.searchDependEndOfChain(procRq.addr);
        Maybe#(cRqIdxT) cRqQueuedEOC = cRqMshr.pipelineResp.searchQueuedEndOfChain(procRq.addr);

        function Action cRqScEarlyFail(Bool resetOwner);
        action
            // resp to proc
            procResp.respLrScAmo(procRq.id, fromInteger(valueof(ScFailVal)));
            // reset link addr
            linkAddr <= Invalid;
            // deq pipeline (we cannot swap in successor because Sc may not
            // occupy a line). We don't touch cache contents, but we may reset
            // line owner.
            pipeline.deqWrite(Invalid, RamData {
                info: CacheInfo {
                    tag: ram.info.tag,
                    cs: ram.info.cs,
                    dir: ram.info.dir,
                    owner: resetOwner ? Invalid : ram.info.owner,
                    other: ram.info.other
                },
                line: ram.line
            }, pipeOutNextInQueue, False);
            // retry successor
            Maybe#(cRqIdxT) succ = pipeOutSucc;
            if(succ matches tagged Valid .s) begin
                cRqRetryIndexQ.enq(s);
            end
            // release MSHR entry
            cRqMshr.pipelineResp.releaseEntry(n);
           if (verbose)
            $display("%t L1 %m pipelineResp: Sc early fail func: ", $time,
                fshow(resetOwner), " ; ",
                fshow(succ)
            );
        endaction
        endfunction

        // function to process cRq miss without replacement (MSHR slot may have garbage)
        function Action cRqMissNoReplacement;
        action
            cRqSlotT cSlot = pipeOutCSlot;
            // it is impossible in L1 to have slot.waitP == True in this function
            // because cRq is not set to Depend when pRq invalidates it (pRq just directly resp)
            // and this func is only called when cs < toState (otherwise will hit)
            // because L1 has no children to wait for
            doAssert(!cSlot.waitP && !enoughCacheState(ram.info.cs, procRq.toState),
                "waitP must be false and cs must not be enough"
            );
            // Thus we must send req to parent
            // XXX first send to a temp indexQ to avoid conflict, then merge to rqToPIndexQ later
            rqToPIndexQ_pipelineResp.enq(n);
            // update mshr
            cRqMshr.pipelineResp.setStateSlot(n, WaitSt, L1CRqSlot {
                way: pipeOut.way, // use way from pipeline
                cs: ram.info.cs, // record cs for future rqToPIndexQ.deq
                repTag: ?, // no replacement
                waitP: True // we have req parent, so waiting
            });
            // deq pipeline & set owner, tag
            pipeline.deqWrite(Invalid, RamData {
                info: CacheInfo {
                    tag: getTag(procRq.addr), // tag may be garbage if cs == I
                    cs: ram.info.cs,
                    dir: ?,
                    owner: Valid (n), // owner is req itself
                    other: ?
                },
                line: ram.line
            }, pipeOutNextInQueue, False);
            if (!cRqIsPrefetch[n]) begin
                prefetcher.reportAccess(procRq.addr, procRq.pcHash, MISS);
                llcPrefetcher.reportAccess(procRq.addr, procRq.pcHash, MISS);
            end
        endaction
        endfunction

        // function to do replacement for cRq
        function Action cRqReplacement;
        action
            // deq pipeline
            pipeline.deqWrite(Invalid, RamData {
                info: CacheInfo {
                    tag: getTag(procRq.addr), // set to req tag (old tag is replaced right now)
                    cs: I,
                    dir: ?,
                    owner: Valid (n), // owner is req itself
                    other: ?
                },
                line: ? // data is no longer used
            }, pipeOutNextInQueue, False);
            // update MSHR: may save replaced line data
            cRqMshr.pipelineResp.setStateSlot(n, WaitNewTag, L1CRqSlot {
                way: pipeOut.way, // use way from pipeline
                cs: I,
                repTag: ram.info.tag, // tag being replaced
                waitP: False // we send req to parent later (when resp to parent is sent)
            });
            cRqMshr.pipelineResp.setData(n, ram.info.cs == M ? Valid (ram.line) : Invalid);
            if (!cRqIsPrefetch[n]) begin
                prefetcher.reportAccess(procRq.addr, procRq.pcHash, MISS);
                llcPrefetcher.reportAccess(procRq.addr, procRq.pcHash, MISS);
            end
            // send replacement resp to parent
            rsToPIndexQ.enq(CRq (n));
            // reset link addr
            LineAddr repLineAddr = getLineAddr({ram.info.tag, truncate(procRq.addr)}); // index & bank are from procRq
            if(linkAddr == Valid (repLineAddr)) begin
                linkAddr <= Invalid;
            end
        endaction
        endfunction

        // function to set cRq to Depend, and make no further change to cache
        function Action cRqSetDepNoCacheChange;
        action
            cRqMshr.pipelineResp.setStateSlot(n, Depend, defaultValue);
            pipeline.deqWrite(Invalid, pipeOut.ram, pipeOutNextInQueue, False);
        endaction
        endfunction

        function Action cRqQueue;
        action
            cRqMshr.pipelineResp.setStateSlot(n, Queued, defaultValue);
            pipeline.deqWrite(Invalid, pipeOut.ram, Valid(fromMaybe(n, pipeOutNextInQueue)), False);
        endaction
        endfunction

        function Action cRqDrop;
        action
            cRqMshr.pipelineResp.releaseEntry(n);
            crqMshrDeqs <= crqMshrDeqs + 1;
            pipeline.deqWrite(Invalid, pipeOut.ram, pipeOutNextInQueue, False);
        endaction
        endfunction

        // check if Sc fails early. If an Sc needs to req parent but it's addr
        // does not match link addr, we can directly respond the Sc with
        // failure, and thus avoid requesting parent.
        Bool scFail = procRq.op == Sc && linkAddr != Valid (getLineAddr(procRq.addr));
        // check tag match
        Bool tag_match = ram.info.tag == getTag(procRq.addr);
        // check enough cache state for hit
        Bool enough_cs_to_hit = enoughCacheState(ram.info.cs, procRq.toState);
        // check if cs is not I
        Bool cs_valid = ram.info.cs > I;
        if(ram.info.owner matches tagged Valid .cOwner) begin
            if(cOwner != n) begin
                doAssert(pipeOutCState == Init, "must first time go through tag match");
                // If the tags match then we want to add to a dependency chain
                if (tag_match) begin
                    // should be added to a cRq in dependency chain & deq from pipeline
                    doAssert(isValid(cRqDependEOC), ("cRq hit on another cRq, cRqDependEOC must be true"));
                    // If this is a prefetch, we can drop the prefetch here (prefetch was probably late)
                    if (cRqIsPrefetch[n]) begin
                        cRqDrop;
                    end else begin
                        cRqMshr.pipelineResp.setSucc(fromMaybe(?, cRqDependEOC), Valid (n));
                        cRqSetDepNoCacheChange;
                    end
                    if (verbose)
                        $display("%t L1 %m pipelineResp: cRq: own by other cRq ", $time,
                            fshow(cOwner), ", depend on cRq ", fshow(cRqDependEOC)
                        );
                end 
                // If the tags don't match then we want to queue the cRq
                else begin
                    // if there was an option to add a dependency, L1Pipe should have found it
                    doAssert(!isValid(cRqDependEOC), ("end of chain is valid but the chosen way did not match tags"));
                    if (cRqIsPrefetch[n]) begin
                        cRqDrop;
                    end else begin
                        if (cRqQueuedEOC matches tagged Valid .eoc) begin
                            cRqMshr.pipelineResp.setSucc(eoc, Valid (n));
                        end
                        cRqQueue;
                    end
                end
            end
            else begin
                // owner is myself, so must be swapped in
                // tag should match, since always swapped in by cRq, cs > I
                doAssert(pipeOutCState == Depend, "must be swapped in");
                doAssert(cs_valid && tag_match,
                    "cRq swapped in by previous cRq, tag must match & cs > I"
                );
                // Hit or Miss (but no replacement)
                if(enough_cs_to_hit) begin
                   if (verbose)
                    $display("%t L1 %m pipelineResp: cRq: own by itself, hit", $time);
                    cRqHit(n, procRq);
                end
                else if(scFail) begin
                    // Sc already fails, so we don't need to req parent.  Since
                    // Sc is the owner of the line, we need to reset owner to
                    // Invalid.
                   if (verbose)
                    $display("%t L1 %m pipelineResp: cRq: own by itself, Sc early fails, ",
                        $time, fshow(linkAddr)
                    );
                    cRqScEarlyFail(True);
                end
                else begin
                  if (verbose)
                   $display("%t L1 %m pipelineResp: cRq: own by itself, miss no replace", $time);
                  cRqMissNoReplacement;
                end
            end
        end
        else begin
            // cache has no owner, cRq must just go through tag match
            // Here are two cases:
            // 1. cRq in Init state, first time go through tag match
            // 2. cRq addr-depends on an Sc which fails early, just got waken up
            L1CRqState cState = pipeOutCState;

            // There should be no dependency
            doAssert(!isValid(cRqDependEOC), "end of chain is valid but the chosen way is not owned");
            
            // Check hit or miss, replacment may be needed
            if(tag_match && enough_cs_to_hit) begin
                // Hit
                doAssert(cs_valid, "hit, so cs must > I");
                if (verbose)
                $display("%t L1 %m pipelineResp: cRq: no owner, hit", $time);
                cRqHit(n, procRq, False);
            end
            else if(scFail) begin
                // Sc already fails, so we don't need to req parent.  Since
                // there is no owner of the line, we can reset owner to
                // Invalid.
                if (verbose)
                $display("%t L1 %m pipelineResp: cRq: no owner, Sc early fails, ",
                    $time, fshow(linkAddr)
                );
                cRqScEarlyFail(True);
            end
            else if(cs_valid && !tag_match) begin
                // Req parent, need replacement
                if (verbose)
                $display("%t L1 %m pipelineResp: cRq: no owner, replace", $time);
                cRqReplacement;
            end
            else begin
                if (verbose)
                $display("%t L1 %m pipelineResp: cRq: no owner, miss no replace", $time);
                // Req parent, no replacement needed
                cRqMissNoReplacement;
            end
        end
    endrule

    rule pipelineResp_pRs(!isValid(processAmo) &&& pipeOut.cmd == L1PRs);
       if (verbose) begin
        $display("%t L1 %m pipelineResp: ", $time, fshow(pipeOut));
        $display("%t L1 %m pipelineResp: pRs: ", $time);
       end

        if(ram.info.owner matches tagged Valid .cOwner) begin
            procRqT procRq = pipeOutCRq;
            doAssert(ram.info.cs >= procRq.toState && ram.info.tag == getTag(procRq.addr),
                ("pRs must be a hit")
            );
            cRqHit(cOwner, procRq);
            // performance counter: miss cRq
            if (!cRqIsPrefetch[cOwner]) begin
                incrMissCnt(procRq.op, cOwner);
            end
        end
        else begin
            doAssert(False, ("pRs owner must match some cRq"));
        end
    endrule

    rule pipelineResp_pRq(!isValid(processAmo) &&& pipeOut.cmd matches tagged L1PRq .n);
        pRqFromPT pRq = pRqMshr.pipelineResp.getRq(n);
       if (verbose)
        $display("%t L1 %m pipelineResp: pRq: ", $time, fshow(n), " ; ", fshow(pRq));

        // pRq is never in dependency chain, so it is never swapped in
        // pRq must go through tag match, which either returns a tag matched way or asserts pRqMiss
        // and pRq is always directly handled: either dropped or Done

        if(pipeOut.pRqMiss || ram.info.cs <= pRq.toState || ram.info.tag != getTag(pRq.addr)) begin
           if (verbose)
            $display("%t L1 %m pipelineResp: pRq: drop", $time);
            // pRq can be directly dropped
            // must go through tag match, no successor
            pRqMshr.pipelineResp.releaseEntry(n);
            pipeline.deqWrite(Invalid, pipeOut.ram, pipeOutNextInQueue, False);
            // sanity check (ram.info.tag != getTag(pRq.addr) is useless)
            if(!pipeOut.pRqMiss) begin
                doAssert(ram.info.cs == S && pRq.toState == S && ram.info.tag == getTag(pRq.addr),
                    ("pRqMiss deasserted, must be down to S")
                );
            end
        end
        else if(ram.info.owner matches tagged Valid .cOwner) begin
            procRqT cRq = pipeOutCRq;
            // must be the case the pRq overtakes cRq
            L1CRqState cState = pipeOutCState;
            cRqSlotT cSlot = pipeOutCSlot;
           if (verbose)
            $display("%t L1 %m pipelineResp: pRq: overtake cRq: ", $time,
                fshow(cOwner), " ; ",
                fshow(cRq), " ; ",
                fshow(cState), " ; ",
                fshow(cSlot)
            );
            doAssert(ram.info.cs == S && cRq.toState > S && pRq.toState == I && cState == WaitSt && cSlot.waitP,
                ("pRq overtakes CRq")
            );
            // process pRq
            pRqMshr.pipelineResp.setDone_setData(n, Invalid); // S->I, no data needed
            pipeline.deqWrite(Invalid, RamData {
                info: CacheInfo {
                    tag: ram.info.tag, // keep tag the same (for sake of cRq)
                    cs: I, // downgraded to I
                    dir: ?,
                    owner: ram.info.owner, // keep owner to cRq
                    other: ?
                },
                line: ram.line
            }, pipeOutNextInQueue, False);
            rsToPIndexQ.enq(PRq (n));
            // update cRq bookkeeping
            cRqMshr.pipelineResp.setStateSlot(cOwner, WaitSt, L1CRqSlot {
                way: pipeOut.way,
                cs: I, // update cs (actually useless because there cannot be any future pRq)
                repTag: ?,
                waitP: True
            });
        end
        else begin
           if (verbose)
            $display("%t L1 %m pipelineResp: pRq: valid process", $time);
            // line must NOT be owned
            doAssert(ram.info.owner == Invalid,
                ("pRq cannot hit on line owned by pRq (even itself)")
            );
            // should process pRq
            doAssert(ram.info.cs > pRq.toState && ram.info.tag == getTag(pRq.addr),
                ("pRq should be processed")
            );
            pRqMshr.pipelineResp.setDone_setData(n, ram.info.cs == M ? Valid (ram.line) : Invalid);
            pipeline.deqWrite(Invalid, RamData {
                info: CacheInfo {
                    tag: ram.info.tag,
                    cs: pRq.toState,
                    dir: ?,
                    owner: Invalid, // no successor
                    other: ?
                },
                line: ram.line
            }, pipeOutSecondInQueue, False);
            if (pipeOutNextInQueue matches tagged Valid .nextInQueue) begin
                $display("%t L1D dequeuing queued req: mshr: %d, queueSucc: ",
                    $time,
                    nextInQueue,
                    fshow(pipeOutSecondInQueue)
                );
                cRqRetryIndexQ.enq(nextInQueue);
                cRqMshr.manageQueue.resetEntry(nextInQueue);
            end
            rsToPIndexQ.enq(PRq (n));
        end

        // since pRq is always processed in one shot, we reset link addr here together
        if(linkAddr == Valid (getLineAddr(pRq.addr)) && pRq.toState == I) begin
            linkAddr <= Invalid;
        end
    endrule

`ifdef SECURITY_CACHES
    rule pipelineResp_flush(
        !isValid(processAmo) &&&
        !flushDone &&& !flushRespDone &&&
        pipeOut.cmd matches tagged L1Flush .flush
    );
        pRqIdxT n = flush.mshrIdx;
       if (verbose)
        $display("%t L1 %m pipelineResp: flush: ", $time, fshow(flush));

        // During flush, cRq MSHR is empty, so cache line cannot have owner
        doAssert(ram.info.owner == Invalid, "flushing line cannot have owner");
        doAssert(pipeOutNextInQueue == Invalid, "flushing line cannot have queued requests");

        // flush always goes through cache pipeline, and is directly handled
        // here: either dropped or Done
        if(ram.info.cs == I) begin
           if (verbose)
            $display("%t L1 %m pipelineResp: flush: drop", $time);
            // flush can be directly dropped
            pRqMshr.pipelineResp.releaseEntry(n);
        end
        else begin
           if (verbose)
            $display("%t L1 %m pipelineResp: flush: valid process", $time);
            pRqMshr.pipelineResp.setDone_setData(n, ram.info.cs == M ? Valid (ram.line) : Invalid);
            rsToPIndexQ.enq(PRq (n));
            // record the flushed addr in MSHR so that sendRsToP rule knows
            // which addr is invalidated
            Bit#(LgLineSzBytes) offset = 0;
            Addr addr = {ram.info.tag, flush.index, bankId, offset};
            pRqMshr.pipelineResp.setFlushAddr(n, addr);
        end

        // always clear the cache line
        pipeline.deqWrite(Invalid, RamData {
            info: CacheInfo {
                tag: ?,
                cs: I, // downgraded to I
                dir: ?,
                owner: Invalid, // no successor
                other: ?
            },
            line: ?
        }, Invalid, False);

        // always reset link addr
        linkAddr <= Invalid;

        // check if we have finished all flush
        if (flush.index == maxBound &&
            pipeOut.way == fromInteger(valueof(wayNum) - 1)) begin
            flushRespDone <= True;
        end
    endrule

    rule completeFlush(!flushDone && flushReqStart && flushReqDone && flushRespDone);
        flushDone <= True;
        flushReqStart <= False;
        flushReqDone <= False;
        flushRespDone <= False;
    endrule
`endif

    // merge rq to parent index into indexQ
    rule rqIndexFromPipelineResp;
        let n <- toGet(rqToPIndexQ_pipelineResp).get;
        rqToPIndexQ.enq(n);
    endrule

    (* descending_urgency = "rqIndexFromPipelineResp, rqIndexFromSendRsToP" *)
    rule rqIndexFromSendRsToP;
        let n <- toGet(rqToPIndexQ_sendRsToP).get;
        rqToPIndexQ.enq(n);
    endrule

    interface ChildCacheToParent to_parent;
        interface rsToP = toFifoDeq(rsToPQ);
        interface rqToP = toFifoDeq(rqToPQ);
        interface fromP = toFifoEnq(fromPQ);
    endinterface

    interface L1ProcReq procReq;
        method Action req(procRqT r);
            rqFromCQ.enq(r);
        endmethod
    endinterface

    method Action resetLinkAddr;
        linkAddrRst <= Invalid;
    endmethod

    interface Get cRqStuck;
        method ActionValue#(L1CRqStuck) get;
            let s <- cRqMshr.stuck.get;
            return L1CRqStuck {
                addr: s.req.addr,
                op: s.req.op,
                state: s.state,
                slotCs: s.slotCs,
                waitP: s.waitP
            };
        endmethod
    endinterface

    interface pRqStuck = pRqMshr.stuck;

`ifdef SECURITY_CACHES
    method Action flush if(flushDone);
        flushDone <= False;
    endmethod
    method flush_done = flushDone._read;
`else
    method flush = noAction;
    method flush_done = True;
`endif

    method Action setPerfStatus(Bool stats);
`ifdef PERF_COUNT
        doStats <= stats;
`else
        noAction;
`endif
    endmethod

    method Data getPerfData(L1DPerfType t);
        return (case(t)
`ifdef PERF_COUNT
            L1DLdCnt: ldCnt;
            L1DStCnt: stCnt;
            L1DAmoCnt: amoCnt;
            L1DLdMissCnt: ldMissCnt;
            L1DStMissCnt: stMissCnt;
            L1DAmoMissCnt: amoMissCnt;
            L1DLdMissLat: ldMissLat;
            L1DStMissLat: stMissLat;
            L1DAmoMissLat: amoMissLat;
`endif
            default: 0;
        endcase);
    endmethod
`ifdef PERFORMANCE_MONITORING
    method EventsL1D events = perf_events[0];
`endif
endmodule


// Scheduling note

// cRqTransfer: write new cRq MSHR entry, cRqMshr.getEmptyEntry

// pRqTransfer: write new pRq MSHR entry, pRqMshr.getEmptyEntry

// pRsTransfer: -

// sendRsToP_cRq: access cRq MSHR entry that is sending replacement
// -- read req/state/slot/data
// -- write state/slot/data

// sendRsToP_pRq: read pRq MSHR entry that is responding, pRqMshr.releaseEntry

// sendRqToP: read cRq MSHR req/slot that is requesting parent

// pipelineResp_cRq:
// -- cRqMshr.releaseEntry
// -- read cRq MSHR req/state/slot/data currently processed
// -- write cRq MSHR state/slot/data currently processed
// -- write cRq MSHR state/succ of some existing cRq MSHR entry (in Queued)
// -- write succ of some existing cRq MSHR entry (in WaitNewTag or WaitSt)
// -- read all state/req/succ in cRq MSHR entry (searchDependEOC and searchQueueEOC)
//    -- not affected by write in cRqTransfer
//    -- not affected by write in sendRsToP_cRq (state change is just WaitNewTag->WaitSt)

// pipelineResp_pRs:
// -- cRqMshr.releaseEntry
// -- read cRqMSHR req/succ

// pipelineResp_pRq:
// -- r/w pRq MSHR entry, pRqMshr.releaseEntry
// -- read existing cRq state/req/slot, write its slot/state
//    -- cannot be the cRq in sendRsToP_cRq

// ---- conflict analysis ----

// XXXTransfer is conflict with each other
// but they are access new MSHR entry, so never conflict with other rules

// sendxxxToP is conflict free with each other

// sendRsToP_cRq conflict free with pipelineResp_xxx in terms of MSHR
// -- writes in sendRsToP_cRq does not affect r&w in pipelineResp_xxx
// -- writes in pipelineResp_xxx never affects r/w in sendRsToP_cRq
// BUT they may both enq to rqToPIndexQ

// sendRsToP_pRq conflict free with pipelineResp_xxx

// sendRqToP conflict free with pipelineResp_xxx

// pipelineResp_cRq conflicts with itself, potentially writing state/succ of multiple entries

// ---- conclusion ----

// we have 5 ports from cRq MSHR
// 1. cRqTransfer
// 2. sendRsToP_cRq
// 3. sendRqToP
// 4. pipelineResp
// 5. manageQueue

// we have 3 ports from pRq MSHR
// 1. pRqTransfer
// 2. sendRsToP_pRq
// 3. pipelineResp


// unsafe version: all reads read the original reg value
// and all writes are cononicalized

// safe version: use EHR ports
// sendRsToP_cRq/sendRsToP_pRq: port 0
// pipelineResp: port 1
// manageQueue: port 2
// cRqTransfer/sendRqToP/pRqTransfer: port 3

// We put cRqTransfer at last because it is ordered after the issue method of
// LSQ outside cache, while pipelineResp and sendRsToP will call other methods
// of LSQ. Making cRqTransfer after pipelineResp/sendRsToP will not add any
// extra ordering constraint to methods of LSQ.


// group banks into cache
module mkL1Cache#(
    module#(L1CRqMshr#(cRqNum, indexT, wayT, tagT, procRqT)) mkL1CRqMshrLocal,
    module#(L1PRqMshr#(pRqNum)) mkL1PRqMshrLocal,
    module#(L1Pipe#(lgBankNum, wayNum, indexT, tagT, cRqIdxT, pRqIdxT)) mkL1Pipeline,
    L1ProcResp#(procRqIdT) procResp
)(
    L1Bank#(lgBankNum, wayNum, indexSz, tagSz, cRqNum, pRqNum, procRqIdT)
) provisos (
    NumAlias#(bankNum, TExp#(lgBankNum)),
    Alias#(bankIdT, Bit#(lgBankNum)),
    Alias#(l1BankT, L1Bank#(lgBankNum, wayNum, indexSz, tagSz, cRqNum, pRqNum, procRqIdT)),
    Alias#(wayT, Bit#(TLog#(wayNum))),
    Alias#(indexT, Bit#(indexSz)),
    Alias#(tagT, Bit#(tagSz)),
    Alias#(cRqIdxT, Bit#(TLog#(cRqNum))),
    Alias#(pRqIdxT, Bit#(TLog#(pRqNum))),
    Alias#(cacheOwnerT, Maybe#(cRqIdxT)),
    Alias#(cacheOtherT, void),
    Alias#(cacheSetAuxT, Maybe#(cRqIdxT)),
    Alias#(procRqT, ProcRq#(procRqIdT)),
    Alias#(cRqToPT, CRqMsg#(wayT, void)),
    Alias#(cRsToPT, CRsMsg#(void)),
    Alias#(pRqRsFromPT, PRqRsMsg#(wayT, void)),
    Alias#(l1CmdT, L1Cmd#(indexT, cRqIdxT, pRqIdxT)),
    Alias#(pipeOutT, PipeOut#(wayT, tagT, Msi, void, cacheOwnerT, void, RandRepInfo, Line, cacheSetAuxT, l1CmdT)),
    // requirements
    Bits#(procRqIdT, _procRqIdT),
    FShow#(procRqIdT),
    FShow#(pipeOutT),
    Add#(tagSz, a__, AddrSz),
    // make sure: cRqNum <= wayNum
    Add#(cRqNum, b__, wayNum),
    Add#(lgBankNum, c__, AddrSz),
    Add#(1, d__, bankNum),
    Add#(TAdd#(tagSz, indexSz), TAdd#(lgBankNum, LgLineSzBytes), AddrSz)
);
    // bank id of each cache bank is implicit, we always send fixed subset of address to a bank
    // the pipelineResp_cRq,pRs will conflict with each other
    Vector#(bankNum, l1BankT) banks;
    for (Integer i = 0; i < valueof(bankNum); i = i+1) begin
        banks[i] <- mkL1Bank(fromInteger(i), mkL1CRqMshrLocal, mkL1PRqMshrLocal, mkL1Pipeline, procResp);
    end

    function bankIdT getBankId(Addr a);
        return truncate(a >> valueof(LgLineSzBytes));
    endfunction

    ChildCacheToParent#(wayT, void) toParentIfc; // ifc to parent cache

    if(valueof(bankNum) == 1) begin
        toParentIfc = banks[0].to_parent;
    end
    else begin
        // multiple banks need cross bar
        Fifo#(2, cRqToPT) cRqToPQ <- mkCFFifo;
        Fifo#(2, cRsToPT) cRsToPQ <- mkCFFifo;
        Fifo#(2, pRqRsFromPT) pRqRsFromPQ <- mkCFFifo;

        function XBarDstInfo#(Bit#(0), cRqToPT) getCRqDstInfo(bankIdT bid, cRqToPT cRq);
            return XBarDstInfo {idx: 0, data: cRq};
        endfunction
        function Get#(cRqToPT) cRqGet(l1BankT ifc) = toGet(ifc.to_parent.rqToP);
        mkXBar(getCRqDstInfo, map(cRqGet, banks), vec(toPut(cRqToPQ)));

        function XBarDstInfo#(Bit#(0), cRsToPT) getCRsDstInfo(bankdIdT bid, cRsToPT cRs);
            return XBarDstInfo {idx: 0, data: cRs};
        endfunction
        function Get#(cRsToPT) cRsGet(l1BankT ifc) = toGet(ifc.to_parent.rsToP);
        mkXBar(getCRsDstInfo, map(cRsGet, banks), vec(toPut(cRsToPQ)));

        for(Integer i = 0; i < valueof(bankNum); i = i+1) begin
            rule sendPRq(pRqRsFromPQ.first matches tagged PRq .rq &&& getBankId(rq.addr) == fromInteger(i));
                let r <- toGet(pRqRsFromPQ).get;
                banks[i].to_parent.fromP.enq(r);
            endrule
            rule sendPRs(pRqRsFromPQ.first matches tagged PRs .rs &&& getBankId(rs.addr) == fromInteger(i));
                let r <- toGet(pRqRsFromPQ).get;
                banks[i].to_parent.fromP.enq(r);
            endrule
        end

        toParentIfc = (interface ChildCacheToParent;
            interface rqToP = toFifoDeq(cRqToPQ);
            interface rsToP = toFifoDeq(cRsToPQ);
            interface fromP = toFifoEnq(pRqRsFromPQ);
        endinterface);
    end

`ifdef CHECK_DEADLOCK
    FIFO#(L1CRqStuck) cRqStuckQ <- mkFIFO1;
    FIFO#(L1PRqStuck) pRqStuckQ <- mkFIFO1;

    for(Integer i = 0; i < valueof(bankNum); i = i+1) begin
        rule sendCRqStuck;
            let s <- banks[i].cRqStuck.get;
            cRqStuckQ.enq(s);
        endrule
        rule sendPRqStuck;
            let s <- banks[i].pRqStuck.get;
            pRqStuckQ.enq(s);
        endrule
    end

    interface cRqStuck = toGet(cRqStuckQ);
    interface pRqStuck = toGet(pRqStuckQ);
`else
    interface cRqStuck = nullGet;
    interface pRqStuck = nullGet;
`endif

    interface ChildCacheToParent to_parent = toParentIfc;

    interface L1ProcReq procReq;
        method Action req(procRqT r);
            banks[getBankId(r.addr)].procReq.req(r);
        endmethod
    endinterface

    method Action resetLinkAddr;
        for(Integer i = 0; i < valueof(bankNum); i = i+1) begin
            banks[i].resetLinkAddr;
        end
    endmethod

    method Action flush;
        for(Integer i = 0; i < valueof(bankNum); i = i+1) begin
            banks[i].flush;
        end
    endmethod

    method Bool flush_done;
        Vector#(bankNum, Bool) b;
        for(Integer i = 0; i < valueof(bankNum); i = i+1) begin
            b[i] = banks[i].flush_done;
        end
        return fold(\&& , b);
    endmethod

    method Action setPerfStatus(Bool stats);
        for(Integer i = 0; i < valueof(bankNum); i = i+1) begin
            banks[i].setPerfStatus(stats);
        end
    endmethod

    method Data getPerfData(L1DPerfType t);
        Vector#(bankNum, Data) d = ?;
        for(Integer i = 0; i < valueof(bankNum); i = i+1) begin
            d[i] = banks[i].getPerfData(t);
        end
        return fold(\+ , d);
    endmethod
`ifdef PERFORMANCE_MONITORING
    method EventsL1D events;
        EventsL1D ret = unpack(0);
        for(Integer i = 0; i < valueof(bankNum); i = i+1) begin
            ret = unpack(pack(ret) | pack(banks[i].events));
        end
        return ret;
    endmethod
`endif
endmodule
