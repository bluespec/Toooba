
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
import ClientServer::*;
import DefaultValue::*;
import GetPut::*;
import Types::*;
import ProcTypes::*;
import TlbTypes::*;
import Performance::*;
import FullAssocTlb::*;
import ConfigReg::*;
import Fifo::*;
import Cntrs::*;
import SafeCounter::*;
import CacheUtils::*;
import LatencyTimer::*;

// currently blocking
typedef `L1_TLB_SIZE ITlbSize;

typedef struct {
    Vpn vpn;
} ITlbRqToP deriving(Bits, Eq, FShow);

typedef struct {
    // may get page fault: i.e. hit invalid page or
    // get non-leaf page at last-level page table
    Maybe#(TlbEntry) entry; 
} ITlbRsFromP deriving(Bits, Eq, FShow);

interface ITlbToParent;
    interface FifoDeq#(ITlbRqToP) rqToP;
    interface FifoEnq#(ITlbRsFromP) rsFromP;
    // after ITLB flush itself, it notifies L2, and wait L2 to flush
    interface Client#(void, void) flush;
endinterface

interface ITlb;
    // system consistency related
    method Bool flush_done;
    method Action flush;
    method Action updateVMInfo(VMInfo vm);
    method Bool noPendingReq;

    // req/resp with core
    interface Server#(Addr, TlbResp) to_proc;

    // req/resp with L2 TLB
    interface ITlbToParent toParent;

    // performance
    interface Perf#(L1TlbPerfType) perf;
endinterface

typedef FullAssocTlb#(ITlbSize) ITlbArray;
module mkITlbArray(ITlbArray);
    let m <- mkFullAssocTlb(True); // randomness in replacement
    return m;
endmodule

(* synthesize *)
module mkITlb(ITlb::ITlb);
    Bool verbose = True;

    // TLB array
    ITlbArray tlb <- mkITlbArray;

    // processor init flushing by setting this flag
    Reg#(Bool) needFlush <- mkReg(False);
    // after flushing ITLB itself, we want parent TLB to flush
    Reg#(Bool) waitFlushP <- mkReg(False);

    // resp FIFO to proc
    Fifo#(2, TlbResp) hitQ <- mkCFFifo;

    // current processor VM information
    Reg#(VMInfo) vm_info <- mkReg(defaultValue);

    // blocking miss
    Reg#(Maybe#(Addr)) miss <- mkReg(Invalid);

    // req & resp with parent TLB
    Fifo#(2, ITlbRqToP) rqToPQ <- mkCFFifo;
    Fifo#(2, ITlbRsFromP) rsFromPQ <- mkCFFifo;
    // flush req/resp with parent TLB
    Fifo#(1, void) flushRqToPQ <- mkCFFifo;
    Fifo#(1, void) flushRsFromPQ <- mkCFFifo;

    // perf counters
    Fifo#(1, L1TlbPerfType) perfReqQ <- mkCFFifo;
`ifdef PERF_COUNT
    Fifo#(1, PerfResp#(L1TlbPerfType)) perfRespQ <- mkCFFifo;
    Reg#(Bool) doStats <- mkConfigReg(False);
    Count#(Data) accessCnt <- mkCount(0);
    Count#(Data) missCnt <- mkCount(0);
    Count#(Data) missLat <- mkCount(0);

    LatencyTimer#(2, 12) latTimer <- mkLatencyTimer; // max latency: 4K cycles

    rule doPerf;
        let t <- toGet(perfReqQ).get;
        Data d = (case(t)
            L1TlbAccessCnt: (accessCnt);
            L1TlbMissParentCnt: (missCnt);
            L1TlbMissParentLat: (missLat);
            default: (0);
        endcase);
        perfRespQ.enq(PerfResp {
            pType: t,
            data: d
        });
    endrule
`endif

    // do flush: only start when all misses resolve
    rule doStartFlush(needFlush && !waitFlushP && !isValid(miss));
        tlb.flush;
        // request parent TLB to flush
        flushRqToPQ.enq(?);
        waitFlushP <= True;
        if(verbose) $display("ITLB %m: flush begin");
    endrule

    rule doFinishFlush(needFlush && waitFlushP && !isValid(miss));
        flushRsFromPQ.deq;
        needFlush <= False;
        waitFlushP <= False;
        if(verbose) $display("ITLB %m: flush done");
    endrule

    rule doRsFromP(miss matches tagged Valid .vaddr);
        rsFromPQ.deq;
        let pRs = rsFromPQ.first;

        if(pRs.entry matches tagged Valid .en) begin
            // TODO when we have multiple misses in future.  We first need to
            // search TLB to check whether the PTE is already in TLB; this may
            // happen for mega/giga pages.  We don't want same PTE to occupy >1
            // TLB entires.
            
            // check permission
            if(hasVMPermission(vm_info,
                               en.pteType,
                               en.ppn,
                               en.level,
                               InstFetch)) begin
                // fill TLB and resp to proc
                tlb.addEntry(en);
                let trans_addr = translate(vaddr, en.ppn, en.level);
                hitQ.enq(tuple2(trans_addr, Invalid));
                if(verbose) begin
                    $display("ITLB %m refill: ", fshow(vaddr),
                             " ; ", fshow(trans_addr));
                end
            end
            else begin
                // page fault
                hitQ.enq(tuple2(?, Valid (InstPageFault)));
                if(verbose) begin
                    $display("ITLB %m refill no permission: ", fshow(vaddr));
                end
            end
        end
        else begin
            // page fault
            hitQ.enq(tuple2(?, Valid (InstPageFault)));
            if(verbose) $display("ITLB %m refill page fault: ", fshow(vaddr));
        end
        // miss resolved
        miss <= Invalid;

`ifdef PERF_COUNT
        let lat <- latTimer.done(0);
        if(doStats) begin
            missLat.incr(zeroExtend(lat));
        end
`endif
    endrule

    // we check no pending req only at Commit when Fetch1 stage has been
    // stalled (setWaitRedirect has been called in previous cycle). Note that
    // Commit calls redirect method which is ordered after Fetch1, so checking
    // no pending req cannot sequence before Fetch1 which calls iTlb.req.
    // Therefore, we use a wire to catch the pending req a the beginning of the
    // cycle. This does not matter because Fetch1 stage has been stalled.
    Wire#(Bool) no_pending_wire <- mkBypassWire;
    (* fire_when_enabled, no_implicit_conditions *)
    rule set_no_pending;
        no_pending_wire <= !isValid(miss);
    endrule

    method Action flush if(!needFlush);
        needFlush <= True;
        waitFlushP <= False;
        // this won't interrupt current processing, since
        // (1) miss process will continue even if needFlush=True
        // (2) flush truly starts when there is no pending req
    endmethod

    method Bool flush_done = !needFlush;

    method Action updateVMInfo(VMInfo vm);
        vm_info <= vm;
    endmethod

    method Bool noPendingReq = no_pending_wire._read;

    interface Server to_proc;
        interface Put request;
            // We do not accept new req when flushing flag is set.  We also
            // make the guard more restrictive to reduce the time of computing
            // guard i.e. guard does not depend on whether TLB hit or miss
            method Action put(Addr vaddr) if(
                !needFlush && !isValid(miss) && hitQ.notFull && rqToPQ.notFull
            );
`ifdef SECURITY
                // Security Check
                // For non M-mode forbid translating any virtual pc outside of
                // the protection domain. Be careful, in M mode we don't take
                // into account the same vm_info: changing the evbase and
                // evmask in M in en security monitor would lead to failure
                // before we have a chance to go back to U: while in M mode
                // sanctum_evbase is morally from an other set of CSR, which is
                // constant and hardcoded in our implementation.

                // TODO TODO TODO TODO WARNING DANGER NO SECURITY HERE we need
                // to replace sanctum_evbase and evmask with the actual fixed
                // SM region
                VMInfo parvm_info = vm_info;
                VMInfo eparvm_info = vm_info;
                parvm_info.sanctum_evbase = maxBound;
                parvm_info.sanctum_evmask = 0;
                eparvm_info.sanctum_evbase = maxBound;
                eparvm_info.sanctum_evmask = 0;
                if ((vm_info.prv == prvM ? (outOfProtectionDomain(parvm_info,vaddr) && outOfProtectionDomain(eparvm_info,vaddr)) : outOfProtectionDomain(vm_info, vaddr))) begin
                    hitQ.enq(tuple2(?, Valid (InstAccessFault)));
                end
`else
                // No security check
                if (False) begin
                    noAction;
                end
`endif
                else if (vm_info.sv39) begin
                    let vpn = getVpn(vaddr);
                    let trans_result = tlb.translate(vpn, vm_info.asid);
                    if (trans_result.hit) begin
                        // TLB hit
                        let entry = trans_result.entry;
                        // check permission
                        if(hasVMPermission(vm_info,
                                           entry.pteType,
                                           entry.ppn,
                                           entry.level,
                                           InstFetch)) begin
                            // update replacement info
                            tlb.updateRepByHit(trans_result.index);
                            // translate addr
                            Addr trans_addr = translate(
                                vaddr, entry.ppn, entry.level
                            );
                            hitQ.enq(tuple2(trans_addr, Invalid));
                            if(verbose) begin
                                $display("ITLB %m req (hit): ", fshow(vaddr),
                                         " ; ", fshow(trans_result));
                            end
                        end
                        else begin
                            // page fault
                            hitQ.enq(tuple2(?, Valid (InstPageFault)));
                            if(verbose) begin
                                $display("ITLB %m req no permission: ",
                                         fshow(vaddr));
                            end
                        end
                    end
                    else begin
                        // TLB miss, req to parent TLB
                        miss <= Valid (vaddr);
                        rqToPQ.enq(ITlbRqToP {vpn: vpn});
                        if(verbose) begin
                            $display("ITLB %m req (miss): ", fshow(vaddr));
                        end
`ifdef PERF_COUNT
                        latTimer.start(0);
                        if(doStats) begin
                            missCnt.incr(1);
                        end
`endif
                    end
                end
                else begin
                    // bare mode, no translation
                    hitQ.enq(tuple2(vaddr, Invalid));
                    if (verbose) $display("ITLB %m req (bare): ", fshow(vaddr));
                end

`ifdef PERF_COUNT
                if(doStats) begin
                    accessCnt.incr(1);
                end
`endif
            endmethod
        endinterface
        interface Get response = toGet(hitQ);
    endinterface

    interface ITlbToParent toParent;
        interface rqToP = toFifoDeq(rqToPQ);
        interface rsFromP = toFifoEnq(rsFromPQ);
        interface Client flush;
            interface request = toGet(flushRqToPQ);
            interface response = toPut(flushRsFromPQ);
        endinterface
    endinterface

    interface Perf perf;
        method Action setStatus(Bool stats);
`ifdef PERF_COUNT
            doStats <= stats;
`else
            noAction;
`endif
        endmethod

        method Action req(L1TlbPerfType r);
            perfReqQ.enq(r);
        endmethod

        method ActionValue#(PerfResp#(L1TlbPerfType)) resp;
`ifdef PERF_COUNT
            perfRespQ.deq;
            return perfRespQ.first;
`else
            perfReqQ.deq;
            return PerfResp {
                pType: perfReqQ.first,
                data: 0
            };
`endif
        endmethod

        method Bool respValid;
`ifdef PERF_COUNT
            return perfRespQ.notEmpty;
`else
            return perfReqQ.notEmpty;
`endif
        endmethod
    endinterface
endmodule
