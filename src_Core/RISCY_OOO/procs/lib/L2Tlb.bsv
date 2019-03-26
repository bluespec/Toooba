
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
import DefaultValue::*;
import ClientServer::*;
import GetPut::*;
import Types::*;
import ProcTypes::*;
import TlbTypes::*;
import Performance::*;
import FullAssocTlb::*;
import ConfigReg::*;
import Ehr::*;
import Fifo::*;
import Cntrs::*;
import SafeCounter::*;
import CacheUtils::*;
import SetAssocTlb::*;
import L2SetAssocTlb::*;
import TranslationCache::*;
import LatencyTimer::*;

// for SV39 only

// curretly L2 TLB is just a blocking page walker

// interface with memory (LLC)
typedef L2TlbReqIdx TlbMemReqId;

typedef struct {
    // this is always a load req
    Addr addr;
    TlbMemReqId id;
} TlbMemReq deriving(Bits, Eq, FShow);

typedef struct {
    Data data;
    TlbMemReqId id;
} TlbLdResp deriving(Bits, Eq, FShow);

interface TlbMemClient;
    interface FifoDeq#(TlbMemReq) memReq;
    interface FifoEnq#(TlbLdResp) respLd;
endinterface

// interface with children (I/D TLB)
typedef union tagged {
    void I;
    DTlbReqIdx D;
} TlbChild deriving(Bits, Eq, FShow);
typedef struct {
    TlbChild child;
    Vpn vpn;
} L2TlbRqFromC deriving(Bits, Eq, FShow);

typedef struct {
    TlbChild child;
    Maybe#(TlbEntry) entry;
} L2TlbRsToC deriving(Bits, Eq, FShow);

interface L2TlbToChildren;
    interface Put#(L2TlbRqFromC) rqFromC;
    interface FifoDeq#(L2TlbRsToC) rsToC;
    // flush with I/D TLB
    interface Put#(void) iTlbReqFlush;
    interface Put#(void) dTlbReqFlush;
    interface Get#(void) flushDone;
endinterface

interface L2Tlb;
    // keep update with changes to CSRs
    method Action updateVMInfo(VMInfo vmI, VMInfo vmD);

    // ifc with ITLb & DTLB
    interface L2TlbToChildren toChildren;

    // ifc with memory (LLC)
    interface TlbMemClient toMem;

    // performace
    interface Perf#(L2TlbPerfType) perf;
endinterface

typedef FullAssocTlb#(`L2_TLB_HUGE_SIZE) L2FullAssocTlb;
module mkL2FullAssocTlb(L2FullAssocTlb);
    let m <- mkFullAssocTlb(True); // randomness in replacement
    return m;
endmodule

// a pending tlb req may be in following states
typedef union tagged {
    void None;
    void WaitMem; // wait for page walk resp from memory
    L2TlbReqIdx WaitPeer; // wait for page walk resp from peer req
} L2TlbWait deriving(Bits, Eq, FShow);

// TODO we should raise load access fault if the PTE address is not a DRAM
// address. (trap value is still the virtual address being translated).

(* synthesize *)
module mkL2Tlb(L2Tlb::L2Tlb);
    Bool verbose = True;
   
    // set associative TLB for 4KB pages
    L2SetAssocTlb tlb4KB <- mkL2SetAssocTlb;
    // fully associative TLB for mega and giga pages
    L2FullAssocTlb tlbMG <- mkL2FullAssocTlb;
    // FIFO in parallel with TLB
    Fifo#(1, L2TlbReqIdx) tlbReqQ <- mkPipelineFifo;

    // MMU translation cache
    TranslationCache transCache <- mkSplitTransCache;
    // FIFO in parallel with MMU cache
    Fifo#(1, L2TlbReqIdx) transCacheReqQ <- mkPipelineFifo;

    // flush
    Reg#(Bool) iFlushReq <- mkReg(False);
    Reg#(Bool) dFlushReq <- mkReg(False);
    Reg#(Bool) waitFlushDone <- mkReg(False);
    Bool flushing = iFlushReq && dFlushReq;
    Fifo#(1, void) flushDoneQ <- mkCFFifo;

    // req/resp with I/D TLBs
    Fifo#(1, L2TlbRqFromC) rqFromCQ <- mkBypassFifo;
    Fifo#(1, L2TlbRsToC) rsToCQ <- mkBypassFifo;

    // pending reqs
    // pendWait should be meaningful even when entry is invalid. pendWait =
    // WaitMem means this entry is waiting for page walk resp from memory;
    // pendWait = WaitPeer means this entry is waiting for a page walk resp
    // resp initiated by another req. Thus, pendWait must be None when entry is
    // invalid.
    Vector#(L2TlbReqNum, Ehr#(2, Bool)) pendValid <- replicateM(mkEhr(False));
    Vector#(L2TlbReqNum, Reg#(L2TlbRqFromC)) pendReq <- replicateM(mkRegU);
    Vector#(L2TlbReqNum, Ehr#(2, L2TlbWait)) pendWait <- replicateM(mkEhr(None));
    Vector#(L2TlbReqNum, Reg#(PageWalkLevel)) pendWalkLevel <- replicateM(mkRegU);
    Vector#(L2TlbReqNum, Reg#(Addr)) pendWalkAddr <- replicateM(mkRegU);
`ifdef SECURITY
    Vector#(L2TlbReqNum, Reg#(Bool)) pendEnclave <- replicateM(mkRegU);
`endif

    // rule ordering:
    // - trans cache resp < tlb resp < tlb req
    // - trans cache resp mutually exclusive with page walk (trans cache resp
    // has precedence). I don't want them to fire together because both rules
    // update walk level, and I don't want a bypass path.
    // - tlb resp mutually exclusive with page walk (tlb resp has precedence).
    // These two rules cannot fire together, because page walk may update trans
    // cache and tlb resp may request trans cache. tlb resp takes precednece
    // because page walk may update tlb and it needs to wait for tlb resp to
    // deq resp.
    // - tlb req mutually exclusive with page walk (page walk has precedence).
    // They cannot fire together, because page walk may udpate tlb and tlb req
    // may request tlb.

    let pendValid_transCacheResp = getVEhrPort(pendValid, 0); // assert
    let pendValid_tlbResp = getVEhrPort(pendValid, 0);
    let pendValid_pageWalk = getVEhrPort(pendValid, 0);
    let pendValid_tlbReq = getVEhrPort(pendValid, 1);

    let pendWait_transCacheResp = getVEhrPort(pendWait, 0);
    let pendWait_pageWalk = getVEhrPort(pendWait, 0);
    let pendWait_tlbResp = getVEhrPort(pendWait, 1); // perf
    let pendWait_tlbReq = getVEhrPort(pendWait, 1); // assert

    // current processor VM information
    Reg#(VMInfo) vm_info_I <- mkReg(defaultValue);
    Reg#(VMInfo) vm_info_D <- mkReg(defaultValue);

    // Memory Queues for page table walks
    Fifo#(2, TlbMemReq) memReqQ <- mkCFFifo;
    Fifo#(2, TlbLdResp) respLdQ <- mkCFFifo;
    // When a mem resp comes, we first process the initiating req, then process
    // other reqs that in WaitPeer.
    Reg#(Maybe#(L2TlbReqIdx)) respForOtherReq <- mkReg(Invalid);

    // FIFO for perf req
    Fifo#(1, L2TlbPerfType) perfReqQ <- mkCFFifo;
`ifdef PERF_COUNT
    Fifo#(1, PerfResp#(L2TlbPerfType)) perfRespQ <- mkCFFifo;
    Reg#(Bool) doStats <- mkConfigReg(False);
    Count#(Data) instMissCnt <- mkCount(0);
    Count#(Data) instMissLat <- mkCount(0);
    Count#(Data) instPageWalks <- mkCount(0);
    Count#(Data) instSavedPageWalks <- mkCount(0);
    Count#(Data) instHugePageHitCnt <- mkCount(0);
    Count#(Data) instHugePageMissCnt <- mkCount(0);
    Count#(Data) dataMissCnt <- mkCount(0);
    Count#(Data) dataMissLat <- mkCount(0);
    Count#(Data) dataPageWalks <- mkCount(0);
    Count#(Data) dataSavedPageWalks <- mkCount(0);
    Count#(Data) dataHugePageHitCnt <- mkCount(0);
    Count#(Data) dataHugePageMissCnt <- mkCount(0);
    Count#(Data) hitUnderMissCnt <- mkCount(0);
    Count#(Data) allMissCycles <- mkCount(0);
    Count#(Data) peerSavedMemReqCnt <- mkCount(0);

    LatencyTimer#(L2TlbReqNum, 12) latTimer <- mkLatencyTimer; // max latency: 4K cycles

    function Action incrMissLat(TlbChild child, L2TlbReqIdx idx);
    action
        let lat <- latTimer.done(idx);
        if(doStats) begin
            if(child == I) begin
                instMissLat.incr(zeroExtend(lat));
            end
            else begin
                dataMissLat.incr(zeroExtend(lat));
            end
        end
    endaction
    endfunction

    rule incrAllMissCycles(doStats);
        function Bool isMiss(L2TlbWait x) = x != None;
        when(all(isMiss, readVEhr(0, pendWait)), allMissCycles.incr(1));
    endrule

    rule doPerf;
        let t <- toGet(perfReqQ).get;
        Data d = (case(t)
            L2TlbInstMissCnt: (instMissCnt);
            L2TlbInstMissLat: (instMissLat);
            L2TlbInstPageWalks: (instPageWalks);
            L2TlbInstSavedPageWalks: (instSavedPageWalks);
            L2TlbInstHugePageHits: (instHugePageHitCnt);
            L2TlbInstHugePageMisses: (instHugePageMissCnt);
            L2TlbDataMissCnt: (dataMissCnt);
            L2TlbDataMissLat: (dataMissLat);
            L2TlbDataPageWalks: (dataPageWalks);
            L2TlbDataSavedPageWalks: (dataSavedPageWalks);
            L2TlbDataHugePageHits: (dataHugePageHitCnt);
            L2TlbDataHugePageMisses: (dataHugePageMissCnt);
            L2TlbHitUnderMissCnt: (hitUnderMissCnt);
            L2TlbAllMissCycles: (allMissCycles);
            L2TlbPeerSavedMemReqs: (peerSavedMemReqCnt);
            default: (0);
        endcase);
        perfRespQ.enq(PerfResp {
            pType: t,
            data: d
        });
    endrule
`endif

    // when flushing is true, since both I and D TLBs have finished flush and
    // is waiting for L2 to flush, all I/D TLB req must have been responded.
    // Thus, there cannot be any req in pendReq or rqFromCQ.
    rule doStartFlush(flushing && !waitFlushDone);
        waitFlushDone <= True;
        tlb4KB.flush;
        tlbMG.flush;
        transCache.flush;
        // check no req
        doAssert(!rqFromCQ.notEmpty, "cannot have new req");
        doAssert(readVEhr(0, pendValid) == replicate(False), "cannot have pending req");
    endrule

    rule doWaitFlush(flushing && waitFlushDone && tlb4KB.flush_done && transCache.flush_done);
        waitFlushDone <= False;
        flushDoneQ.enq(?);
        iFlushReq <= False;
        dFlushReq <= False;
    endrule

    // tlb req rule is preempted by page walk rule, i.e., don't fire when page
    // walk resp is avaiable
    rule doTlbReq(!flushing && !respLdQ.notEmpty);
        // find a slot for the new req
        L2TlbReqIdx idx = ?;
        if(findIndex( \== (False) , readVReg(pendValid_tlbReq) ) matches tagged Valid .i) begin
            idx = pack(i);
        end
        else begin
            when(False, noAction);
        end
        // get new req
        rqFromCQ.deq;
        let r = rqFromCQ.first;
        // req tlb array
        VMInfo vm_info = r.child == I ? vm_info_I : vm_info_D;
        tlb4KB.req(Translate (SetAssocTlbTranslateReq {
            vpn: r.vpn,
            asid: vm_info.asid
        }));
        tlbReqQ.enq(idx);
        // record req
        pendValid_tlbReq[idx] <= True;
        pendReq[idx] <= r;
        doAssert(!pendValid_tlbReq[idx], "entry must be invalid");
        doAssert(pendWait_tlbReq[idx] == None, "cannot be waiting");
        if(verbose) $display("L2TLB new req: ", fshow(r), "; ", fshow(idx));
    endrule

    // process resp from 4KB TLB and mega-giga TLB
    rule doTlbResp(tlbReqQ.notEmpty);
        doAssert(!flushing, "cannot have pending req when flushing");

        // get req in tlb
        tlbReqQ.deq;
        L2TlbReqIdx idx = tlbReqQ.first;
        L2TlbRqFromC cRq = pendReq[idx];
        doAssert(pendWait_tlbResp[idx] == None, "cannot be waiting");

        // get correct VM info
        VMInfo vm_info = cRq.child == I ? vm_info_I : vm_info_D;
        doAssert(vm_info.sv39, "must be in sv39 mode");

        // get resp from 4KB TLB and mega-giga TLB
        let resp4KB = tlb4KB.resp;
        let respMG = tlbMG.translate(cRq.vpn, vm_info.asid);

        if(verbose) begin
            $display("L2TLB resp: ", fshow(vm_info), " ; ", fshow(cRq), " ; ", 
                     fshow(resp4KB), " ; ", fshow(respMG));
        end

        // when page hit, resp to child (4KB array is not dequeued)
        function Action pageHit(TlbEntry entry);
        action
            // resp to child
            rsToCQ.enq(L2TlbRsToC {
                child: cRq.child,
                entry: Valid (entry)
            });
            // req is done
            pendValid_tlbResp[idx] <= False;
`ifdef PERF_COUNT
            // perf: hit under miss
            function Bool otherMiss(L2TlbReqIdx i);
                return pendValid_tlbResp[i] && pendWait_tlbResp[i] != None && i != idx;
            endfunction
            Vector#(L2TlbReqNum, L2TlbReqIdx) idxVec = genWith(fromInteger);
            if(any(otherMiss, idxVec)) begin
                hitUnderMissCnt.incr(1);
            end
`endif
        endaction
        endfunction

        if(!vm_info.sv39) begin
            // not in sv39 -> page fault
            // resp with invalid entry
            rsToCQ.enq(L2TlbRsToC {
                child: cRq.child,
                entry: Invalid
            });
            // deq 4KB TLB array
            tlb4KB.deqResp(Invalid);
            // req is done
            pendValid_tlbResp[idx] <= False;
        end
        else if(respMG.hit) begin
            // hit on a mega or giga page
            let entry = respMG.entry;
            doAssert(entry.level > 0 && entry.level <= maxPageWalkLevel,
                     "mega or giga page");
            pageHit(entry);
            tlb4KB.deqResp(Invalid); // just deq 4KB array
            tlbMG.updateRepByHit(respMG.index); // update replacement in MG array
`ifdef PERF_COUNT
            if(doStats) begin
                if(cRq.child == I) begin
                    instHugePageHitCnt.incr(1);
                end
                else begin
                    dataHugePageHitCnt.incr(1);
                end
            end
`endif
        end
        else if(resp4KB.hit) begin
            // hit on 4KB page
            let entry = resp4KB.entry;
            doAssert(entry.level == 0, "must be 4KB page");
            pageHit(entry);
            // update 4KB array replacement, no need to touch MG array
            tlb4KB.deqResp(Valid (resp4KB.way));
        end
        else begin
            // miss, deq resp
            tlb4KB.deqResp(Invalid);
            // check translation cache
            transCache.req(cRq.vpn, vm_info.asid);
            transCacheReqQ.enq(idx);
            // perf: TLB miss
`ifdef PERF_COUNT
            latTimer.start(idx);
            if(doStats) begin
                if(cRq.child == I) begin
                    instMissCnt.incr(1);
                end
                else begin
                    dataMissCnt.incr(1);
                end
            end
`endif
        end
    endrule

    // Find other req doing the same page walk. Although it should be ok to
    // just compare the VPNs, for safety, we compare the PTE addrs.
    function Maybe#(L2TlbReqIdx) otherReqSamePTE(
        L2TlbReqIdx idx, Addr pteAddr,
        Vector#(L2TlbReqNum, L2TlbWait) waitVec
    );
        function Bool samePTE(L2TlbReqIdx i);
            return i != idx && waitVec[i] == WaitMem && pendWalkAddr[i] == pteAddr;
        endfunction
        Vector#(L2TlbReqNum, L2TlbReqIdx) idxVec = genWith(fromInteger);
        return find(samePTE, idxVec);
    endfunction

    rule doTranslationCacheResp(transCacheReqQ.notEmpty);
        // get req in trans cache
        transCacheReqQ.deq;
        L2TlbReqIdx idx = transCacheReqQ.first;
        L2TlbRqFromC cRq = pendReq[idx];
        // get trans cache resp
        transCache.deqResp;
        let resp = transCache.resp;
        // start page walk based on the translation cache resp. Note that if
        // the startLevel in resp is max, then we should use the base ppn in vm
        // info.
        // XXX We believe the PPN stored in translation cache has been
        // sanitized before, so we don't need to check again if the PPN is an
        // enclave PPN or not. (This should follow the same reasoning that TLB
        // hits do not need checks.)
        VMInfo vm_info = cRq.child == I ? vm_info_I : vm_info_D;
        PageWalkLevel level = resp.startLevel;
        Ppn rootPPN = vm_info.basePPN; // root of page table
`ifdef SECURITY
        Bool isEnclave = (zeroExtend(cRq.vpn) & vm_info.sanctum_evmask) == vm_info.sanctum_evbase;
        if(isEnclave) begin
            rootPPN = vm_info.sanctum_ebasePPN; // enclave has its own root of page table
        end
`endif
        Addr baseAddr = getPTBaseAddr(level < maxPageWalkLevel ? resp.ppn : rootPPN);
        Addr pteAddr = getPTEAddr(baseAddr, cRq.vpn, level);
        // record page walk info
        pendWalkLevel[idx] <= level;
        pendWalkAddr[idx] <= pteAddr;
`ifdef SECURITY
        pendEnclave[idx] <= isEnclave;
`endif
        doAssert(pendWait_transCacheResp[idx] == None, "cannot be waiting");
        // don't req memory if someone else is also doing the same page walk.
        if(otherReqSamePTE(idx, pteAddr, readVReg(pendWait_transCacheResp)) matches tagged Valid .i) begin
            // peer entry has already requested, so don't send
            // duplicate req
            pendWait_transCacheResp[idx] <= WaitPeer (i);
            doAssert(pendValid_transCacheResp[i], "peer must be valid");
`ifdef PERF_COUNT
            if(doStats) begin
                peerSavedMemReqCnt.incr(1);
            end
`endif
        end
        else begin
            // no one has requested before, req memory
            memReqQ.enq(TlbMemReq {
                addr: pteAddr,
                id: idx
            });
            pendWait_transCacheResp[idx] <= WaitMem;
        end
        if(verbose) begin
            $display("L2TLB start page walk: ", fshow(cRq), "; ",
                     fshow(vm_info), "; ", fshow(resp), "; ",
                     fshow(level), "; ", fshow(pteAddr));
        end
`ifdef PERF_COUNT
        // perf: saved page walks
        if(doStats) begin
            Data saved = zeroExtend(maxPageWalkLevel - level);
            if(cRq.child == I) begin
                instSavedPageWalks.incr(saved);
            end
            else begin
                dataSavedPageWalks.incr(saved);
            end
        end
`endif
    endrule

    // page walk is preempted by tlb resp rule and trans cache resp rule, i.e.,
    // don't fire when tlb resp or trans cache resp are available
    rule doPageWalk(respLdQ.notEmpty && !tlbReqQ.notEmpty && !transCacheReqQ.notEmpty);
        doAssert(!flushing, "cannot have pending req when flushing");

        // get the resp data from memory (LLC); this resp is for the initiating
        // req and other req that wait on this one, so don't deq right away
        PTESv39 pte = unpack(respLdQ.first.data);
        L2TlbReqIdx idx = fromMaybe(respLdQ.first.id, respForOtherReq);
        L2TlbRqFromC cRq = pendReq[idx];

        // find another req waiting for this resp to process in next cycle
        function Bool waitForResp(L2TlbReqIdx i);
            return pendWait_pageWalk[i] == WaitPeer (respLdQ.first.id) && i != idx;
        endfunction
        Vector#(L2TlbReqNum, L2TlbReqIdx) idxVec = genWith(fromInteger);
        if(find(waitForResp, idxVec) matches tagged Valid .i) begin
            // still have req waiting for this resp, keep processing
            respForOtherReq <= Valid (i);
            doAssert(pendValid_pageWalk[i], "waiting entry must be valid");
        end
        else begin
            // all req done, deq the mem resp
            respForOtherReq <= Invalid;
            respLdQ.deq;
        end

        // handle page fault
        function Action pageFault(String reason);
        action
            // resp with invalid entry
            rsToCQ.enq(L2TlbRsToC {
                child: cRq.child,
                entry: Invalid
            });
            // req is done
            pendValid_pageWalk[idx] <= False;
            pendWait_pageWalk[idx] <= None;
`ifdef PERF_COUNT
            // incr miss latency
            incrMissLat(cRq.child, idx);
`endif
        endaction
        endfunction

        // get correct VM info
        VMInfo vm_info = cRq.child == I ? vm_info_I : vm_info_D;

        // assume we are continuing page walk, update level and pte addr (it
        // causes not harm even if we stop page walk)
        PageWalkLevel walkLevel = pendWalkLevel[idx];
        PageWalkLevel newWalkLevel = walkLevel - 1;
        Addr newPTBase = getPTBaseAddr(pte.ppn);
        Addr newPTEAddr = getPTEAddr(newPTBase, cRq.vpn, newWalkLevel);
        pendWalkLevel[idx] <= newWalkLevel;
        pendWalkAddr[idx] <= newPTEAddr;

        // reach leaf PTE
        Bool leafPTE = isLeafPTE(pte.pteType);

`ifdef SECURITY
        // base/bound/mask to enforce that page walk does not cross enclave
        // boundary
        Addr parbase = pendEnclave[idx] ? vm_info.sanctum_eparbase : vm_info.sanctum_parbase;
        Addr parmask = pendEnclave[idx] ? vm_info.sanctum_eparmask : vm_info.sanctum_parmask;
        Addr mrbm    = pendEnclave[idx] ? vm_info.sanctum_emrbm    : vm_info.sanctum_mrbm;
`endif

        if(verbose) begin
            $display("L2TLB page walk: ", fshow(vm_info), " ; ",
                     fshow(idx), " ; ", fshow(cRq), " ; ",
                     fshow(walkLevel), " ; ", fshow(pte));
        end

        if(!vm_info.sv39) begin
            // no longer in sv39 mode -> page fault
            pageFault("Not in sv39");
        end
        else if(!pte.valid) begin
            // invalid pte -> fault
            pageFault("invalid page");
        end
`ifdef SECURITY
        // TODO: raise page fault or access fault?
        else if((newPTBase & parmask) == parbase) begin
            // access falls into protected address range (that I don't have permission) -> fault
            pageFault("SANCTUM protected address range");
        end
        else if((getAddrRegions(newPTBase, leafPTE, walkLevel) & mrbm) == 0) begin
            // access falls outiside regions belong to me -> fault
            pageFault("SANCTUM protected address range");
        end
`endif
        else begin
            // page is valid, check leaf or not
            if(!leafPTE) begin
                // non-leaf page
                if(walkLevel == 0) begin
                    // page walk end with non-leaf page -> fault
                    pageFault("non-leaf page at end");
                end
                else begin
                    // continue page walk, check if other req is doing the same
                    // walk 
                    if(otherReqSamePTE(idx, newPTEAddr, readVReg(pendWait_pageWalk)) matches tagged Valid .i) begin
                        pendWait_pageWalk[idx] <= WaitPeer (i);
`ifdef PERF_COUNT
                        if(doStats) begin
                            peerSavedMemReqCnt.incr(1);
                        end
`endif
                    end
                    else begin
                        memReqQ.enq(TlbMemReq {
                            addr: newPTEAddr,
                            id: idx
                        });
                        pendWait_pageWalk[idx] <= WaitMem;
                    end
                    // add to translation cache
                    transCache.addEntry(cRq.vpn, walkLevel, pte.ppn, vm_info.asid);
                end
            end
            else begin
                // leaf page, get new entry
                Vpn masked_vpn = getMaskedVpn(cRq.vpn, walkLevel);
                Ppn masked_ppn = getMaskedPpn(pte.ppn, walkLevel);
                let entry = TlbEntry {
                    vpn:     masked_vpn,
                    ppn:     masked_ppn,
                    pteType: pte.pteType,
                    level:   walkLevel,
                    asid:    vm_info.asid
                };
                // resp child
                rsToCQ.enq(L2TlbRsToC {
                    child: cRq.child,
                    entry: Valid (entry)
                });
                // update TLB array
                if(entry.level > 0) begin
                    // add to mega/giga page tlb
                    tlbMG.addEntry(entry);
`ifdef PERF_COUNT
                    if(doStats) begin
                        if(cRq.child == I) begin
                            instHugePageMissCnt.incr(1);
                        end
                        else begin
                            dataHugePageMissCnt.incr(1);
                        end
                    end
`endif
                end
                else begin
                    // 4KB page, add to 4KB TLB
                    tlb4KB.req(Refill (entry));
                end
                // req is done
                pendValid_pageWalk[idx] <= False;
                pendWait_pageWalk[idx] <= None;
`ifdef PERF_COUNT
                // incr miss latency
                incrMissLat(cRq.child, idx);
`endif
            end
        end
`ifdef PERF_COUNT
        // perf: page walk done once
        if(doStats) begin
            if(cRq.child == I) begin
                instPageWalks.incr(1);
            end
            else begin
                dataPageWalks.incr(1);
            end
        end
`endif
    endrule

    method Action updateVMInfo(VMInfo vmI, VMInfo vmD); //if(!isValid(pendReq));
        vm_info_I <= vmI;
        vm_info_D <= vmD;
    endmethod

    interface L2TlbToChildren toChildren;
        interface Put rqFromC = toPut(rqFromCQ);
        interface rsToC = toFifoDeq(rsToCQ);

        interface Put iTlbReqFlush;
            method Action put(void x) if(!iFlushReq);
                iFlushReq <= True;
            endmethod
        endinterface
        interface Put dTlbReqFlush;
            method Action put(void x) if(!dFlushReq);
                dFlushReq <= True;
            endmethod
        endinterface
        interface Get flushDone = toGet(flushDoneQ);
    endinterface

    interface TlbMemClient toMem;
        interface FifoDeq memReq = toFifoDeq(memReqQ);
        interface FifoEnq respLd = toFifoEnq(respLdQ);
    endinterface
  
    interface Perf perf;
        method Action setStatus(Bool stats);
`ifdef PERF_COUNT
            doStats <= stats;
`else
            noAction;
`endif
        endmethod

        method Action req(L2TlbPerfType r);
            perfReqQ.enq(r);
        endmethod

        method ActionValue#(PerfResp#(L2TlbPerfType)) resp;
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
