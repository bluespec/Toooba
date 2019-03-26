
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

import Types::*;
import ProcTypes::*;
import RegFile::*;
import Ehr::*;
import Vector::*;
import GlobalBrHistReg::*;
import BrPred::*;
import TourPred::*;

export mkTourPredSecure;

// 4KB tournament predictor with flush methods for security
// XXX We DO NOT stall prediction or update methods when flushing to reduce
// logic. It is guaranteed outside that no prediction or update can happen when
// flushing.

typedef 10 PCIndexSz;
typedef Bit#(PCIndexSz) PCIndex;

// We group several sat counters/local hists together in order to flush faster
typedef 9 TabIndexSz;
typedef Bit#(TabIndexSz) TabIndex;

// vector of local hists
typedef TSub#(PCIndexSz, TabIndexSz) LgLocalHistVecSz;
typedef TExp#(LgLocalHistVecSz) LocalHistVecSz;
typedef Bit#(LgLocalHistVecSz) LocalHistVecSelect;

// vector of local bht sat counters
typedef TSub#(TourLocalHistSz, TabIndexSz) LgLocalBhtVecSz;
typedef TExp#(LgLocalBhtVecSz) LocalBhtVecSz;
typedef Bit#(LgLocalBhtVecSz) LocalBhtVecSelect;

// vector of global bht sat counters and global choice sat counters
typedef TSub#(TourGlobalHistSz, TabIndexSz) LgGlobalVecSz;
typedef TExp#(LgGlobalVecSz) GlobalVecSz;
typedef Bit#(GlobalVecSz) GlobalVecSelect;

typedef struct {
    Addr pc;
    Bool taken;
    TourTrainInfo train;
    Bool mispred;
} TourUpdate deriving(Bits, Eq, FShow);

(* synthesize *)
module mkTourPredSecure(DirPredictor#(TourTrainInfo));
    // FIXME: The regfile should be initialized (on FPGA, all 0 after programming)
    // local history: MSB is the latest branch
    RegFile#(TabIndex, Vector#(LocalHistVecSz, TourLocalHist)) localHistTab <- mkRegFileWCF(0, maxBound);
    // local sat counters
    RegFile#(TabIndex, Vector#(LocalBhtVecSz, Bit#(3))) localBht <- mkRegFileWCF(0, maxBound);
    // global history reg
    TourGHistReg gHistReg <- mkTourGHistReg;
    // global sat counters
    RegFile#(TabIndex, Vector#(GlobalVecSz, Bit#(2))) globalBht <- mkRegFileWCF(0, maxBound);
    // choice sat counters: large (taken) -- use local, small (not taken) -- use global
    RegFile#(TabIndex, Vector#(GlobalVecSz, Bit#(2))) choiceBht <- mkRegFileWCF(0, maxBound);

    // EHR to record predict results in this cycle
    Ehr#(TAdd#(1, SupSize), SupCnt) predCnt <- mkEhr(0);
    Ehr#(TAdd#(1, SupSize), Bit#(SupSize)) predRes <- mkEhr(0);

    RWire#(TourUpdate) updateEn <- mkRWire;

    // security flush
    Reg#(Bool) flushDone <- mkReg(True);
    Reg#(TabIndex) flushIndex <- mkReg(0);

    function Tuple2#(TabIndex, LocalHistVecSelect) getPCIndex(Addr pc);
        PCIndex pcIdx = truncate(pc >> 2);
        TabIndex tabIdx = truncateLSB(pcIdx);
        LocalHistVecSelect sel = truncate(pcIdx);
        return tuple2(tabIdx, sel);
    endfunction

    function Tuple2#(TabIndex, LocalBhtVecSelect) getLocalBhtIndex(TourLocalHist hist);
        TabIndex idx = truncateLSB(hist);
        LocalHistVecSelect sel = truncate(hist);
        return tuple2(idx, sel);
    endfunction

    function Tuple2#(TabIndex, GlobalVecSelect) getGlobalIndex(TourGlobalHist hist);
        TabIndex idx = truncateLSB(hist);
        GlobalVecSelect sel = truncate(hist);
        return tuple2(idx, sel);
    endfunction

    // common sat counter operations
    function Bool isTaken(Bit#(n) cnt) provisos(Add#(1, a__, n));
        Bit#(1) msb = truncateLSB(cnt);
        return msb == 1;
    endfunction

    function Bit#(n) updateCnt(Bit#(n) cnt, Bool taken);
        if(taken) begin
            return cnt == maxBound ? maxBound : cnt + 1;
        end
        else begin
            return cnt == 0 ? 0 : cnt - 1;
        end
    endfunction

    TourGlobalHist curGHist = gHistReg.history; // global history: MSB is the latest branch

    Vector#(SupSize, DirPred#(TourTrainInfo)) predIfc;
    for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
        predIfc[i] = (interface DirPred;
            method ActionValue#(DirPredResult#(TourTrainInfo)) pred(Addr pc);
                // get local history
                let {localHistTabIdx, localHistVecSel} = getPCIndex(pc);
                Vector#(LocalHistVecSz, TourLocalHist) localHistVec = localHistTab.sub(localHistTabIdx);
                TourLocalHist localHist = localHistVec[localHistVecSel];
                // get local prediction
                let {localBhtTabIdx, localBhtVecSel} = getLocalBhtIndex(localHist);
                Vector#(LocalBhtVecSz, Bit#(3)) localBhtVec = localBht.sub(localBhtTabIdx);
                Bool localTaken = isTaken(localBhtVec[localBhtVecSel]);

                // get the global history
                // all previous branch in this cycle must be not taken
                // otherwise this branch should be on wrong path
                // because all inst in same cycle are fetched consecutively
                TourGlobalHist globalHist = curGHist >> predCnt[i];
                // get global prediction
                let {globalTabIdx, globalVecSel} = getGlobalIndex(globalHist);
                Vector#(GlobalVecSz, Bit#(2)) globalBhtVec = globalBht.sub(globalTabIdx);
                Bool globalTaken = isTaken(globalBhtVec[globalVecSel]);

                // make choice
                Vector#(GlobalVecSz, Bit#(2)) choiceVec = choiceBht.sub(globalTabIdx);
                Bool useLocal = isTaken(choiceVec[globalVecSel]);
                Bool taken = useLocal ? localTaken : globalTaken;

                // record prediction
                predCnt[i] <= predCnt[i] + 1;
                Bit#(SupSize) res = predRes[i];
                res[predCnt[i]] = pack(taken);
                predRes[i] <= res;

                // return
                return DirPredResult {
                    taken: taken,
                    train: TourTrainInfo {
                        globalHist: globalHist,
                        localHist: localHist,
                        globalTaken: globalTaken,
                        localTaken: localTaken
                    }
                };
            endmethod
        endinterface);
    end

    (* fire_when_enabled, no_implicit_conditions *)
    rule canonGlobalHist;
        gHistReg.addHistory(predRes[valueof(SupSize)], predCnt[valueof(SupSize)]);
        predRes[valueof(SupSize)] <= 0;
        predCnt[valueof(SupSize)] <= 0;
    endrule

    // no flush, accept update
    (* fire_when_enabled, no_implicit_conditions *)
    rule canonUpdate(flushDone &&& updateEn.wget matches tagged Valid .upd);
        let pc = upd.pc;
        let taken = upd.taken;
        let train = upd.train;
        let mispred = upd.mispred;

        // update history if mispred
        if(mispred) begin
            TourGlobalHist newHist = truncateLSB({pack(taken), train.globalHist});
            gHistReg.redirect(newHist);
        end

        // update local history (assume only 1 branch for an PC in flight)
        let {localHistTabIdx, localHistVecSel} = getPCIndex(pc);
        Vector#(LocalHistVecSz, TourLocalHist) localHistVec = localHistTab.sub(localHistTabIdx);
        localHistVec[localHistVecSel] = truncateLSB({pack(taken), train.localHist});
        localHistTab.upd(localHistTabIdx, localHistVec);

        // update local sat cnt
        let {localBhtTabIdx, localBhtVecSel} = getLocalBhtIndex(train.localHist);
        Vector#(LocalBhtVecSz, Bit#(3)) localBhtVec = localBht.sub(localBhtTabIdx);
        Bit#(3) localCnt = localBhtVec[localBhtVecSel];
        localBhtVec[localBhtVecSel] = updateCnt(localCnt, taken);
        localBht.upd(localBhtTabIdx, localBhtVec);

        // update global sat cnt
        let {globalTabIdx, globalVecSel} = getGlobalIndex(train.globalHist);
        Vector#(GlobalVecSz, Bit#(2)) globalBhtVec = globalBht.sub(globalTabIdx);
        Bit#(2) globalCnt = globalBhtVec[globalVecSel];
        globalBhtVec[globalVecSel] = updateCnt(globalCnt, taken);
        globalBht.upd(globalTabIdx, globalBhtVec);

        // update choice cnt
        if(train.globalTaken != train.localTaken) begin
            Vector#(GlobalVecSz, Bit#(2)) choiceVec = choiceBht.sub(globalTabIdx);
            Bit#(2) choiceCnt = choiceVec[globalVecSel];
            Bool useLocal = train.localTaken == taken;
            choiceVec[globalVecSel] = updateCnt(choiceCnt, useLocal);
            choiceBht.upd(globalTabIdx, choiceVec);
        end
    endrule

    // flushing, drop update and flush table entries one by one
    rule canonFlush(!flushDone);
        localHistTab.upd(flushIndex, replicate(0));
        localBht.upd(flushIndex, replicate(0));
        globalBht.upd(flushIndex, replicate(0));
        choiceBht.upd(flushIndex, replicate(0));
        gHistReg.redirect(0);
        flushIndex <= flushIndex + 1;
        if (flushIndex == maxBound) begin
            flushDone <= True;
        end
    endrule

    interface pred = predIfc;

    method Action update(Addr pc, Bool taken, TourTrainInfo train, Bool mispred);
        updateEn.wset(TourUpdate {pc: pc, taken: taken, train: train, mispred: mispred});
    endmethod

    method Action flush if(flushDone);
        flushDone <= False;
    endmethod
    method flush_done = flushDone._read;
endmodule
