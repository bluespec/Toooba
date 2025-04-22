
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
import Cur_Cycle :: *;

export TourLocalHistSz;
export TourLocalHist;
export TourGlobalHistSz;
export TourGlobalHist;
export TourTrainInfo(..);
export TourGHistReg(..);
export mkTourGHistReg;
export mkTourPred;
export PCIndexSz;
export PCIndex;

// 4KB tournament predictor

typedef 17 TourGlobalHistSz;
typedef 17 TourLocalHistSz;
typedef 17 PCIndexSz;

typedef Bit#(TourGlobalHistSz) TourGlobalHist;
typedef Bit#(TourLocalHistSz) TourLocalHist;
typedef Bit#(PCIndexSz) PCIndex;

typedef struct {
    TourGlobalHist globalHist;
    TourLocalHist localHist;
    Bool globalTaken;
    Bool localTaken;
    PCIndex pcIndex;
} TourTrainInfo deriving(Bits, Eq, FShow);

// global history reg
typedef GlobalBrHistReg#(TourGlobalHistSz) TourGHistReg;

(* synthesize *)
module mkTourGHistReg(TourGHistReg);
    let m <- mkGlobalBrHistReg;
    return m;
endmodule

(* synthesize *)
module mkTourPred(DirPredictor#(TourTrainInfo));
    // local history: MSB is the latest branch
    RegFile#(PCIndex, TourLocalHist) localHistTab <- mkRegFileWCF(0, maxBound);
    // local sat counters
    RegFile#(TourLocalHist, Bit#(3)) localBht <- mkRegFileWCF(0, maxBound);
    // global history reg
    TourGHistReg gHistReg <- mkTourGHistReg;
    // global sat counters
    RegFile#(TourGlobalHist, Bit#(2)) globalBht <- mkRegFileWCF(0, maxBound);
    // choice sat counters: large (taken) -- use local, small (not taken) -- use global
    RegFile#(TourGlobalHist, Bit#(2)) choiceBht <- mkRegFileWCF(0, maxBound);

    Reg#(UInt#(64)) predCount <- mkReg(0);
    Reg#(UInt#(64)) misPredCount <- mkReg(0);

    // Lookup PC
    Reg#(Addr) pc_reg <- mkRegU;

    // EHR to record predict results in this cycle
    Ehr#(TAdd#(1, SupSize), SupCnt) predCnt <- mkEhr(0);
    Ehr#(TAdd#(1, SupSize), Bit#(SupSize)) predRes <- mkEhr(0);

    function PCIndex getPCIndex(Addr pc);
        return truncate(pc >> 2);
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
    Reg#(Vector#(SupSize, Bool)) globalTakenVec <- mkRegU;
    Reg#(Vector#(SupSize, Bool)) useLocalVec <- mkRegU;

    Vector#(SupSize, DirPred#(TourTrainInfo)) predIfc;
    for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
        predIfc[i] = (interface DirPred;
            method ActionValue#(DirPredResult#(TourTrainInfo)) pred;
                PCIndex pcIndex = getPCIndex(offsetPc(pc_reg, i));
                // get local history & prediction
                TourLocalHist localHist = localHistTab.sub(pcIndex);
                Bool localTaken = isTaken(localBht.sub(localHist));

                // get the global history
                // all previous branch in this cycle must be not taken
                // otherwise this branch should be on wrong path
                // because all inst in same cycle are fetched consecutively
                // get global prediction
                Bool globalTaken = globalTakenVec[predCnt[i]];

                // make choice
                Bool useLocal = useLocalVec[predCnt[i]];
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
                        globalHist: curGHist >> predCnt[i],
                        localHist: localHist,
                        globalTaken: globalTaken,
                        localTaken: localTaken,
                        pcIndex: pcIndex
                    }
                };
            endmethod
        endinterface);
    end

    (* fire_when_enabled, no_implicit_conditions *)
    rule canonGlobalHist;
        gHistReg.addHistory(predRes[valueof(SupSize)], predCnt[valueof(SupSize)]);
        // Buffer useLocalVec
        // Reproduce next history; this would ideally be done in GlobalBrHistReg to avoid duplicating logic.
        TourGlobalHist nHist = truncate({predRes[valueof(SupSize)], curGHist} >> predCnt[valueof(SupSize)]);
        function Bool globalTakenLookup (Integer i) = isTaken(globalBht.sub(nHist >> i));
        function Bool useLocalLookup (Integer i) = isTaken(choiceBht.sub(nHist >> i));
        globalTakenVec <= genWith(globalTakenLookup);
        useLocalVec <= genWith(useLocalLookup);
        // Reset counters and prediction.
        predRes[valueof(SupSize)] <= 0;
        predCnt[valueof(SupSize)] <= 0;
    endrule

    method nextPc = pc_reg._write;

    interface pred = predIfc;

    method Action update(Bool taken, TourTrainInfo train, Bool mispred);
        // update history if mispred
        if(mispred) begin
            TourGlobalHist newHist = truncateLSB({pack(taken), train.globalHist});
            gHistReg.redirect(newHist);
        end

        predCount <= predCount+1;
        if(mispred)
            misPredCount <= misPredCount + 1;
        $display("Cycle %0d, TOURPRED, predCount = %d, mispred Count = %d\n", cur_cycle, predCount, misPredCount);
        // update local history (assume only 1 branch for an PC in flight)
        localHistTab.upd(train.pcIndex, truncateLSB({pack(taken), train.localHist}));
        // update local sat cnt
        let localCnt = localBht.sub(train.localHist);
        localBht.upd(train.localHist, updateCnt(localCnt, taken));
        // update global sat cnt
        let globalCnt = globalBht.sub(train.globalHist);
        globalBht.upd(train.globalHist, updateCnt(globalCnt, taken));
        // update choice cnt
        if(train.globalTaken != train.localTaken) begin
            Bool useLocal = train.localTaken == taken;
            let choiceCnt = choiceBht.sub(train.globalHist);
            choiceBht.upd(train.globalHist, updateCnt(choiceCnt, useLocal));
        end
    endmethod

    method flush = noAction;
    method flush_done = True;
endmodule
