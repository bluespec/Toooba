
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
import Fifos::*;
import DReg::*;

import Cur_Cycle :: *;

export TourLocalHistSz;
export TourLocalHist;
export TourGlobalHistSz;
export TourGlobalHist;
export TourTrainInfo(..);
export TourGHistReg(..);
export mkTourGHistReg;
export mkTourPredStaged;
export PCIndexSz;
export PCIndex;
export TourPredSpecInfo;

// 4KB tournament predictor

typedef 12 TourGlobalHistSz;
typedef 10 TourLocalHistSz;
typedef 10 PCIndexSz;

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

typedef Bit#(1) TourPredSpecInfo;

// global history reg
typedef GlobalBrHistReg#(TourGlobalHistSz) TourGHistReg;

(* synthesize *)
module mkTourGHistReg(TourGHistReg);
    let m <- mkGlobalBrHistReg;
    return m;
endmodule

//(* synthesize *)
module mkTourPredStaged#(Vector#(SupSize, SupFifoEnq#(GuardedResult#(TourTrainInfo, TourPredSpecInfo))) outInf)(DirPredictor#(TourTrainInfo, TourPredSpecInfo));
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
    
    // Lookup PC
    Vector#(SupSize, RWire#(PredIn)) predIn <- replicateM(mkRWire);
    Vector#(SupSize, Reg#(Maybe#(GuardedResult#(TourTrainInfo, TourPredSpecInfo)))) pred1ToPred2 <- replicateM(mkDReg(tagged Invalid));

    // EHR to record predict results in this cycle
    Ehr#(TAdd#(1, SupSize), SupCnt) predCnt <- mkEhr(0);
    Ehr#(TAdd#(1, SupSize), Bit#(SupSize)) predRes <- mkEhr(0);

    Reg#(UInt#(64)) predCount <- mkReg(0);
    Reg#(UInt#(64)) misPredCount <- mkReg(0);


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

    for (Integer i = 0; i < valueOf(SupSize); i = i + 1) begin
        rule predStageOne(predIn[i].wget matches tagged Valid .in);
            $display("Prediction on %x\n", in.pc);
            PCIndex pcIndex = getPCIndex(offsetPc(in.pc, i));
            // get local history & prediction
            TourLocalHist localHist = localHistTab.sub(pcIndex);
            Bool localTaken = isTaken(localBht.sub(localHist));

            // TODO: add speculative before
            Bool globalTaken = globalTakenVec[0];

            // make choice
            Bool useLocal = useLocalVec[0];
            Bool taken = useLocal ? localTaken : globalTaken;

            // return
            pred1ToPred2[i] <= tagged Valid GuardedResult {
                result: DirPredResult{
                    taken: taken,
                    train: TourTrainInfo {
                        globalHist: curGHist >> predCnt[i],
                        localHist: localHist,
                        globalTaken: globalTaken,
                        localTaken: localTaken,
                        pcIndex: pcIndex
                    },
                    pc: in.pc,
                    spec: 0
                },
                main_epoch: in.main_epoch,
                decode_epoch: in.decode_epoch
            };
        endrule
    end

    // Should fix schedueling as it will make pred1 < pred2 when they actually should be conflict free

    // Very awkward, it doesn't like me doing it directly, as it thinks there is a conflict
    rule predStageTwo;
        Vector#(SupSize, GuardedResult#(TourTrainInfo, TourPredSpecInfo)) results = replicate(unpack(0));
        Bit#(TAdd#(TLog#(SupSize),1)) count = 0;
        for (Integer i = 0; i < valueOf(SupSize); i = i + 1) begin
            if(pred1ToPred2[i] matches tagged Valid .in) begin
                $display("Predict2 detected %x %d", results[i].result.pc, i);
                results[count] = in;
                count = count + 1;
            end
        end

        for (Integer i = 0; fromInteger(i) < valueOf(SupSize) && fromInteger(i) < count; i = i + 1) begin
            if(outInf[i].canEnq) begin
                    $display("Predict enqueue %x onto %d", results[i].result.pc, i);
                    outInf[i].enq(results[i]);
            end
            else
                doAssert(False, "FAIL TO ENQUEUE PRED2");
        end
    endrule
    

    method Action confirmPred(Bit#(SupSize) results, SupCnt count);
        predCnt[0] <= count;
        predRes[0] <= results;
    endmethod

    method Action nextPc(Vector#(SupSize,Maybe#(PredIn)) next);
        for(Integer i = 0; i < valueOf(SupSize); i = i + 1) begin
            if (next[i] matches tagged Valid .n)
                predIn[i].wset(n);
        end
    endmethod

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

    method Action specRecover(TourPredSpecInfo dummy, Bool taken) = noAction;

    method flush = noAction;
    method flush_done = True;
endmodule
