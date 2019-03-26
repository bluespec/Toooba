
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

export GSelectGHistSz;
export GSelectGHist;
export GSelectTrainInfo;
export mkGSelectPred;

// 1KB gselect predictor

typedef 8 GSelectGHistSz;
typedef 4 PCIndexSz;

typedef Bit#(GSelectGHistSz) GSelectGHist;

typedef TAdd#(GSelectGHistSz, PCIndexSz) BhtIndexSz;
typedef Bit#(BhtIndexSz) BhtIndex;

// bookkeeping info a branch should keep for future training
typedef struct {
    GSelectGHist gHist;
} GSelectTrainInfo deriving(Bits, Eq, FShow);

// global history
typedef GlobalBrHistReg#(GSelectGHistSz) GSelectGHistReg;

(* synthesize *)
module mkGSelectGHistReg(GSelectGHistReg);
    let m <- mkGlobalBrHistReg;
    return m;
endmodule

(* synthesize *)
module mkGSelectPred(DirPredictor#(GSelectTrainInfo));
    // sat counter table
    RegFile#(BhtIndex, Bit#(2)) tab <- mkRegFileWCF(0, maxBound);

    // global branch history
    GSelectGHistReg globalHist <- mkGSelectGHistReg;
    
    // EHR to record predict results in this cycle
    Ehr#(TAdd#(1, SupSize), Bit#(TLog#(TAdd#(SupSize, 1)))) predCnt <- mkEhr(0);
    Ehr#(TAdd#(1, SupSize), Bit#(SupSize)) predRes <- mkEhr(0);

    function BhtIndex getIndex(Addr pc, GSelectGHist gHist);
        Bit#(PCIndexSz) pcIdx = truncate(pc >> 2);
        return {gHist, pcIdx};
    endfunction

    function Bool isTaken(Bit#(2) cnt);
        return cnt[1] == 1;
    endfunction

    function Bit#(2) updateCnt(Bit#(2) cnt, Bool taken);
        if(taken) begin
            return cnt == maxBound ? maxBound : cnt + 1;
        end
        else begin
            return cnt == 0 ? 0 : cnt - 1;
        end
    endfunction

    GSelectGHist curGHist = globalHist.history; // global history: MSB is the latest branch

    Vector#(SupSize, DirPred#(GSelectTrainInfo)) predIfc;
    for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
        predIfc[i] = (interface DirPred;
            method ActionValue#(DirPredResult#(GSelectTrainInfo)) pred(Addr pc);
                // get the global history
                // all previous branch in this cycle must be not taken
                // otherwise this branch should be on wrong path
                // because all inst in same cycle are fetched consecutively
                GSelectGHist gHist = curGHist >> predCnt[i];
                Bool taken = isTaken(tab.sub(getIndex(pc, gHist)));

                // record pred result
                predCnt[i] <= predCnt[i] + 1;
                Bit#(SupSize) res = predRes[i];
                res[predCnt[i]] = pack(taken);
                predRes[i] <= res;
                
                // return
                return DirPredResult {
                    taken: taken,
                    train: GSelectTrainInfo {
                        gHist: gHist
                    }
                };
            endmethod
        endinterface);
    end

    (* fire_when_enabled, no_implicit_conditions *)
    rule canonGlobalHist;
        globalHist.addHistory(predRes[valueof(SupSize)], predCnt[valueof(SupSize)]);
        predRes[valueof(SupSize)] <= 0;
        predCnt[valueof(SupSize)] <= 0;
    endrule

    interface pred = predIfc;

    method Action update(Addr pc, Bool taken, GSelectTrainInfo train, Bool mispred);
        // update history if mispred
        if(mispred) begin
            GSelectGHist newHist = truncate({pack(taken), train.gHist} >> 1);
            globalHist.redirect(newHist);
        end
        // update sat cnt
        let index = getIndex(pc, train.gHist);
        Bit#(2) cnt = tab.sub(index);
        tab.upd(index, updateCnt(cnt, taken));
    endmethod

    method flush = noAction;
    method flush_done = True;
endmodule
