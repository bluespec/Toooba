
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
import Vector::*;
import BrPred::*;

export BhtTrainInfo;
export mkBht;
export BhtEntries;
export BhtIndex;

// Local BHT Typedefs
typedef 128 BhtEntries;
typedef Bit#(TLog#(BhtEntries)) BhtIndex;

typedef BhtIndex BhtTrainInfo;

(* synthesize *)
module mkBht(DirPredictor#(BhtTrainInfo));
    // Read and Write ordering doesn't matter since this is a predictor
    // mkRegFileWCF is the RegFile version of mkConfigReg
    RegFile#(BhtIndex, Bit#(2)) hist <- mkRegFileWCF(0,fromInteger(valueOf(BhtEntries)-1));
    Reg#(Addr) pc_reg <- mkRegU;

    function BhtIndex getIndex(Addr pc);
        return truncate(pc >> 2);
    endfunction

    Vector#(SupSize, DirPred#(BhtTrainInfo)) predIfc;
    for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
        predIfc[i] = (interface DirPred;
            method ActionValue#(DirPredResult#(BhtTrainInfo)) pred;
                let index = getIndex(offsetPc(pc_reg, i));
                Bit#(2) cnt = hist.sub(index);
                Bool taken = cnt[1] == 1;
                return DirPredResult {
                    taken: taken,
                    train: index
                };
            endmethod
        endinterface);
    end

    method nextPc = pc_reg._write;

    interface pred = predIfc;

    method Action update(Bool taken, BhtTrainInfo train, Bool mispred);
        let index = train;
        let current_hist = hist.sub(index);
        Bit#(2) next_hist;
        if(taken) begin
            next_hist = (current_hist == 2'b11) ? 2'b11 : current_hist + 1;
        end else begin
            next_hist = (current_hist == 2'b00) ? 2'b00 : current_hist - 1;
        end
        hist.upd(index, next_hist);
    endmethod

    method flush = noAction;
    method flush_done = True;
endmodule

