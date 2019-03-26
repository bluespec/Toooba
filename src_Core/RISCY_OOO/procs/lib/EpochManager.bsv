
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
import Ehr::*;
import Types::*;
import ProcTypes::*;

typedef struct {
    Epoch curEp;
    Epoch checkedEp;
} EpochDebugState deriving(Bits, Eq, FShow);

interface EM_checkEpoch;
    method Bool check(Epoch e);
endinterface

interface EM_updatePrevEpoch;
    method Action update(Epoch e);
endinterface

interface EpochManager;
    interface Vector#(SupSize, EM_checkEpoch) checkEpoch;
    interface Vector#(SupSize, EM_updatePrevEpoch) updatePrevEpoch;
    method Epoch getEpoch;
    method Action incrementEpoch;
    // for debug
    method EpochDebugState getEpochState;
    // performance: count full cycle
    method Bool isFull_ehrPort0;
endinterface

(* synthesize *)
module mkEpochManager(EpochManager);
    Reg#(Epoch) curr_epoch <- mkReg(0);
    Reg#(Epoch) prev_checked_epoch <- mkReg(0);
    Epoch next_epoch = (curr_epoch== fromInteger(valueOf(NumEpochs)-1)) ? 0 : (curr_epoch+1);

    // epochs in the core are within range [prev_checked_epoch, curr_epoch]
    // prev_checked_epoch can be updated in a lazy way
    Vector#(SupSize, Ehr#(2, Maybe#(Epoch))) updatePrevEn <- replicateM(mkEhr(Invalid));

    (* fire_when_enabled, no_implicit_conditions *)
    rule canon_prev_checked_epoch;
        Vector#(SupSize, Maybe#(Epoch)) updates = readVEhr(1, updatePrevEn);
        // find the last update
        if(find(isValid, reverse(updates)) matches tagged Valid .upd) begin
            doAssert(isValid(upd), "must be valid");
            prev_checked_epoch <= validValue(upd);
        end
        // reset EHR
        for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
            updatePrevEn[i][1] <= Invalid;
        end
    endrule

    Vector#(SupSize, EM_updatePrevEpoch) updateIfc;
    for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
        updateIfc[i] = (interface EM_updatePrevEpoch;
            method Action update(Epoch e);
                updatePrevEn[i][0] <= Valid (e); // record update action
`ifdef BSIM
                // sanity check
                Epoch checkedEpoch = prev_checked_epoch;
                for(Integer j = 0; j < i; j = j+1) begin
                    if(updatePrevEn[j][1] matches tagged Valid .ep) begin
                        checkedEpoch = ep;
                    end
                end
                if(checkedEpoch <= curr_epoch) begin
                    doAssert(checkedEpoch <= e && e <= curr_epoch, "e in [checkedEpoch, curr_epoch]");
                end
                else begin
                    doAssert(checkedEpoch <= e || e <= curr_epoch, "e in [checkedEpoch, max] + [0, curr_epoch]");
                end
`endif
            endmethod
        endinterface);
    end

    Vector#(SupSize, EM_checkEpoch) checkIfc;
    for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
        checkIfc[i] = (interface EM_checkEpoch;
            method Bool check(Epoch e);
                return (e == curr_epoch);
            endmethod
        endinterface);
    end

    interface updatePrevEpoch = updateIfc;

    interface checkEpoch = checkIfc;

    method Epoch getEpoch;
        return curr_epoch;
    endmethod
    method Action incrementEpoch if(prev_checked_epoch != next_epoch);
        curr_epoch <= next_epoch;
    endmethod

    method EpochDebugState getEpochState;
        return EpochDebugState {
            curEp: curr_epoch,
            checkedEp: prev_checked_epoch
        };
    endmethod

    method Bool isFull_ehrPort0;
        return next_epoch == prev_checked_epoch;
    endmethod
endmodule

