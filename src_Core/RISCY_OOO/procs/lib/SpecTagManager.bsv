
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

import Vector::*;
import ProcTypes::*;
import HasSpecBits::*;
import Ehr::*;

interface SpecTagManager;
    method SpecBits currentSpecBits;
    method SpecTag  nextSpecTag;
    method Action   claimSpecTag;
    method Bool     canClaim;
    interface SpeculationUpdate specUpdate;
    // performance: count full cycle
    method Bool isFull_ehrPort0;
endinterface

(* synthesize *)
module mkSpecTagManager(SpecTagManager);
    Ehr#(2,SpecBits) current_spec_bits_ehr <- mkEhr(0);
    // normal processing & wrong spec use port 0
    Reg#(SpecBits) current_spec_bits = current_spec_bits_ehr[0];
    // correct spec use port 1

    // dependent_chekcpoints[i] is the SpecBits that depend on SpecTag i.
    // i.e., if SpecTag i is incorrect, then dependent_checkpoints[i] are all
    // wrong.
    Vector#(NumSpecTags, Reg#(SpecBits)) dependent_checkpoints <- replicateM(mkReg(0));

    // wrong spec conflict with claim spec tag
    RWire#(void) wrongSpec_claim_conflict <- mkRWire;

    Maybe#(SpecTag) next_spec_tag = tagged Invalid;
    for (Integer i = valueOf(NumSpecTags) - 1 ; i >= 0 ; i = i-1) begin
        if (current_spec_bits[i] == 0) begin
            next_spec_tag = tagged Valid fromInteger(i);
        end
    end

    rule debugSt;
        if ((next_spec_tag == tagged Invalid )) begin 
            $fdisplay(stdout, "SpecTag manager locked");
        end
    endrule

    method SpecBits currentSpecBits;
        return current_spec_bits;
    endmethod
    method SpecTag nextSpecTag if (next_spec_tag matches tagged Valid .valid_spec_tag);
        return valid_spec_tag;
    endmethod
    method Action claimSpecTag if (next_spec_tag matches tagged Valid .valid_spec_tag);
        current_spec_bits[valid_spec_tag] <= 1;

        for (Integer i = 0 ; i < valueOf(NumSpecTags) ; i = i+1) begin
            if (fromInteger(i) == valid_spec_tag) begin
                dependent_checkpoints[valid_spec_tag] <= (1 << valid_spec_tag);
            end else if (current_spec_bits[i] == 1) begin
                dependent_checkpoints[i] <= dependent_checkpoints[i] | (1 << valid_spec_tag);
            end
        end
        // conflict with wrong spec
        wrongSpec_claim_conflict.wset(?);
    endmethod
    method Bool canClaim = isValid(next_spec_tag);
    interface SpeculationUpdate specUpdate;
        method Action incorrectSpeculation(Bool killAll, SpecTag tag);
            if(killAll) begin
                current_spec_bits <= 0;
            end
            else begin
                current_spec_bits <= current_spec_bits & (~(dependent_checkpoints[tag]));
            end
            // conflict with claim spec tag
            wrongSpec_claim_conflict.wset(?);
        endmethod
        method Action correctSpeculation(SpecBits mask);
            current_spec_bits_ehr[1] <= current_spec_bits_ehr[1] & mask;
        endmethod
    endinterface

    method Bool isFull_ehrPort0;
        return next_spec_tag == Invalid;
    endmethod
endmodule
