
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

import ProcTypes::*;
import HasSpecBits::*;
import GetPut::*;
import Vector::*;
import ReorderBuffer::*;
import SpecFifo::*;

typedef struct {
    Bool kill_all;
    SpecTag spec_tag;
    InstTag inst_tag;
} IncorrectSpec deriving(Bits, Eq, FShow);

interface GlobalSpecUpdate#(numeric type correctSpecPortNum, numeric type conflictWrongSpecPortNum);
    interface Vector#(correctSpecPortNum, Put#(SpecTag)) correctSpec;
    method Action incorrectSpec(Bool kill_all, SpecTag spec_tag, InstTag inst_tag, SpecBits spec_bits);
    // Some rules (e.g. doFinishFpuMulDiv) in Core.bsv may not conflict with wrong spec
    // and is ordered before rules that calls incorrectSpec
    // this creates cycles in scheduling
    // To break the cycle, such rules can call the following interface
    // to manually create a conflict with rules that do incorrectSpec
    interface Vector#(conflictWrongSpecPortNum, Put#(void)) conflictWrongSpec;
    method Bool pendingIncorrectSpec;
endinterface

module mkGlobalSpecUpdate#(
    SpeculationUpdate ifc, ROB_SpeculationUpdate rob
    // ROB is treatly separatly due to an optimization on killing
)(
    GlobalSpecUpdate#(correctSpecPortNum, conflictWrongSpecPortNum)
);
    // record correct spec tags
    Vector#(correctSpecPortNum, RWire#(SpecTag)) correctSpecTag <- replicateM(mkRWire);
    // make wrong spec conflict with correct spec
    Vector#(correctSpecPortNum, PulseWire) spec_conflict <- replicateM(mkPulseWire);
    // let the caller of conflictWrongSpec to be conflict with wrong spec
    Vector#(conflictWrongSpecPortNum, PulseWire) wrongSpec_conflict <- replicateM(mkPulseWire);
    // must be a single-element fifo to ensure all pushing rules cannot fire while we are waiting
    // to kill.
    SpecFifo#(2,IncorrectSpec,1,1) incorrectSpec_ff <- mkSpecFifoCF(True);

    (* fire_when_enabled, no_implicit_conditions *)
    rule canon_correct_spec;
        SpecBits mask = maxBound;
        for(Integer i = 0; i < valueof(correctSpecPortNum); i = i+1) begin
            if(correctSpecTag[i].wget matches tagged Valid .tag) begin
                mask[tag] = 0;
            end
        end
        incorrectSpec_ff.specUpdate.correctSpeculation(mask);
        ifc.correctSpeculation(mask);
        rob.correctSpeculation(mask);
    endrule

    rule do_incorrect_spec;
        IncorrectSpec x = incorrectSpec_ff.first.data;
        incorrectSpec_ff.deq;
        incorrectSpec_ff.specUpdate.incorrectSpeculation(x.kill_all, x.spec_tag);
        ifc.incorrectSpeculation(x.kill_all, x.spec_tag);
        rob.incorrectSpeculation(x.kill_all, x.spec_tag, x.inst_tag);
        // conflict with correct spec
        for(Integer i = 0; i < valueof(correctSpecPortNum); i = i+1) begin
            spec_conflict[i].send;
        end
        // conflict with the caller of conflictWrongSpec
        for(Integer i = 0; i < valueof(conflictWrongSpecPortNum); i = i+1) begin
            wrongSpec_conflict[i].send;
        end
    endrule

    Vector#(correctSpecPortNum, Put#(SpecTag)) correctVec = ?;
    for(Integer i = 0; i < valueof(correctSpecPortNum); i = i+1) begin
        correctVec[i] = (interface Put;
            method Action put(SpecTag t) if (!spec_conflict[i]);
                correctSpecTag[i].wset(t);
            endmethod
        endinterface);
    end

    Vector#(conflictWrongSpecPortNum, Put#(void)) conflictWrongVec = ?;
    for(Integer i = 0; i < valueof(conflictWrongSpecPortNum); i = i+1) begin
        conflictWrongVec[i] = (interface Put;
            method Action put(void x) if (!wrongSpec_conflict[i]);
                noAction;
            endmethod
        endinterface);
    end

    interface correctSpec = correctVec;

    method Action incorrectSpec(Bool kill_all, SpecTag spec_tag, InstTag inst_tag, SpecBits spec_bits)
        = incorrectSpec_ff.enq(ToSpecFifo{
              data: IncorrectSpec{kill_all: kill_all, spec_tag: spec_tag, inst_tag: inst_tag},
              spec_bits: spec_bits
          });

    interface conflictWrongSpec = conflictWrongVec;

    method Bool pendingIncorrectSpec = incorrectSpec_ff.notEmpty;
endmodule
