
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
import Vector::*;
import Ehr::*;
import Types::*;
import SpecFifo::*;
import ConfigReg::*;
import FIFOF::*;

interface SpecPoisonFifo#(numeric type size, type t);
    method Action enq(ToSpecFifo#(t) x);
    method Action deq;
    method ToSpecFifo#(t) first_data;
    method Bool first_poisoned;
    interface SpeculationUpdate specUpdate;
endinterface

module mkSpecPoisonFifo#(Bool lazyEnq)(
    SpecPoisonFifo#(size, t)
) provisos(
    Alias#(idxT, Bit#(TLog#(size))),
    Bits#(t, _tsz), FShow#(t)
);
    // deq < enq < correctSpec
    // deq read poison, wrongSpec write poison, can use same EHR port
    // wrongSpec conflict enq
    Integer valid_deq_port = 1;
    Integer valid_enq_port = 2;
    Integer poisoned_deq_port = 1;
    Integer poisoned_wrongSpec_port = 1;
    Integer poisoned_enq_port = 2;
    Integer sb_deq_port = 1;
    Integer sb_enq_port = 1;

    Ehr#(3, Vector#(size, Bool)) valid <- mkEhr(replicate(False));
    Ehr#(3, Vector#(size, Bool)) poisoned <- mkEhr(?);
    Vector#(size, Reg#(t)) row <- replicateM(mkConfigRegU);
    Ehr#(3, Vector#(size, SpecBits)) specBits <- mkEhr(?);

    FIFOF#(SpecBits) correctSpecF <- mkUGFIFOF;
    FIFOF#(IncorrectSpeculation) incorrectSpecF <- mkUGFIFOF;

    Reg#(idxT) enqP <- mkReg(0);
    Reg#(idxT) deqP <- mkReg(0);

    function idxT getNextPtr(idxT p);
        return p == fromInteger(valueOf(size) - 1) ? 0 : p + 1;
    endfunction

    Bool valid_for_enq = ?;
    if(lazyEnq) begin
        // use EHR port 0
        Wire#(Bool) valid_for_enq_wire <- mkBypassWire;
        (* fire_when_enabled, no_implicit_conditions *)
        rule setEnqWire;
            valid_for_enq_wire <= valid[1][enqP];
        endrule
        valid_for_enq = valid_for_enq_wire;
    end
    else begin
        valid_for_enq = valid[valid_enq_port][enqP];
    end

    (* fire_when_enabled, no_implicit_conditions *)
    rule canon_speculation;
        Vector#(size, SpecBits) newSpecBits = specBits[0];
        Vector#(size, Bool) newPoisened = poisoned[0];
        // Fold in CorrectSpec update:
        if (correctSpecF.notEmpty) begin
            SpecBits mask = correctSpecF.first();
            correctSpecF.deq();
            // clear spec bits for all entries
            for (Integer i=0; i<valueOf(size); i=i+1)
                newSpecBits[i] = newSpecBits[i] & mask;
        end
        // Fold in IncorrectSpec update:
        if (incorrectSpecF.notEmpty) begin
            IncorrectSpeculation incSpec = incorrectSpecF.first();
            incorrectSpecF.deq();
            // clear entries
            for(Integer i = 0 ; i < valueOf(size) ; i = i+1)
                if(incSpec.kill_all || newSpecBits[i][incSpec.specTag] == 1)
                    newPoisened[i] = True;
        end
        specBits[0] <= newSpecBits;
        poisoned[0] <= newPoisened;
    endrule

    method Action enq(ToSpecFifo#(t) x) if(!valid_for_enq);
        valid[valid_enq_port][enqP] <= True;
        poisoned[poisoned_enq_port][enqP] <= False;
        row[enqP] <= x.data;
        specBits[sb_enq_port][enqP] <= x.spec_bits;
        enqP <= getNextPtr(enqP);
    endmethod

    method Action deq if(valid[valid_deq_port][deqP]);
        valid[valid_deq_port][deqP] <= False;
        // poison bit does not need reset
        deqP <= getNextPtr(deqP);
    endmethod

    method ToSpecFifo#(t) first_data if(valid[valid_deq_port][deqP]);
        return ToSpecFifo {
            data: row[deqP],
            spec_bits: specBits[sb_deq_port][deqP]
        };
    endmethod

    method Bool first_poisoned if(valid[valid_deq_port][deqP]);
        return poisoned[poisoned_deq_port][deqP];
    endmethod

    interface SpeculationUpdate specUpdate;
        //method correctSpeculation = correctSpecF.enq;
        //method incorrectSpeculation(kill_all, specTag) =
        //    incorrectSpecF.enq(IncorrectSpeculation{kill_all: kill_all, specTag: specTag});
    endinterface
endmodule
