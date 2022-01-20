
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
import DReg::*;
import Types::*;
import SpecFifo::*;

interface SpecPoisonFifo#(numeric type size, type t);
    method Action enq(ToSpecFifo#(t) x);
    method Action deq;
    method ToSpecFifo#(t) first_data;
    method Bool first_poisoned;
    interface SpeculationUpdate specUpdate;
endinterface

typedef struct {
    Bool kill_all;
    SpecTag specTag;
} IncorrectSpeculation deriving(Bits, Eq, FShow);

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
    Integer cannon_port = 0;

    Ehr#(3, Vector#(size, Bool)) valid <- mkEhr(replicate(False));
    Ehr#(3, Vector#(size, Bool)) poisoned <- mkEhr(?);
    Vector#(size, Reg#(t)) row <- replicateM(mkRegU);
    Ehr#(3, Vector#(size, SpecBits)) specBits <- mkEhr(?);

    Reg#(Maybe#(SpecBits))                correctSpec <- mkDReg(Invalid);
    Reg#(Maybe#(IncorrectSpeculation))  incorrectSpec <- mkDReg(Invalid);

    Reg#(idxT) enqP <- mkReg(0);
    Reg#(idxT) deqP <- mkReg(0);

    function idxT getNextPtr(idxT p);
        return p == fromInteger(valueOf(size) - 1) ? 0 : p + 1;
    endfunction

    (* fire_when_enabled, no_implicit_conditions *)
    rule canon_speculation;
        Vector#(size, SpecBits) newSpecBits = specBits[cannon_port];
        Vector#(size, Bool) newValid = valid[cannon_port];
        Vector#(size, Bool) newPoisoned = poisoned[cannon_port];
        if (correctSpec matches tagged Valid .mask) begin
            // clear spec bits for all entries
            for(Integer i = 0 ; i < valueOf(size) ; i = i+1) begin
                let new_spec_bits = newSpecBits[i] & mask;
                newSpecBits[i] = new_spec_bits;
            end
        end
        if (incorrectSpec matches tagged Valid .incSpec) begin
            // poison entries
            for(Integer i = 0 ; i < valueOf(size) ; i = i+1) begin
                if(incSpec.kill_all || newSpecBits[i][incSpec.specTag] == 1) begin
                    newPoisoned[i] = True;
                end
            end
        end
        specBits[cannon_port] <= newSpecBits;
        valid[cannon_port] <= newValid;
        poisoned[cannon_port] <= newPoisoned;
    endrule

    Bool valid_for_enq = ?;
    if(lazyEnq) begin
        // use EHR port 0
        Wire#(Bool) valid_for_enq_wire <- mkBypassWire;
        (* fire_when_enabled, no_implicit_conditions *)
        rule setEnqWire;
            valid_for_enq_wire <= valid[sb_enq_port][enqP];
        endrule
        valid_for_enq = valid_for_enq_wire;
    end
    else begin
        valid_for_enq = valid[valid_enq_port][enqP];
    end

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
        method Action incorrectSpeculation(Bool kill_all, SpecTag specTag) =
           incorrectSpec._write(Valid(IncorrectSpeculation{kill_all: kill_all, specTag: specTag}));
        method Action correctSpeculation(SpecBits mask) = correctSpec._write(Valid(mask));
    endinterface
endmodule
