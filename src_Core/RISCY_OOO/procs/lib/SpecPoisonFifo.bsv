
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
    Integer valid_deq_port = 0;
    Integer valid_enq_port = 1;
    Integer poisoned_deq_port = 0;
    Integer poisoned_wrongSpec_port = 0;
    Integer poisoned_enq_port = 1;
    Integer sb_deq_port = 0;
    Integer sb_wrongSpec_port = 0;
    Integer sb_enq_port = 0;
    Integer sb_correctSpec_port = 1;

    Vector#(size, Ehr#(2, Bool)) valid <- replicateM(mkEhr(False));
    Vector#(size, Ehr#(2, Bool)) poisoned <- replicateM(mkEhr(?));
    Vector#(size, Reg#(t)) row <- replicateM(mkRegU);
    Vector#(size, Ehr#(2, SpecBits)) specBits <- replicateM(mkEhr(?));

    RWire#(void) wrongSpec_enq_conflict <- mkRWire;

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
            valid_for_enq_wire <= valid[enqP][0];
        endrule
        valid_for_enq = valid_for_enq_wire;
    end
    else begin
        valid_for_enq = valid[enqP][valid_enq_port];
    end

    method Action enq(ToSpecFifo#(t) x) if(!valid_for_enq);
        valid[enqP][valid_enq_port] <= True;
        poisoned[enqP][poisoned_enq_port] <= False;
        row[enqP] <= x.data;
        specBits[enqP][sb_enq_port] <= x.spec_bits;
        enqP <= getNextPtr(enqP);
        // make conflict with wrongSpec
        wrongSpec_enq_conflict.wset(?);
    endmethod

    method Action deq if(valid[deqP][valid_deq_port]);
        valid[deqP][valid_deq_port] <= False;
        // poison bit does not need reset
        deqP <= getNextPtr(deqP);
    endmethod
    
    method ToSpecFifo#(t) first_data if(valid[deqP][valid_deq_port]);
        return ToSpecFifo {
            data: row[deqP],
            spec_bits: specBits[deqP][sb_deq_port]
        };
    endmethod
    
    method Bool first_poisoned if(valid[deqP][valid_deq_port]);
        return poisoned[deqP][poisoned_deq_port];
    endmethod
    
    interface SpeculationUpdate specUpdate;
        method Action incorrectSpeculation(Bool kill_all, SpecTag x);
            // poison entries
            for(Integer i = 0 ; i < valueOf(size) ; i = i+1) begin
                if(kill_all || specBits[i][sb_wrongSpec_port][x] == 1) begin
                    poisoned[i][poisoned_wrongSpec_port] <= True;
                end
            end
            // make conflicts with enq
            wrongSpec_enq_conflict.wset(?);
        endmethod
        method Action correctSpeculation(SpecBits mask);
            // clear spec bits for all entries
            for(Integer i = 0 ; i < valueOf(size) ; i = i+1) begin
                let new_spec_bits = specBits[i][sb_correctSpec_port] & mask;
                specBits[i][sb_correctSpec_port] <= new_spec_bits;
            end
        endmethod
    endinterface
endmodule
