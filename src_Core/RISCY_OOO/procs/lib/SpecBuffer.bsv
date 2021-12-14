//     Copyright (c) 2021 Jonathan Woodruff
//     All rights reserved.
//
//     This software was developed by SRI International and the University of
//     Cambridge Computer Laboratory (Department of Computer Science and
//     Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
//     DARPA SSITH research programme.
//-
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
import Vector::*;
import Ehr::*;
import Assert::*;
import ConfigReg::*;
import GetPut::*;
import FIFOF::*;
import Fifos::*;
import ClientServer::*;

typedef struct {
    t data;
    InstTag tag;
} ToSpecBuffer#(type t) deriving(Bits, Eq, FShow);

interface SpecBuffer#(
    numeric type depth, numeric type width, type t,
    type search_t_out, type search_t_in
);
    interface Server#(ToSpecBuffer#(t), t) server;
    method Maybe#(search_t_out) search(search_t_in in);
    interface Vector#(width, Put#(InstTag)) commit;
    method Action killAll;
endinterface

module mkSpecBuffer#(
    function Maybe#(search_t_out) search_f(search_t_in w, t x),
    Bool unguarded_insert
)(
    SpecBuffer#(depth, width, t, search_t_out, search_t_in)
) provisos (
    Alias#(idxT, Bit#(TLog#(depth))),
    Bits#(t, _tsz),
    FShow#(t), Eq#(t),
    NumEq#(TExp#(TLog#(width)), width) // width is power-of-two; required by SupFifo.
);
    Reg#(Vector#(depth, Maybe#(ToSpecBuffer#(t)))) vec <- mkConfigReg(replicate(Invalid));
    Bool full = all(isValid,vec);
    RWire#(ToSpecBuffer#(t)) insert <- mkRWire;
    Vector#(width, FIFOF#(InstTag)) committed <- replicateM(mkUGFIFOF);
    SupFifo#(width,2,t) nonSpeculative <- mkUGSupFifo;
    PulseWire killAll_w <- mkPulseWire;

    (* no_implicit_conditions *)
    rule canon;
        Vector#(depth, Maybe#(ToSpecBuffer#(t))) newVec = vec;
        // Clean out one entry that is non-speculative
        Integer k = 0;
        for (Integer i=0; i<valueOf(width); i=i+1) begin
            if (committed[i].notEmpty) begin
                function Bool tagMatch (Maybe#(ToSpecBuffer#(t)) mtsb);
                    if (mtsb matches tagged Valid .tsb &&& tsb.tag == committed[i].first)
                        return True;
                    else return False;
                endfunction
                if (findIndex(tagMatch,vec) matches tagged Valid .x) begin
                    if (nonSpeculative.enqS[k].canEnq) begin
                        nonSpeculative.enqS[k].enq(fromMaybe(?,vec[i]).data);
                        k = k + 1;
                    end
                    newVec[i] = Invalid;
                end
                // For now, assume at most one record per instruction ID
                //if (!any(tagMatch, newVec))
                committed[i].deq;
            end
        end
        // Insert any new entry.
        if (insert.wget matches tagged Valid .x)
            if (findElem(Invalid,vec) matches tagged Valid .i)
                newVec[i] = Valid(x);
        if (killAll_w) newVec = replicate(Invalid);
        vec <= newVec;
    endrule

    interface Server server;
        interface Put request;
            method put(x) if (!full || unguarded_insert) = insert.wset(x);
        endinterface
        interface Get response;
            method ActionValue#(t) get;
                nonSpeculative.deqS[0].deq;
                return nonSpeculative.deqS[0].first;
            endmethod
        endinterface
    endinterface
    method Maybe#(search_t_out) search(search_t_in in);
        Maybe#(search_t_out) ret = Invalid;
        for(Integer i = 0; i < valueOf(depth); i = i + 1)
            if (vec[i] matches tagged Valid .x)
                if (search_f(in, x.data) matches tagged Valid .y)
                    ret = Valid(y);
        return ret;
    endmethod
    interface commit = map(toPut, committed);
    method Action killAll = killAll_w.send;
endmodule
