
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
import HList::*; // for Gettable typeclass
import Vector::*;

// actions that can be applied to data structures
interface SpeculationUpdate;
    method Action incorrectSpeculation(Bool kill_all, SpecTag kill_tag);
    method Action correctSpeculation(SpecBits mask); // mask out corrected spec bits, i.e. sb <= sb & mask
endinterface

function SpeculationUpdate joinSpeculationUpdate( Vector#(n, SpeculationUpdate) vec );
    return (interface SpeculationUpdate;
            method Action incorrectSpeculation(Bool all, SpecTag tag);
                for (Integer i = 0 ; i < valueOf(n) ; i = i+1) begin
                    vec[i].incorrectSpeculation(all, tag);
                end
            endmethod
            method Action correctSpeculation(SpecBits mask);
                for (Integer i = 0 ; i < valueOf(n) ; i = i+1) begin
                    vec[i].correctSpeculation(mask);
                end
            endmethod
        endinterface);
endfunction

