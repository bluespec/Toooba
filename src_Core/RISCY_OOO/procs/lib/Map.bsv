
// Copyright (c) 2021 Jonathan Woodruff
//
// All rights reserved.
//
// This software was developed by SRI International and the University of
// Cambridge Computer Laboratory (Department of Computer Science and
// Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
// DARPA SSITH research programme.
//
// This work was supported by NCSC programme grant 4212611/RFA 15971 ("SafeBet").
//
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

import RegFile::*;

function inT poorMansMod(inT val, Integer i)
    provisos (Bits#(inT, inW));
    Bit#(inW) mask = ~((~0) << log2(i));
    Bit#(inW) low = pack(val) & mask;
    Bit#(inW) max = fromInteger(i);
    return unpack(pack(val) & (mask >> ((low < max) ? 0:1)));
endfunction

interface Map#(type i, type k, type v);
    method Action update(Tuple2#(k,i) key, v value);
    method Maybe#(v) lookup(Tuple2#(k,i) lookup_key);
endinterface

module mkMapStatic(Map#(i,k,v)) provisos (
Bits#(k,k_sz), Bits#(v,v_sz), Eq#(k), Arith#(k),
Bounded#(i), Literal#(i), Bits#(i, a__));
    RegFile#(i, Tuple2#(k,v)) mem <- mkRegFileWCF(0, maxBound);

    method Action update(Tuple2#(k,i) ki, v value);
        match {.key, .index} = ki;
        $display("[Map - update] k: %x", key, " v: %x", value, " index: %x", index);
        mem.upd(index, tuple2(key, value));
    endmethod
    method Maybe#(v) lookup(Tuple2#(k,i) ki);
        match {.lookup_key, .index} = ki;
        match {.key, .value} = mem.sub(index);
        return (key == lookup_key) ? Valid(value):Invalid;
    endmethod
endmodule
