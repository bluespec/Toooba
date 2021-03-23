
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
import Vector::*;

// Type parameters are for index and key (which together are the "address"),
// the value stored in the map, and the associativity of the storage.
interface Map#(type ix, type ky, type vl, numeric type as);
    method Action update(Tuple2#(ky,ix) key, vl value);
    method Maybe#(vl) lookup(Tuple2#(ky,ix) lookup_key);
endinterface

module mkMapLossy(Map#(ix,ky,vl,as)) provisos (
Bits#(ky,ky_sz), Bits#(vl,vl_sz), Eq#(ky), Arith#(ky),
Bounded#(ix), Literal#(ix), Bits#(ix, ix_sz));
    Vector#(as, RegFile#(ix, Tuple2#(ky,vl))) mem
        <- replicateM(mkRegFileWCF(0, maxBound));
    Reg#(Bit#(TLog#(as))) wayNext <- mkReg(0);
    Integer a = valueof(as);

    method Action update(Tuple2#(ky,ix) ki, vl value);
        match {.key, .index} = ki;
        mem[wayNext].upd(index, tuple2(key, value));
        wayNext <= (wayNext == fromInteger(a-1)) ? 0: wayNext + 1;
    endmethod
    method Maybe#(vl) lookup(Tuple2#(ky,ix) ki);
        match {.lookup_key, .index} = ki;
        Maybe#(vl) ret = Invalid;
        for (Integer i = 0; i < a; i = i + 1) begin
            match {.key, .value} = mem[i].sub(index);
            if (key == lookup_key) ret = Valid(value);
        end
        return ret;
    endmethod
endmodule
