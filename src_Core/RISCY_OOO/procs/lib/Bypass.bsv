
// Copyright (c) 2017 Massachusetts Institute of Technology
//
//-
// RVFI_DII + CHERI modifications:
//     Copyright (c) 2020 Jonathan Woodruff
//     All rights reserved.
//
//     This software was developed by SRI International and the University of
//     Cambridge Computer Laboratory (Department of Computer Science and
//     Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
//     DARPA SSITH research programme.
//
//     This work was supported by NCSC programme grant 4212611/RFA 15971 ("SafeBet").
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

import Types::*;
import ProcTypes::*;
import Vector::*;
import CHERICap::*;
import CHERICC_Fat::*;

interface SendBypass;
    method Action send(PhyRIndx dst, CapPipe data);
endinterface

interface RecvBypass;
    method Action recv(PhyRIndx dst, CapPipe data);
endinterface

function ActionValue#(dataType) readRFBypass(
    PhyRIndx src,
    Bool rfReady, dataType rfData,
    Vector#(n, RWire#(Tuple2#(PhyRIndx, dataType))) bypassWire
);
actionvalue
    if(rfReady) begin
        return rfData;
    end
    else begin
        // search through all bypass
        function Maybe#(dataType) getBypassData(RWire#(Tuple2#(PhyRIndx, dataType)) w);
            if(w.wget matches tagged Valid {.dst, .d} &&& dst == src) begin
                return Valid (d);
            end
            else begin
                return Invalid;
            end
        endfunction
        Vector#(n, Maybe#(dataType)) bypassData = map(getBypassData, bypassWire);
        if(find(isValid, bypassData) matches tagged Valid .b) begin
            doAssert(isValid(b), "bypass found must be valid");
            return validValue(b);
        end
        else begin
            when(False, noAction);
            return ?;
        end
    end
endactionvalue
endfunction

function RecvBypass getRecvBypassIfc(RWire#(Tuple2#(PhyRIndx, CapPipe)) w);
    return (interface RecvBypass;
        method Action recv(PhyRIndx dst, CapPipe data);
            w.wset(tuple2(dst, data));
        endmethod
    endinterface);
endfunction

function RecvBypass getRecvBypassDataIfc(RWire#(Tuple2#(PhyRIndx, Data)) w);
    return (interface RecvBypass;
        method Action recv(PhyRIndx dst, CapPipe data);
            w.wset(tuple2(dst, getAddr(data)));
        endmethod
    endinterface);
endfunction
