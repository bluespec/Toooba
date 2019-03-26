
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

import Types::*;
import ProcTypes::*;
import Vector::*;

interface SendBypass;
    method Action send(PhyRIndx dst, Data data);
endinterface

interface RecvBypass;
    method Action recv(PhyRIndx dst, Data data);
endinterface

function ActionValue#(Data) readRFBypass(
    PhyRIndx src,
    Bool rfReady, Data rfData,
    Vector#(n, RWire#(Tuple2#(PhyRIndx, Data))) bypassWire
);
actionvalue
    if(rfReady) begin
        return rfData;
    end
    else begin
        // search through all bypass
        function Maybe#(Data) getBypassData(RWire#(Tuple2#(PhyRIndx, Data)) w);
            if(w.wget matches tagged Valid {.dst, .d} &&& dst == src) begin
                return Valid (d);
            end
            else begin
                return Invalid;
            end
        endfunction
        Vector#(n, Maybe#(Data)) bypassData = map(getBypassData, bypassWire);
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

function RecvBypass getRecvBypassIfc(RWire#(Tuple2#(PhyRIndx, Data)) w);
    return (interface RecvBypass;
        method Action recv(PhyRIndx dst, Data data);
            w.wset(tuple2(dst, data));
        endmethod
    endinterface);
endfunction

