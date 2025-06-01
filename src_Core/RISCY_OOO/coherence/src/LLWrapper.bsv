
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

import Vector::*;
import CacheUtils::*;
import Types::*;
import CCTypes::*;
import LLPipe::*;
import LLCRqMshr::*;
import LLBank::*;
import L1LLSizes::*;


(* synthesize *)
module mkLastLvCRqMshr(
    LLCRqMshr#(LLCRqNum, LLWay, LLTag, Vector#(LLChildNum, DirPend), cRqT)
) provisos(
    Alias#(cRqT, LLRq#(LLCRqId, DmaRqId, LLChild))
);
    function Addr getAddr(cRqT r) = r.addr;
    let m <- mkLLCRqMshr(getAddr, getNeedReqChild, getDirPendInitVal);
    return m;
endmodule

(* synthesize *)
module mkLLPipeline(
    LLPipe#(LgLLBankNum, LLChildNum, LLWayNum, LLIndex, LLTag, LLCRqMshrIdx)
);
    let m <- mkLLPipe;
    return m;
endmodule

typedef LLBank#(LgLLBankNum, LLChildNum, LLWayNum, LLIndexSz, LLTagSz, LLCRqNum, LLCRqId, DmaRqId) LLBankWrapper;

(* synthesize *)
module mkLLBankWrapper(LLBankWrapper);
    // resp load req with E when it fills cache line from mem
    function Bool respE(Bool fromMem) = fromMem;
    let m <- mkLLBank(mkLastLvCRqMshr, mkLLPipeline, respE);
    return m;
endmodule
