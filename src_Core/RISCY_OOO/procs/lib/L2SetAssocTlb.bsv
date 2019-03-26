
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

`include "ProcConfig.bsv"

import Vector::*;
import Ehr::*;
import Types::*;
import ProcTypes::*;
import TlbTypes::*;
import SetAssocTlb::*;

// L2 4KB page set assoc TLB
typedef `LOG_L2_TLB_4KB_WAYS LogL2TlbWayNum;
typedef TExp#(LogL2TlbWayNum) L2TlbWayNum;
typedef Bit#(LogL2TlbWayNum) L2TlbWay;

typedef `LOG_L2_TLB_4KB_SIZE LogL2TlbSize;
typedef TSub#(LogL2TlbSize, LogL2TlbWayNum) LogL2TlbSetNum;

// use true LRU: index 0 --MRU, index way-1 -- LRU
typedef Vector#(L2TlbWayNum, L2TlbWay) L2TlbRepInfo;
L2TlbRepInfo l2TlbRepInfoInitVal = genWith(fromInteger);

function L2TlbWay getL2TlbRepWay(L2TlbRepInfo repInfo, Vector#(L2TlbWayNum, Bool) invalid);
    // if there are invalid entries, LRU entry must be invalid
    return repInfo[valueof(L2TlbWayNum) - 1];
endfunction

function L2TlbRepInfo updateL2TlbRepInfo(L2TlbRepInfo repInfo, L2TlbWay way);
    L2TlbRepInfo newInfo = repInfo;
    newInfo[0] = way; // MRU
    Bool findWay = False;
    for(Integer i = 1; i < valueof(L2TlbWayNum); i = i+1) begin
        findWay = findWay || (repInfo[i-1] == way);
        if(!findWay) begin
            newInfo[i] = repInfo[i-1];
        end
    end
    return newInfo;
endfunction

typedef SetAssocTlbResp#(L2TlbWay) L2SetAssocTlbResp;

typedef SetAssocTlb#(L2TlbWayNum, LogL2TlbSetNum, L2TlbRepInfo) L2SetAssocTlb;

// cannot synthesize becaue a method guard depends on input
module mkL2SetAssocTlb(L2SetAssocTlb);
    let m <- mkSetAssocTlb(l2TlbRepInfoInitVal, getL2TlbRepWay, updateL2TlbRepInfo);
    return m;
endmodule
