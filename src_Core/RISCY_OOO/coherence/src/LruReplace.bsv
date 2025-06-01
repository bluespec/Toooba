
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
import CCTypes::*;
import RWBramCore::*;

// True LRU

// vec[0] -- MRU, vec[wayNum-1] -- LRU
typedef Vector#(wayNum, Bit#(TLog#(wayNum))) TrueLruRepInfo#(numeric type wayNum);

module mkTrueLruReplace(ReplacePolicy#(wayNum, TrueLruRepInfo#(wayNum))) provisos(
    Add#(1, a__, wayNum),
    Alias#(wayT, Bit#(TLog#(wayNum))),
    Alias#(repT, TrueLruRepInfo#(wayNum))
);
    // rand rep as fall back if LRU way is locked
    Reg#(wayT) randWay <- mkReg(0);
    rule tick;
        randWay <= randWay == fromInteger(valueOf(wayNum) - 1) ? 0 : randWay + 1;
    endrule

    method repT initRepInfo;
        return genWith(fromInteger);
    endmethod

    method Maybe#(wayT) getReplaceWay(
        Vector#(wayNum, Bool) unlocked,
        Vector#(wayNum, Bool) invalid,
        repT repInfo
    );
        // first search for invalid & unlocked way
        function Bool isInvUnlock(Integer i);
            return unlocked[i] && invalid[i];
        endfunction
        Vector#(wayNum, Integer) idxVec = genVector;
        Maybe#(wayT) repWay = searchIndex(isInvUnlock, idxVec);
        if(!isValid(repWay)) begin
            // check whether LRU way is unlocked
            wayT lruWay = repInfo[valueof(wayNum) - 1];
            if(unlocked[lruWay]) begin
                repWay = Valid (lruWay);
            end
            else begin
                // check if a random way is unlocked
                if(unlocked[randWay]) begin
                    repWay = Valid (randWay);
                end
                else begin
                    // just find a unlocked way
                    repWay = searchIndex(id, unlocked);
                end
            end
        end
        return repWay;
    endmethod

    method repT updateRepInfo(repT repInfo, wayT hitWay);
        repT newInfo = repInfo;
        // find which vector index contains hitWay, and shift rep info
        if(findElem(hitWay, repInfo) matches tagged Valid .idx) begin
            newInfo[0] = hitWay;
            for(Integer i = 0; i < valueof(wayNum) - 1; i = i+1) begin
                if(fromInteger(i) < idx) begin
                    newInfo[i + 1] = repInfo[i];
                end
            end
        end
        return newInfo;
    endmethod
endmodule
