
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
import CacheUtils::*;
import CCTypes::*;
import L1Wrapper::*;
import LLWrapper::*;
import CrossBar::*;
import Vector::*;
import GetPut::*;
import ClientServer::*;
import L1Bank::*;
import LLBank::*;
import L1LLSizes::*;
import Connectable::*;

export L1LL(..);
export mkL1LL;

// LLChildNum == L1Num
// XXX no LLC banking: LgLLBankNum = 0

typedef TExp#(LgLLBankNum) LLNum;
typedef Bit#(LgLLBankNum) LLIdx;
typedef Bit#(TLog#(L1Num)) L1Idx;

typedef CRqMsg#(L1Way, void) CRqFromL1;
typedef CRqMsg#(LLCRqId, LLChild) CRqToLL;
typedef CRsMsg#(void) CRsFromL1;
typedef CRsMsg#(LLChild) CRsToLL;
typedef PRqRsMsg#(LLCRqId, LLChild) PRqRsFromLL;
typedef PRqRsMsg#(L1Way, void) PRqRsToL1;

typedef 1 XBarSrcDelay;
typedef 2 XBarDstDelay;

// cross bar for L1 cRq to LL
typedef CrossBar#(L1Num, XBarSrcDelay, CRqFromL1, LLNum, XBarDstDelay, CRqToLL) L1CRqToLLXBar;

(* synthesize *)
module mkL1CRqToLLXBar(L1CRqToLLXBar);
    function XBarDstInfo#(LLIdx, CRqToLL) getL1CRqDstInfo(L1Idx whichL1, CRqFromL1 rq);
        return XBarDstInfo {
            idx: 0,
            data: CRqMsg {
                addr: rq.addr,
                fromState: rq.fromState,
                toState: rq.toState,
                canUpToE: rq.canUpToE,
                id: rq.id,
                child: whichL1
            }
        };
    endfunction

    let m <- mkCrossBar(getL1CRqDstInfo);
    return m;
endmodule

// cross bar for L1 cRs to LL
typedef CrossBar#(L1Num, XBarSrcDelay, CRsFromL1, LLNum, XBarDstDelay, CRsToLL) L1CRsToLLXBar;

(* synthesize *)
module mkL1CRsToLLXBar(L1CRsToLLXBar);
    function XBarDstInfo#(LLIdx, CRsToLL) getL1CRsDstInfo(L1Idx whichL1, CRsFromL1 rs);
        return XBarDstInfo {
            idx: 0,
            data: CRsMsg {
                addr: rs.addr,
                toState: rs.toState,
                data: rs.data,
                child: whichL1
            }
        };
    endfunction

    let m <- mkCrossBar(getL1CRsDstInfo);
    return m;
endmodule

// cross bar for LL pRqRs to L1
typedef CrossBar#(LLNum, XBarSrcDelay, PRqRsFromLL, L1Num, XBarDstDelay, PRqRsToL1) LLPRqRsToL1XBar;

(* synthesize *)
module mkLLPRqRsToL1XBar(LLPRqRsToL1XBar);
    function XBarDstInfo#(L1Idx, PRqRsToL1) getLLPRqRsDstInfo(LLIdx whichLL, PRqRsFromLL msg);
        return (case(msg) matches
            tagged PRq .rq: return XBarDstInfo {
                idx: rq.child,
                data: PRq (PRqMsg {
                    addr: rq.addr,
                    toState: rq.toState,
                    child: ?
                })
            };
            tagged PRs .rs: return XBarDstInfo {
                idx: rs.child,
                data: PRs (PRsMsg {
                    addr: rs.addr,
                    toState: rs.toState,
                    child: ?,
                    data: rs.data,
                    id: rs.id
                })
            };
        endcase);
    endfunction

    let m <- mkCrossBar(getLLPRqRsDstInfo);
    return m;
endmodule

// L1 + LL
// LLC port mapping:
// 0 ~ L1DNum -1 -- D$ 0~L1DNum-1
// L1Dnum ~ L1Num -- I$ 0~L1INum-1

interface L1LL;
    interface Vector#(L1DNum, L1ProcReq#(ProcRqId)) dReq;
    interface Vector#(L1INum, InstServer#(L1ISupSz)) inst;
    interface DmaServer#(DmaRqId) dma;
    interface MemFifoClient#(LdMemRqId#(LLCRqMshrIdx), void) to_mem;
endinterface

module mkL1LL#(Vector#(L1DNum, L1ProcResp#(ProcRqId)) procResp)(L1LL) provisos(
    Add#(0, 0, LgLLBankNum),
    Add#(1, 0, LLNum)
);
    Vector#(L1DNum, L1CacheWrapper) dc = ?;
    for(Integer i = 0; i < valueof(L1DNum); i = i+1) begin
        dc[i] <- mkL1CacheWrapper(procResp[i]);
    end
    
    Vector#(LLNum, LLBankWrapper) llc <- replicateM(mkLLBankWrapper);

    let cRqXBar <- mkL1CRqToLLXBar;
    let cRsXBar <- mkL1CRsToLLXBar;
    let pXBar <- mkLLPRqRsToL1XBar;

    for(Integer i = 0; i < valueOf(L1DNum); i = i+1) begin
        mkConnection(cRqXBar.srcIfc[i], dc[i].to_parent.rqToP);
        mkConnection(cRsXBar.srcIfc[i], dc[i].to_parent.rsToP);
        mkConnection(pXBar.dstIfc[i], dc[i].to_parent.fromP);
    end

    for(Integer i = 0; i < valueOf(LLNum); i = i+1) begin
        mkConnection(cRqXBar.dstIfc[i], llc[i].to_child.rqFromC);
        mkConnection(cRsXBar.dstIfc[i], llc[i].to_child.rsFromC);
        mkConnection(pXBar.srcIfc[i], llc[i].to_child.toC);
    end

    function L1ProcReq#(ProcRqId) getDReqIfc(L1CacheWrapper ifc);
        return ifc.procReq;
    endfunction

    interface dReq = map(getDReqIfc, dc);
    interface dma = llc[0].dma;
    interface to_mem = llc[0].to_mem;
endmodule
