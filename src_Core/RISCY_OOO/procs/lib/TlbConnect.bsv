
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

import GetPut::*;
import ClientServer::*;
import Connectable::*;
import Types::*;
import ProcTypes::*;
import TlbTypes::*;
import CacheUtils::*;
import ITlb::*;
import DTlb::*;
import L2Tlb::*;

module mkTlbConnect#(ITlbToParent i, DTlbToParent d, L2TlbToChildren l2)(Empty);
    // give priority to DTlb req
    (* descending_urgency = "sendDTlbReq, sendITlbReq" *)
    rule sendDTlbReq;
        DTlbRqToP r <- toGet(d.rqToP).get;
        l2.rqFromC.put(L2TlbRqFromC {
            child: D (r.id),
            vpn: r.vpn
        });
    endrule

    rule sendITlbReq;
        ITlbRqToP r <- toGet(i.rqToP).get;
        l2.rqFromC.put(L2TlbRqFromC {
            child: I,
            vpn: r.vpn
        });
    endrule

    rule sendRsToDTlb(l2.rsToC.first.child matches tagged D .id);
        L2TlbRsToC r <- toGet(l2.rsToC).get;
        d.ldTransRsFromP.enq(DTlbTransRsFromP {
            entry: r.entry,
            id: id
        });
    endrule

    rule sendRsToITlb(l2.rsToC.first.child == I);
        L2TlbRsToC r <- toGet(l2.rsToC).get;
        i.rsFromP.enq(ITlbRsFromP {entry: r.entry});
    endrule

    mkConnection(d.flush.request, l2.dTlbReqFlush);
    mkConnection(i.flush.request, l2.iTlbReqFlush);

    rule sendFlushDone;
        let x <- l2.flushDone.get;
        d.flush.response.put(?);
        i.flush.response.put(?);
    endrule
endmodule
