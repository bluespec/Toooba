
// Copyright (c) 2018 Massachusetts Institute of Technology
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
import ConfigReg::*;
import ProcTypes::*;
import MMIOAddrs::*;
import CacheUtils::*;
import Fifo::*;
import Amo::*;
import MMIOInst::*;

// local MMIO logic in each core (MMIOCore)
// Every MMIO req from the core is directly passed to the platform, while this
// logic handles req from the platform when platform is processing core req or
// posting timer interrupt.

interface MMIOCoreToPlatform;
    interface FifoDeq#(MMIOCRq) cRq;
    interface FifoEnq#(MMIOPRs) pRs;
    interface FifoEnq#(MMIOPRq) pRq;
    interface FifoDeq#(MMIOCRs) cRs;
    // core keeps a copy of the mtime reg. This method allows the platform to
    // inform the core to change time CSR.
    method Action setTime(Data t);
endinterface

interface MMIOCore;
    // is the PHYSICAL mem access addr an MMIO addr
    method Bool isMMIOAddr(Addr addr);
    // for mem exe pipeline to send/recv MMIO req/resp
    method Action dataReq(MMIOCRq r);
    method MMIODataPRs dataRespVal;
    method Action dataRespDeq;
    // set tohost & fromhost addr
    method Action setHtifAddrs(Addr toHost, Addr fromHost);
    // signal that we have a pending pRq, so some parts of the core (e.g.,
    // rename/issue) may yield themselves.
    method Bool hasPendingPRq;
    // methods to platform
    interface MMIOCoreToPlatform toP;
endinterface

interface MMIOCoreInput;
    // ifc from inst fetch
    interface MMIOInstToCore fetch;
    // MMIOCore needs to access MSIP and MTIP CSRs
    method Bit#(1) getMSIP;
    method Action setMSIP(Bit#(1) v);
    method Action setMTIP(Bit#(1) v);
    // guard for accessing MSIP or MTIP
    method Bool noInflightCSRInstOrInterrupt;
    // MMIOCore needs to pass mtime to CSRF
    method Action setTime(Data t);
endinterface

module mkMMIOCore#(MMIOCoreInput inIfc)(MMIOCore);
    // HTIF mem mapped addrs
    Reg#(DataAlignedAddr) toHostAddr <- mkConfigReg(0);
    Reg#(DataAlignedAddr) fromHostAddr <- mkConfigReg(0);

    // FIFOs connected to memory pipeline
    Fifo#(1, MMIOCRq) dataReqQ <- mkCFFifo;
    Fifo#(1, MMIODataPRs) dataRespQ <- mkCFFifo;
    // we limit to at most 1 data MMIO req by mem pipeline, so that this req
    // will not clog requests from other cores
    Fifo#(1, void) dataPendQ <- mkCFFifo;

    // FIFOs connected to platform
    Fifo#(1, MMIOCRq) cRqQ <- mkCFFifo;
    Fifo#(1, MMIOPRs) pRsQ <- mkCFFifo;
    Fifo#(1, MMIOPRq) pRqQ <- mkCFFifo;
    Fifo#(1, MMIOCRs) cRsQ <- mkCFFifo;

    // PRq from platform will access MSIP or MTIP. As explained in CSRF,
    // accessing these bits may break the atomicity of CSRXXX inst or interrupt
    // handling, so we need to check for them. Note that exception handling
    // does not touch MSIP/MTIP. Besides, a inst with exception may be after
    // the MMIO inst that accesses its own MSIP. Waiting for the clear of
    // exception may cause deadlock.
    rule handlePRq(inIfc.noInflightCSRInstOrInterrupt);
        pRqQ.deq;
        MMIOPRq req = pRqQ.first;
        MMIOCRs resp = MMIOCRs {data: ?};
        case(req.target)
            MSIP: begin
                if(req.func == St) begin
                    inIfc.setMSIP(req.data[0]);
                end
                else begin
                    // both AMO and Ld need the original data as resp
                    Bit#(1) msip = inIfc.getMSIP;
                    resp.data = msip;
                    // AMO also needs to write
                    if(req.func matches tagged Amo .amoFunc) begin
                        let amoInst = AmoInst {
                            func: amoFunc,
                            doubleWord: False,
                            aq: False,
                            rl: False
                        };
                        let newData = amoExec(amoInst, zeroExtend(msip),
                                              zeroExtend(req.data), False);
                        inIfc.setMSIP(newData[0]);
                    end
                end
            end
            MTIP: begin
                if(req.func == St) begin
                    inIfc.setMTIP(req.data[0]);
                end
                else begin
                    doAssert(False, "platform can only write MTIP");
                end
            end
            default: doAssert(False, "unknown target");
        endcase
        // resp to platform
        cRsQ.enq(resp);
    endrule

    // arbitrate requests from fetch stage and mem stage; prioritize mem stage
    rule sendDataReq;
        dataReqQ.deq;
        cRqQ.enq(dataReqQ.first);
    endrule

    (* descending_urgency = "sendDataReq, sendInstReq" *)
    rule sendInstReq;
        inIfc.fetch.instReq.deq;
        let {addr, maxWay} = inIfc.fetch.instReq.first;
        cRqQ.enq(MMIOCRq {
            addr: addr,
            func: Inst (maxWay),
            byteEn: ?,
            data: ?
        });
    endrule

    // dispatch resp
    rule sendDataResp(pRsQ.first matches tagged DataAccess .r);
        pRsQ.deq;
        dataRespQ.enq(r);
    endrule

    rule sendInstResp(pRsQ.first matches tagged InstFetch .r);
        pRsQ.deq;
        inIfc.fetch.instResp.enq(r);
    endrule

    method Bool isMMIOAddr(Addr addr);
        let a = getDataAlignedAddr(addr);
        return a < mainMemBaseAddr || a == toHostAddr || a == fromHostAddr;
    endmethod

    method Action dataReq(MMIOCRq r);
        dataReqQ.enq(r);
        dataPendQ.enq(?);
    endmethod

    method MMIODataPRs dataRespVal if(dataPendQ.notEmpty);
        return dataRespQ.first;
    endmethod
    
    method Action dataRespDeq;
        dataPendQ.deq;
        dataRespQ.deq;
    endmethod

    method Action setHtifAddrs(Addr toHost, Addr fromHost);
        toHostAddr <= getDataAlignedAddr(toHost);
        fromHostAddr <= getDataAlignedAddr(fromHost);
        inIfc.fetch.setHtifAddrs(toHost, fromHost);
    endmethod

    method Bool hasPendingPRq;
        return pRqQ.notEmpty;
    endmethod

    interface MMIOCoreToPlatform toP;
        interface cRq = toFifoDeq(cRqQ);
        interface pRs = toFifoEnq(pRsQ);
        interface pRq = toFifoEnq(pRqQ);
        interface cRs = toFifoDeq(cRsQ);
        method setTime = inIfc.setTime;
    endinterface
endmodule

