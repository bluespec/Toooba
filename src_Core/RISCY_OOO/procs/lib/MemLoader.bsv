
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

// mem loader: responsible for initializing memory

import Clocks::*;
import Assert::*;
import Connectable::*;
import FShow::*;
import FIFO::*;
import Vector::*;
import Fifo::*;
import Types::*;
import ProcTypes::*;
import CCTypes::*;
import CacheUtils::*;
import MMIOAddrs::*;
import MemLoaderIF::*;
import SyncFifo::*;

// messages from host
typedef struct {
    Bool valid;
    Addr addr;
} HostWrAddr deriving(Bits, Eq, FShow);

typedef struct {
    Data data;
    Bit#(DataSzBytes) byteEn;
    Bool last;
} HostWrData deriving(Bits, Eq, FShow);

interface MemLoaderIndInv;
    method ActionValue#(Addr) start;
    method Action wrDone;
endinterface

instance Connectable#(MemLoaderIndInv, MemLoaderIndication);
    module mkConnection#(MemLoaderIndInv inv, MemLoaderIndication ind)(Empty);
        rule doStart;
            let addr <- inv.start;
            ind.start(addr);
        endrule
        rule doWrDone;
            inv.wrDone;
            ind.wrDone;
        endrule
    endmodule
endinstance

// messages to LLC
typedef void MemLoaderMemReqId;
typedef DmaRq#(MemLoaderMemReqId) MemLoaderMemReq;

interface MemLoaderMemClient;
    interface FifoDeq#(MemLoaderMemReq) memReq;
    interface FifoEnq#(MemLoaderMemReqId) respSt;
endinterface

// MMIO
interface MemLoaderMMIO;
    method ActionValue#(MMIODataPRs) req(MemLoaderAlignedOffset offset,
                                         ByteEn wrBE, Data wrData);
endinterface

interface MemLoader;
    // MMIO ifc
    interface MemLoaderMMIO mmio;
    // ifc to LLC
    interface MemLoaderMemClient to_mem;
    // request & indication inverse, under portal clock domain
    interface MemLoaderRequest hostReq;
    interface MemLoaderIndInv hostIndInv;
endinterface

// this module should be clocked under user domain
(* synthesize *)
module mkMemLoader#(Clock portalClk, Reset portalRst)(MemLoader);
    Bool verbose = True;

    // MMIO regs
    Reg#(Addr) memStartAddr <- mkReg(0);
    Reg#(Bool) busy <- mkReg(False);

    // states to handle writes from host
    // whether we are working on a series of writes
    Reg#(Bool) writing <- mkReg(False);
    // counter of pending (not acked) stores to LLC
    Reg#(Bit#(8)) pendStCnt <- mkReg(0);
    // whether we should expect more write data from host for this series
    Reg#(Bool) expectWrData <- mkReg(False);
    // form req to LLC
    Reg#(LineDataOffset) reqSel <- mkRegU;
    Reg#(LineAddr) reqAddr <- mkRegU;
    Reg#(Line) reqData <- mkRegU;
    Reg#(Vector#(LineSzData, Bit#(DataSzBytes))) reqBE <- mkRegU;

    // sync FIFOs to cross to portal clk
    Clock userClk <- exposeCurrentClock;
    Reset userRst <- exposeCurrentReset;
    SyncFIFOIfc#(HostWrAddr) hostWrAddrQ <- mkSyncFifo(1, portalClk, portalRst, userClk, userRst);
    SyncFIFOIfc#(HostWrData) hostWrDataQ <- mkSyncFifo(1, portalClk, portalRst, userClk, userRst);
    SyncFIFOIfc#(Addr) hostStartQ <- mkSyncFifo(1, userClk, userRst, portalClk, portalRst);
    SyncFIFOIfc#(void) hostWrDoneQ <- mkSyncFifo(1, userClk, userRst, portalClk, portalRst);

    // FIFOs to LLC
    Fifo#(1, MemLoaderMemReq) memReqQ <- mkCFFifo;
    Fifo#(1, MemLoaderMemReqId) respStQ <- mkCFFifo;

    rule doNewWrite(busy && !writing);
        hostWrAddrQ.deq;
        HostWrAddr req = hostWrAddrQ.first;
        if(req.valid) begin
            // new write series, setup regs
            writing <= True;
            pendStCnt <= 0;
            expectWrData <= True;
            reqSel <= getLineDataOffset(req.addr);
            reqAddr <= getLineAddr(req.addr);
            reqBE <= replicate(0);
            // check addr align to Data
            Bit#(LgDataSzBytes) offset = truncate(req.addr);
            doAssert(offset == 0, "write addr not aligned to Data");
        end
        else begin
            // all writes done, reset busy
            busy <= False;
        end
        if(verbose) begin
            $display("[MemLoader doNewWrite] ", fshow(req));
        end
    endrule

    // check counter overflow in guard
    rule doStReq(writing && expectWrData && pendStCnt != maxBound);
        // get write data from host
        hostWrDataQ.deq;
        HostWrData wr = hostWrDataQ.first;
        // merge with req data & BE
        Line newData = reqData;
        Vector#(LineSzData, Bit#(DataSzBytes)) newBE = reqBE;
        newData[reqSel] = wr.data;
        newBE[reqSel] = wr.byteEn;
        // common state update
        expectWrData <= !wr.last;
        reqData <= newData;
        reqSel <= reqSel + 1;
        reqAddr <= reqSel == maxBound ? reqAddr + 1 : reqAddr;
        // print
        if(verbose) begin
            $display("[MemLoader doStReq] ", fshow(wr),
                     " ; reqData ", fshow(reqData), " ; reqBE " ,fshow(reqBE),
                     " ; reqSel %d ; reqAddr %x", reqSel, reqAddr);
        end
        // we need to req LLC when: (1) fill up a line OR (2) last host busrt
        if(reqSel == maxBound || wr.last) begin
            MemLoaderMemReq req = DmaRq {
                addr: {reqAddr, 0},
                byteEn: unpack(pack(newBE)),
                data: newData,
                id: ?
            };
            if(verbose) begin
                $display("[MemLoader doStReq] req to LLC ", fshow(req));
            end
            // reset BE for next fresh LLC req
            reqBE <= replicate(0);
            // only send real write to LLC, otherwise may spawn orphan read resp
            if(req.byteEn != replicate(False)) begin
                memReqQ.enq(req);
                pendStCnt <= pendStCnt + 1;
            end
            else begin
                doAssert(False, "write req cannot have zero BE");
            end
        end
        else begin
            reqBE <= newBE;
        end
    endrule

    rule doStResp(writing);
        if(verbose) begin
            $display("[MemLoader doStResp] pend st cnt %d, expect wr data %d",
                     pendStCnt, expectWrData);
        end
        // get st resp
        respStQ.deq;
        doAssert(pendStCnt > 0, "pend st cnt underflow");
        let pendStCntNext = pendStCnt - 1;
        pendStCnt <= pendStCntNext;
        if(pendStCntNext == 0 && !expectWrData) begin
            // we have finished this write series
            hostWrDoneQ.enq(?);
            writing <= False;
        end
    endrule

    // We don't resolve rule conflicts here because this module does not need
    // to be fast.

    interface MemLoaderMMIO mmio;
        method ActionValue#(MMIODataPRs) req(MemLoaderAlignedOffset offset,
                                             ByteEn wrBE, Data wrData);
            if(wrBE == replicate(False)) begin
                return MMIODataPRs {
                    valid: True,
                    data: offset == 0 ? memStartAddr : zeroExtend(pack(busy))
                };
            end
            else begin
                if(offset == 0) begin
                    // only write mem start addr when not busy
                    if(!busy) begin
                        Vector#(DataSzBytes, Bit#(8)) wrVec = unpack(wrData);
                        Vector#(DataSzBytes, Bit#(8)) newVec = unpack(memStartAddr);
                        for(Integer i = 0; i < valueof(DataSzBytes); i = i+1) begin
                            if(wrBE[i]) begin
                                newVec[i] = wrVec[i];
                            end
                        end
                        memStartAddr <= pack(newVec);
                        // become busy and ask host to start transmitting data
                        busy <= True;
                        hostStartQ.enq(pack(newVec));
                    end
                    return MMIODataPRs {
                        valid: True,
                        data: ?
                    };
                end
                else begin
                    return MMIODataPRs {
                        valid: False,
                        data: ?
                    };
                end
            end
        endmethod
    endinterface

    interface MemLoaderMemClient to_mem;
        interface memReq = toFifoDeq(memReqQ);
        interface respSt = toFifoEnq(respStQ);
    endinterface

    interface MemLoaderRequest hostReq;
        method Action wrAddr(Bool valid, Addr addr);
            hostWrAddrQ.enq(HostWrAddr {
                valid: valid,
                addr: addr
            });
        endmethod
        method Action wrData(Data data, Bit#(DataSzBytes) byteEn, Bool last);
            hostWrDataQ.enq(HostWrData {
                data: data,
                byteEn: byteEn,
                last: last
            });
        endmethod
    endinterface

    interface MemLoaderIndInv hostIndInv;
        method ActionValue#(Addr) start;
            hostStartQ.deq;
            return hostStartQ.first;
        endmethod
        method Action wrDone;
            hostWrDoneQ.deq;
        endmethod
    endinterface
endmodule
