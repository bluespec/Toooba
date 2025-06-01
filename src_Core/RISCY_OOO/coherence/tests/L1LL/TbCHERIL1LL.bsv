
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
import MemoryTypes::*;
import Amo::*;
import CacheUtils::*;

import CCTypes::*;
import FShow::*;
import Randomizable::*;
import Vector::*;
import FIFO::*;
import RegFile::*;
import Connectable::*;
import GetPut::*;
import ClientServer::*;
//import AtomicMem::*;
import DelayMemTypes::*;
import IdealDelayMem::*;
import Printf::*;
import ConfigReg::*;
import L1LLSizes::*;
import L1LL::*;

// FIXME assume no banking

// number of reqs (tests) per core
typedef 1 TestNum;
typedef Bit#(TLog#(TestNum)) TestId;
typedef Bit#(TLog#(TAdd#(TestNum, 1))) TestCnt;

typedef TDiv#(TestNum, 1) TestPrintNum;

// number of DMA reqs
typedef 1 DmaTestNum; //TMul#(TestNum, L1Num) DmaTestNum; // To reduce sc failure
typedef Bit#(TLog#(DmaTestNum)) DmaTestId;
typedef Bit#(TLog#(TAdd#(DmaTestNum, 1))) DmaTestCnt;
typedef TDiv#(DmaTestNum, 1) DmaTestPrintNum;

// memory delay
typedef 30 MemDelay;

// time out
typedef 10000 MaxTimeOut;
typedef Bit#(TLog#(MaxTimeOut)) TimeOutCnt;

// test index, tag, data offset choices
typedef TMul#(4, LLWayNum) TagNum;
typedef 2 IndexNum;

typedef LLTag LLCTag;
typedef LLIndex LLCIndex;

function Addr getAddr(LLCTag tag, LLCIndex index, LineDataOffset sel);
    DataBytesOffset off = 0;
    return {tag, index, sel, off};
endfunction
/*
function Addr getInstAddr(LLCTag tag, LLCIndex index, LineInstOffset sel);
    Bit#(TLog#(TDiv#(InstSz, 8))) off = 0;
    return {tag, index, sel, off};
endfunction
*/
// memory size for testing
typedef TAdd#(TSub#(AddrSz, SizeOf#(LLCTag)), TLog#(TagNum)) LgTestMemSzBytes;

// req/resp to/from memory system
typedef enum {
    Ld, St, Lr, Sc, Amo
`ifdef STORE_PREFETCH
    , StPrefetch
`endif
} MemTestOp deriving(Bits, Eq, FShow, Bounded);

function MemOp getMemOp(MemTestOp op);
    case(op)
        Ld: return Ld;
        St: return St;
        Lr: return Lr;
        Sc: return Sc;
        Amo: return Amo;
`ifdef STORE_PREFETCH
        StPrefetch: return StPrefetch;
`endif
        default: return ?;
    endcase
endfunction

function Msi getToState(MemTestOp op);
    case(op)
        Ld: return S;
        Lr: return E;
        St, Sc, Amo: return M;
`ifdef STORE_PREFETCH
        StPrefetch: return E;
`endif
        default: return ?;
    endcase
endfunction

typedef struct {
    TestId id;
    MemTestOp op;
    Addr addr;
    MemDataByteEn byteEn; // for Sc
    MemTaggedData data; // for Sc/Amo
    LineByteEn lineBE; // for St
    Line line; // for St
    AmoInst amoInst; // for Amo
} MemTestReq deriving(Bits, Eq, FShow);

typedef enum {
    Ld, St, LrScAmo
`ifdef STORE_PREFETCH
    , StPrefetch
`endif
} MemRespType deriving(Bits, Eq, FShow);
typedef struct {
    MemRespType t;
    TestId id;
    MemTaggedData data;
} MemTestResp deriving(Bits, Eq, FShow);

function MemRespType getMemRespType(MemTestOp op);
    case(op)
        Ld: return Ld;
        St: return St;
        Lr, Sc, Amo: return LrScAmo;
`ifdef STORE_PREFETCH
        StPrefetch: return StPrefetch;
`endif
        default: return ?;
    endcase
endfunction

// random req stall
typedef Bit#(2) ReqStall;

function Bool getReqStall(ReqStall x);
`ifdef NO_REQ_STALL
    return False;
`else
    return x == 0;
`endif
endfunction

function Bool getDmaReqStall(ReqStall x);
`ifdef NO_DMA_REQ_STALL
    return False;
`else
    return x == 0;
`endif
endfunction

// update cache line
// test FSM
typedef enum {InitTable, InitAddr, Idle, Process, Done} TestFSM deriving(Bits, Eq, FShow);

(* synthesize *)
module mkTbCHERIL1LL(Empty);
    // Reference
    //AtomicMem#(LgTestMemSzBytes) refMem <- mkAtomicMem;
    Reg#(Vector#(L1DNum, Vector#(L1BankNum, Maybe#(LineAddr)))) refLink <- mkReg(replicate(replicate(Invalid)));
    // record req
    // D$
    Vector#(L1DNum, RegFile#(TestId, Maybe#(MemTestReq))) dcReqTable <- replicateM(mkRegFileFull);
    Vector#(L1DNum, Randomize#(ReqStall)) randDCReqStall <- replicateM(mkGenericRandomizer);
    Vector#(L1DNum, Randomize#(MemTestOp)) randDCOp <- replicateM(mkConstrainedRandomizer(minBound, maxBound));
    Vector#(L1DNum, Randomize#(LLCTag)) randDCTag <- replicateM(mkConstrainedRandomizer(0, fromInteger(valueOf(TagNum) - 1)));
    Vector#(L1DNum, Randomize#(LLCIndex)) randDCIndex <- replicateM(mkConstrainedRandomizer(0, fromInteger(valueOf(IndexNum) - 1)));
    Vector#(L1DNum, Randomize#(LineDataOffset)) randDCDataSel <- replicateM(mkGenericRandomizer);
    Vector#(L1DNum, Randomize#(Bit#(MemDataSzBytes))) randDCDataBE <- replicateM(mkConstrainedRandomizer(1, maxBound)); // it could be all 0 though..
    Vector#(L1DNum, Randomize#(MemTaggedData)) randDCData <- replicateM(mkGenericRandomizer);
    Vector#(L1DNum, Randomize#(Bit#(LineSzBytes))) randDCLineBE <- replicateM(mkConstrainedRandomizer(1, maxBound)); // it could be all 0 though..
    Vector#(L1DNum, Randomize#(Line)) randDCLine <- replicateM(mkGenericRandomizer);

    Vector#(L1DNum, Randomize#(AmoFunc)) randDCAmoFunc <- replicateM(mkConstrainedRandomizer(Swap, Maxu));
    Vector#(L1DNum, Randomize#(Bool)) randDCDoubleWord <- replicateM(mkGenericRandomizer);
    Vector#(L1DNum, Reg#(TestCnt)) sendDCCnt <- replicateM(mkReg(0));
    Vector#(L1DNum, RWire#(MemTestReq)) sendDCReq <- replicateM(mkRWire);
    // randomize req
    // D$

    Vector#(L1DNum, Reg#(File)) dcReqLog <- replicateM(mkReg(InvalidFile));
    Vector#(L1DNum, Reg#(File)) dcRespLog <- replicateM(mkReg(InvalidFile));

    Reg#(TestFSM) testFSM <- mkConfigReg(InitTable);
    Reg#(Bool) coreTableInitDone <- mkReg(False);
    Reg#(TestId) iterId <- mkReg(0);
    Vector#(L1DNum, RegFile#(TestId, Bool)) dcRespDoneTable <- replicateM(mkRegFileFull);
    Vector#(L1DNum, RWire#(MemTestResp)) recvDCResp <- replicateM(mkRWire);

    Reg#(LLCTag) iterTag <- mkReg(0);
    Reg#(LLCIndex) iterIndex <- mkReg(0);

    function L1ProcResp#(ProcRqId) getL1ProcResp(Integer i);
        return (interface L1ProcResp;
            method Action respLd(ProcRqId id, MemTaggedData d);
                recvDCResp[i].wset(MemTestResp {t: Ld, id: truncate(id), data: d});
            endmethod
            method Action respLrScAmo(ProcRqId id, MemTaggedData d);
                recvDCResp[i].wset(MemTestResp {t: LrScAmo, id: truncate(id), data: d});
            endmethod
            method ActionValue#(Tuple2#(LineByteEn, Line)) respSt(ProcRqId id);
                recvDCResp[i].wset(MemTestResp {t: St, id: truncate(id), data: ?});
                let req = validValue(dcReqTable[i].sub(truncate(id)));
                return tuple2(req.lineBE, req.line);
            endmethod
`ifdef DEBUG_STORE_PREFETCH
            method Action respStPrefetch(ProcRqId id);
                recvDCResp[i].wset(MemTestResp {t: StPrefetch, id: truncate(id), data: ?});
            endmethod
`endif
            method Action evict(LineAddr a);
                noAction;
            endmethod
        endinterface);
    endfunction
    IdealDelayMem#(MemDelay, LgTestMemSzBytes, LdMemRqId#(LLCRqMshrIdx), void) delayMem <- mkIdealDelayMem;
    let memSys <- mkL1LL(map(getL1ProcResp, genVector));

    mkConnection(memSys.to_mem, delayMem.to_proc);
    DelayMemTest dutMem = delayMem.to_test;

    rule doInitCoreTable(testFSM == InitTable && !coreTableInitDone);
        for(Integer i = 0; i < valueOf(L1DNum); i = i+1) begin
            dcReqTable[i].upd(iterId, Invalid);
            dcRespDoneTable[i].upd(iterId, False);
        end
        // change state
        if(iterId == fromInteger(valueof(TestNum) - 1)) begin
            iterId <= 0;
            coreTableInitDone <= True;
        end
        else begin
            iterId <= iterId + 1;
        end
    endrule
    rule doInitTableDone(testFSM == InitTable && coreTableInitDone);
        testFSM <= InitAddr;
        $fdisplay(stderr, "INFO: init table done");
    endrule

    rule doInitAddr(testFSM == InitAddr);
        Addr addr = getAddr(iterTag, iterIndex, 0);
        
        Line initV = unpack(0); //replicate(replicate(addr));
        dutMem.initLine(addr, initV);
        //refMem.writeLine(addr, initV);
        if(iterIndex == fromInteger(valueOf(IndexNum) - 1)) begin
            iterIndex <= 0;
            if(iterTag == fromInteger(valueOf(TagNum) - 1)) begin
                iterTag <= 0;
                // init randomizers and files for each core
                for(Integer i = 0; i < valueOf(L1DNum); i = i+1) begin
                    randDCReqStall[i].cntrl.init;
                    randDCOp[i].cntrl.init;
                    randDCTag[i].cntrl.init;
                    randDCIndex[i].cntrl.init;
                    randDCDataSel[i].cntrl.init;
                    randDCDataBE[i].cntrl.init;
                    randDCData[i].cntrl.init;
                    randDCLineBE[i].cntrl.init;
                    randDCLine[i].cntrl.init;
                    randDCAmoFunc[i].cntrl.init;
                    randDCDoubleWord[i].cntrl.init;
                    String name = sprintf("req_dc_%d.log", i);
                    File f <- $fopen(name, "w");
                    dcReqLog[i] <= f;
                    name = sprintf("resp_dc_%d.log", i);
                    f <- $fopen(name, "w");
                    dcRespLog[i] <= f;
                end

                // init randomizers and files for DMA

                // notify memory that init done
                dutMem.initDone;
                // change state
                testFSM <= Idle;
                $fdisplay(stderr, "INFO: init addr done");
            end
            else begin
                iterTag <= iterTag + 1;
            end
        end
        else begin
            iterIndex <= iterIndex + 1;
        end
    endrule
    Reg#(Bit#(64)) waitCount <- mkReg(0);

    rule simplyWait(testFSM == Idle);
        // wait for LLC to init all BRAMs
        waitCount <= waitCount + 1;
        if(waitCount == fromInteger(valueOf(TExp#(LLIndexSz)))) begin
            $display("%t %m Start Issuing Requests only now!!!!", $time);
            $fdisplay(stderr, "INFO: start issue req");
            testFSM <= Process;
        end
    endrule
    
    Vector#(L1DNum, L1ProcReq#(ProcRqId)) ifcDC = memSys.dReq;
    Vector#(L1DNum, Reg#(TestCnt)) sendPrintDCCnt <- replicateM(mkReg(fromInteger(valueOf(TestPrintNum))));

    for(Integer i = 0; i < valueOf(L1DNum); i = i+1) begin
        rule doDCReq(testFSM == Process && sendDCCnt[i] < fromInteger(valueOf(TestNum)));
            // randomize req
            let index <- randDCIndex[i].next;
            let tag <- randDCTag[i].next;
            let sel <- randDCDataSel[i].next;
            let addr = getAddr(tag, index, sel);
            let op <- randDCOp[i].next;
            let data <- randDCData[i].next;
            let rBE <- randDCDataBE[i].next;
            MemDataByteEn be = unpack(rBE);
            let line <- randDCLine[i].next;
            let rlbe <- randDCLineBE[i].next;
            LineByteEn lineBE = unpack(rlbe);
            let doubleWord <- randDCDoubleWord[i].next;
            let amoFunc <- randDCAmoFunc[i].next;
            let req = MemTestReq {
                id: truncate(sendDCCnt[i]),
                op: St, //op,
                addr: 64'h10000, //addr,
                byteEn: be,
                data: data,
                lineBE: replicate(replicate(True)),// lineBE,
                line: line, //line,
                amoInst: AmoInst {
                    func: amoFunc,
                    //doubleWord: doubleWord,
                    aq: False,
                    rl: False
                }
            };
            // randomize stall
            let rStall <- randDCReqStall[i].next;
            if(!getReqStall(rStall)) begin
                // no stall, send req & record
                ifcDC[i].req(ProcRq {
                    id: zeroExtend(req.id),
                    addr: req.addr,
                    toState: getToState(req.op),
                    op: getMemOp(req.op),
                    byteEn: req.byteEn,
                    data: req.data,
                    amoInst: req.amoInst
                });
                testFSM <= Done;
                sendDCReq[i].wset(req);
                
                // output req cnt
                if((sendDCCnt[i] + 1) == sendPrintDCCnt[i]) begin
                    $fdisplay(stderr, "INFO: %t D$ %d send req %d/%d",
                        $time, i, sendDCCnt[i] + 1, valueOf(TestNum)
                    );
                    sendPrintDCCnt[i] <= sendPrintDCCnt[i] + fromInteger(valueOf(TestPrintNum));
                end
            end
        endrule
    end
endmodule 