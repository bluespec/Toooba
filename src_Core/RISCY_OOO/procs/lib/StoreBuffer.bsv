
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
import Types::*;
import ProcTypes::*;
import Vector::*;
import FIFOF::*;
import FShow::*;
import Assert::*;
import Ehr::*;
import CacheUtils::*;

// store buffer data block byte size == cache line
typedef CLineNumBytes SBBlockNumBytes;
typedef TMul#(8, SBBlockNumBytes) SBBlockSz;
typedef Bit#(SBBlockSz) SBBlock;
typedef Vector#(SBBlockNumBytes, Bool) SBByteEn;

// aligned addr
typedef TSub#(AddrSz, TLog#(SBBlockNumBytes)) SBBlockAddrSz;
typedef Bit#(SBBlockAddrSz) SBBlockAddr;
function SBBlockAddr getSBBlockAddr(Addr a);
    return truncateLSB(a);
endfunction

// SB block vs. normal data
typedef TDiv#(SBBlockSz, DataSz) SBBlockNumData;
typedef Bit#(TLog#(SBBlockNumData)) SBBlockDataSel;
function SBBlockDataSel getSBBlockDataSel(Addr a);
    return truncate(a >> valueOf(TLog#(NumBytes)));
endfunction

// store buffer entry
typedef struct {
    SBBlockAddr addr;
    SBByteEn byteEn;
    SBBlock data;
} SBEntry deriving(Bits, Eq, FShow);

// result of searching (e.g. load byass)
typedef struct {
    Maybe#(SBIndex) matchIdx;
    Maybe#(Data) forwardData; // XXX data is not shifted to match load addr offset
} SBSearchRes deriving(Bits, Eq, FShow);

interface StoreBuffer;
    method Bool isEmpty;
    method Maybe#(SBIndex) getEnqIndex(Addr paddr);
    method Action enq(SBIndex idx, Addr paddr, ByteEn be, Data data);
    method ActionValue#(SBEntry) deq(SBIndex idx);
    method ActionValue#(Tuple2#(SBIndex, SBEntry)) issue;
    method SBSearchRes search(Addr paddr, ByteEn be); // load bypass/stall or atomic inst stall
    // check no matching entry for AMO/Lr/Sc issue
    // XXX assume BE has been shifted approriately for paddr offset
    // (for load we need to do that before calling the methods)
    method Bool noMatchLdQ(Addr paddr, ByteEn be); 
    method Bool noMatchStQ(Addr paddr, ByteEn be); 
endinterface

/////////////////
// EHR version //
/////////////////
// method ordering: (empty CF issue CF search) < deq < enq
// This is mostly derived from SplitLSQ ordering of issueLd, deqSt,
// wakupLdStalledBySB. LSQ issueLd is called with SB search, LSQ deqSt is
// called with SB enq, and wakupLdStalledBy is called with SB deq. Since
// issueLd < (deqSt CF wakeupStalledBySB), we have search < (deq, enq). SB
// issue to memory is unrelated to other rules, so just put it at the
// beginning. Ordering between deq and enq are not important. We put deq < enq
// to cut of bypassing path. empty is used in ROB commit, so EHR port = 0
(* synthesize *)
module mkStoreBufferEhr(StoreBuffer);
    Integer emptyPort = 0;
    Integer issuePort = 0;
    Integer searchPort = 0;
    Integer deqPort = 0; // write
    Integer enqPort = 1; // write

    // entries with valid bit
    Vector#(SBSize, Ehr#(2, SBEntry)) entry <- replicateM(mkEhr(?));
    Vector#(SBSize, Ehr#(2, Bool)) valid <- replicateM(mkEhr(False));
    
    // FIFO of entries to be issued to memory
    FIFOF#(SBIndex) issueQ <- mkUGSizedFIFOF(valueOf(SBSize));
    // FIFO of empty entries
    FIFOF#(SBIndex) freeQ <- mkUGSizedFIFOF(valueOf(SBSize));
    // XXX I use UG FIFOs here, because some methods only call enq/deq in a branch
    // compiler may do somthing wrong with guard if the FIFOs are guarded
    // freeQ never overflow or underflow, so no explicit check is needed
    // issueQ never overflow, so notEmpty check is needed

    // freeQ needs initialization
    Reg#(Bool) inited <- mkReg(False);
    Reg#(SBIndex) initIdx <- mkReg(0);

    rule initFreeQ(!inited);
        freeQ.enq(initIdx);
        initIdx <= initIdx + 1;
        if(initIdx == fromInteger(valueOf(SBSize) - 1)) begin
            inited <= True;
        end
    endrule

    function Maybe#(Bit#(TLog#(n))) searchIndex(function Bool pred(t x), Vector#(n, t) vec);
        case(findIndex(pred, vec)) matches
            tagged Valid .idx: return Valid (pack(idx));
            default: return Invalid;
        endcase
    endfunction

    function Bool noMatch(Addr paddr, ByteEn be);
        // input BE has been shifted, just pack it
        Bit#(NumBytes) ldBE = pack(be);

        // data offset within block
        SBBlockDataSel sel = getSBBlockDataSel(paddr);

        // helper to extract byteEn from entry
        function Bit#(NumBytes) getEntryBE(SBIndex idx);
            Vector#(SBBlockNumData, ByteEn) byteEn = unpack(pack(entry[idx][searchPort].byteEn));
            return pack(byteEn[sel]);
        endfunction
        
        // func to determine whether the load matches a store entry
        function Bool matchEntry(Integer i);
            // entry must be valid, addr should match, byte enable should overlap
            Bool sameAddr = getSBBlockAddr(paddr) == entry[i][searchPort].addr;
            Bool beOverlap = (ldBE & getEntryBE(fromInteger(i))) != 0;
            return valid[i][searchPort] && sameAddr && beOverlap;
        endfunction

        // find match entry and determine return value
        Vector#(SBSize, Integer) idxVec = genVector;
        return !any(matchEntry, idxVec);
    endfunction

    method Bool isEmpty;
        function Bool entryEmpty(Integer i);
            return !valid[i][emptyPort];
        endfunction
        Vector#(SBSize, Integer) idxVec = genVector;
        return all(entryEmpty, idxVec);
    endmethod

    method Maybe#(SBIndex) getEnqIndex(Addr paddr);
        Vector#(SBSize, Integer) idxVec = genVector;
        // first find existing matching entry
        function Bool matchEntry(Integer i);
            return valid[i][enqPort] && entry[i][enqPort].addr == getSBBlockAddr(paddr);
        endfunction
        Maybe#(SBIndex) matchIdx = searchIndex(matchEntry, idxVec);
        if(isValid(matchIdx)) begin
            return matchIdx;
        end
        else begin
            // if no existing entry match, then find an empty entry
            return freeQ.notEmpty ? Valid (freeQ.first) : Invalid;
        end
    endmethod

    method Action enq(SBIndex idx, Addr paddr, ByteEn be, Data d) if(inited);
        // get data offset
        SBBlockDataSel sel = getSBBlockDataSel(paddr);
        // check whether the entry already exists
        if(valid[idx][enqPort]) begin
            // existing entry: merge
            doAssert(getSBBlockAddr(paddr) == entry[idx][enqPort].addr, "SB enq to existing entry addr should match");
            // update data
            Vector#(SBBlockNumData, Data) block = unpack(entry[idx][enqPort].data);
            Vector#(NumBytes, Bit#(8)) origData = unpack(block[sel]);
            Vector#(NumBytes, Bit#(8)) wrData = unpack(d);
            function Bit#(8) getNewByte(Integer i) = be[i] ? wrData[i] : origData[i];
            Vector#(NumBytes, Bit#(8)) newData = map(getNewByte, genVector);
            block[sel] = pack(newData);
            // update byte enable
            Vector#(SBBlockNumData, ByteEn) byteEn = unpack(pack(entry[idx][enqPort].byteEn));
            byteEn[sel] = unpack(pack(byteEn[sel]) | pack(be));
            // update entry
            entry[idx][enqPort] <= SBEntry {
                addr: getSBBlockAddr(paddr),
                byteEn: unpack(pack(byteEn)),
                data: pack(block)
            };
            // this entry must have been sent to issueQ
        end
        else begin
            // new entry: set valid
            valid[idx][enqPort] <= True;
            // setup entry
            Vector#(SBBlockNumData, Data) block = ?;
            block[sel] = d;
            Vector#(SBBlockNumData, ByteEn) byteEn = replicate(replicate(False));
            byteEn[sel] = be;
            entry[idx][enqPort] <= SBEntry {
                addr: getSBBlockAddr(paddr),
                byteEn: unpack(pack(byteEn)),
                data: pack(block)
            };
            // send this entry to issueQ
            doAssert(issueQ.notFull, "SB issueQ should not be full");
            issueQ.enq(idx);
            // remove from freeQ
            doAssert(freeQ.notEmpty, "SB freeQ should not be empty");
            freeQ.deq;
        end
    endmethod

    method ActionValue#(SBEntry) deq(SBIndex idx) if(inited);
        doAssert(valid[idx][deqPort], "SB deq entry must be valid");
        valid[idx][deqPort] <= False; // set entry to invalid
        freeQ.enq(idx); // recycle entry
        return entry[idx][deqPort];
    endmethod

    method ActionValue#(Tuple2#(SBIndex, SBEntry)) issue if(issueQ.notEmpty);
        issueQ.deq;
        SBIndex idx = issueQ.first;
        return tuple2(idx, entry[idx][issuePort]);
    endmethod

    method SBSearchRes search(Addr paddr, ByteEn be);
        // input BE has been shifted, just pack it
        Bit#(NumBytes) ldBE = pack(be);

        // data offset within block
        SBBlockDataSel sel = getSBBlockDataSel(paddr);

        // helper to extract byteEn from entry
        function Bit#(NumBytes) getEntryBE(SBIndex idx);
            Vector#(SBBlockNumData, ByteEn) byteEn = unpack(pack(entry[idx][searchPort].byteEn));
            return pack(byteEn[sel]);
        endfunction
        
        // func to determine whether the load matches a store entry
        function Bool matchEntry(Integer i);
            // entry must be valid, addr should match, byte enable should overlap
            Bool sameAddr = getSBBlockAddr(paddr) == entry[i][searchPort].addr;
            Bool beOverlap = (ldBE & getEntryBE(fromInteger(i))) != 0;
            return valid[i][searchPort] && sameAddr && beOverlap;
        endfunction

        // find match entry and determine return value
        Vector#(SBSize, Integer) idxVec = genVector;
        if(searchIndex(matchEntry, idxVec) matches tagged Valid .idx) begin
            // check whether bytes reading are all covered by the entry
            if((getEntryBE(idx) & ldBE) == ldBE) begin
                // fully covered, forward data
                Vector#(SBBlockNumData, Data) block = unpack(entry[idx][searchPort].data);
                return SBSearchRes {
                    matchIdx: Valid (idx),
                    forwardData: Valid (block[sel])
                };
            end
            else begin
                // interact byte, not fully covered, stall the load
                return SBSearchRes {
                    matchIdx: Valid (idx),
                    forwardData: Invalid
                };
            end
        end
        else begin
            // load should go to memory
            return SBSearchRes {
                matchIdx: Invalid,
                forwardData: Invalid
            };
        end
    endmethod

    method noMatchLdQ = noMatch;
    method noMatchStQ = noMatch;
endmodule

(* synthesize *)
module mkDummyStoreBuffer(StoreBuffer);
    method Bool isEmpty = True;
    method Maybe#(SBIndex) getEnqIndex(Addr paddr) = Invalid;
    method Action enq(SBIndex idx, Addr paddr, ByteEn be, Data data);
        doAssert(False, "enq should never be called)");
    endmethod
    method ActionValue#(SBEntry) deq(SBIndex idx);
        doAssert(False, "deq should never be called)");
        return ?;
    endmethod
    method ActionValue#(Tuple2#(SBIndex, SBEntry)) issue;
        doAssert(False, "issue should never be called)");
        return ?;
    endmethod
    method SBSearchRes search(Addr paddr, ByteEn be);
        return SBSearchRes {matchIdx: Invalid, forwardData: Invalid};
    endmethod
    method Bool noMatchLdQ(Addr paddr, ByteEn be) = True; 
    method Bool noMatchStQ(Addr paddr, ByteEn be) = True; 
endmodule
