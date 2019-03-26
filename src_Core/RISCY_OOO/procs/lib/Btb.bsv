
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
import ConfigReg::*;
import RegFile::*;
import Vector::*;

export NextAddrPred(..);
export mkBtb;

interface NextAddrPred;
    method Addr predPc(Addr pc);
    method Action update(Addr pc, Addr nextPc, Bool taken);
    // security
    method Action flush;
    method Bool flush_done;
endinterface

// Local BTB Typedefs
typedef 256 BtbEntries; // 4KB BTB
typedef Bit#(TLog#(BtbEntries)) BtbIndex;
typedef Bit#(TSub#(TSub#(AddrSz, TLog#(BtbEntries)), 2)) BtbTag;

typedef struct {
    Addr pc;
    Addr nextPc;
    Bool taken;
} BtbUpdate deriving(Bits, Eq, FShow);

// No synthesize boundary because we need to call predPC several times
module mkBtb(NextAddrPred);
    // Read and Write ordering doesn't matter since this is a predictor
    // mkRegFileWCF is the RegFile version of mkConfigReg
    RegFile#(BtbIndex, Addr) next_addrs <- mkRegFileWCF(0,fromInteger(valueOf(BtbEntries)-1));
    RegFile#(BtbIndex, BtbTag) tags <- mkRegFileWCF(0,fromInteger(valueOf(BtbEntries)-1));
    Vector#(BtbEntries, Reg#(Bool)) valid <- replicateM(mkConfigReg(False));

    RWire#(BtbUpdate) updateEn <- mkRWire;

`ifdef SECURITY
    Reg#(Bool) flushDone <- mkReg(True);
`else
    Bool flushDone = True;
`endif

    function BtbIndex getIndex(Addr pc) = truncate(pc >> 2);
    function BtbTag getTag(Addr pc) = truncateLSB(pc);

    // no flush, accept update
    (* fire_when_enabled, no_implicit_conditions *)
    rule canonUpdate(flushDone &&& updateEn.wget matches tagged Valid .upd);
        let pc = upd.pc;
        let nextPc = upd.nextPc;
        let taken = upd.taken;

        let index = getIndex(pc);
        let tag = getTag(pc);
        if(taken) begin
            valid[index] <= True;
            tags.upd(index, tag);
            next_addrs.upd(index, nextPc);
        end else if( tags.sub(index) == tag ) begin
            // current instruction has target in btb, so clear it
            valid[index] <= False;
        end
    endrule

`ifdef SECURITY
    // flush, clear everything (and drop update)
    rule doFlush(!flushDone);
        writeVReg(valid, replicate(False));
        flushDone <= True;
    endrule
`endif

    method Addr predPc(Addr pc);
        BtbIndex index = getIndex(pc);
        BtbTag tag = getTag(pc);
        if(valid[index] && tag == tags.sub(index))
            return next_addrs.sub(index);
        else
            return (pc + 4);
    endmethod

    method Action update(Addr pc, Addr nextPc, Bool taken);
        updateEn.wset(BtbUpdate {pc: pc, nextPc: nextPc, taken: taken});
    endmethod

`ifdef SECURITY
    method Action flush if(flushDone);
        flushDone <= False;
    endmethod
    method flush_done = flushDone._read;
`else
    method flush = noAction;
    method flush_done = True;
`endif
endmodule

