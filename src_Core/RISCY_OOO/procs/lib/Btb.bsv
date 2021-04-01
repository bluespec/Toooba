
// Copyright (c) 2017 Massachusetts Institute of Technology
//
//-
// RVFI_DII + CHERI modifications:
//     Copyright (c) 2020 Jessica Clarke
//     Copyright (c) 2020 Jonathan Woodruff
//     All rights reserved.
//
//     This software was developed by SRI International and the University of
//     Cambridge Computer Laboratory (Department of Computer Science and
//     Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
//     DARPA SSITH research programme.
//
//     This work was supported by NCSC programme grant 4212611/RFA 15971 ("SafeBet").
//-
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
import Map::*;
import Vector::*;
import CHERICC_Fat::*;
import CHERICap::*;

export NextAddrPred(..);
export mkBtb;

interface NextAddrPred#(numeric type hashSz);
    method Action put_pc(CapMem pc);
    interface Vector#(SupSizeX2, Maybe#(CapMem)) pred;
    method Action update(CapMem pc, CapMem brTarget, Bool taken);
    // security
    method Action flush;
    method Bool flush_done;
endinterface

// Local BTB Typedefs
typedef 1 PcLsbsIgnore;
typedef 1024 BtbEntries;
typedef 2 BtbAssociativity;
typedef Bit#(TLog#(SupSizeX2)) BtbBank;
// Total entries/lanes of superscalar lookup/associativity
typedef TDiv#(TDiv#(BtbEntries,SupSizeX2),BtbAssociativity) BtbIndices;
typedef Bit#(TLog#(BtbIndices)) BtbIndex;
typedef Bit#(TSub#(TSub#(TSub#(AddrSz,SizeOf#(BtbBank)), SizeOf#(BtbIndex)), PcLsbsIgnore)) BtbTag;
typedef Bit#(hashSz) HashedTag#(numeric type hashSz);

typedef struct {
    BtbTag tag;
    BtbIndex index;
    BtbBank bank;
} BtbAddr deriving(Bits, Eq, FShow);

typedef struct {
    CapMem pc;
    CapMem nextPc;
    Bool taken;
} BtbUpdate deriving(Bits, Eq, FShow);

typedef struct {
    Bool v;
    data d;
} VnD#(type data) deriving(Bits, Eq, FShow);

(* synthesize *)
module mkBtb(NextAddrPred#(16));
    NextAddrPred#(16) btb <- mkBtbCore;
    return btb;
endmodule

module mkBtbCore(NextAddrPred#(hashSz))
    provisos (NumAlias#(tagSz, TSub#(TSub#(TSub#(AddrSz,SizeOf#(BtbBank)), SizeOf#(BtbIndex)), PcLsbsIgnore)),
        Add#(1, a__, TDiv#(tagSz, hashSz)),
    Add#(b__, tagSz, TMul#(TDiv#(tagSz, hashSz), hashSz)));
    // Read and Write ordering doesn't matter since this is a predictor
    Reg#(BtbBank) firstBank_reg <- mkRegU;
    Vector#(SupSizeX2, MapSplit#(HashedTag#(hashSz), BtbIndex, VnD#(CapMem), BtbAssociativity))
        records <- replicateM(mkMapLossyBRAM);
    RWire#(BtbUpdate) updateEn <- mkRWire;

    function BtbAddr getBtbAddr(CapMem pc) = unpack(truncateLSB(getAddr(pc)));
    function BtbBank getBank(CapMem pc) = getBtbAddr(pc).bank;
    function BtbIndex getIndex(CapMem pc) = getBtbAddr(pc).index;
    function BtbTag getTag(CapMem pc) = getBtbAddr(pc).tag;
    function MapKeyIndex#(HashedTag#(hashSz),BtbIndex) lookupKey(CapMem pc) =
        MapKeyIndex{key: hash(getTag(pc)), index: getIndex(pc)};

    // no flush, accept update
    (* fire_when_enabled, no_implicit_conditions *)
    rule canonUpdate(updateEn.wget matches tagged Valid .upd);
        let pc = upd.pc;
        let nextPc = upd.nextPc;
        let taken = upd.taken;
        /*$display("MapUpdate in BTB - pc %x, bank: %x, taken: %x, next: %x, time: %t",
                  pc, getBank(pc), taken, nextPc, $time);*/
        records[getBank(pc)].update(lookupKey(pc), VnD{v:taken, d:nextPc});
    endrule

    method Action put_pc(CapMem pc);
        BtbAddr addr = getBtbAddr(pc);
        firstBank_reg <= addr.bank;
        // Start SupSizeX2 BTB lookups, but ensure to lookup in the appropriate
        // bank for the alignment of each potential branch.
        for (Integer i = 0; i < valueOf(SupSizeX2); i = i + 1) begin
            BtbAddr a = unpack(pack(addr) + fromInteger(i));
            records[a.bank].lookupStart(MapKeyIndex{key: hash(a.tag), index: a.index});
        end
    endmethod

    method Vector#(SupSizeX2, Maybe#(CapMem)) pred;
        Vector#(SupSizeX2, Maybe#(CapMem)) ppcs = replicate(Invalid);
        for (Integer i = 0; i < valueOf(SupSizeX2); i = i + 1)
            if (records[i].lookupRead matches tagged Valid .record)
                ppcs[i] = record.v ? Valid(record.d):Invalid;
        ppcs = rotateBy(ppcs,unpack(-firstBank_reg)); // Rotate firstBank down to zeroeth element.
        return ppcs;
    endmethod

    method Action update(CapMem pc, CapMem nextPc, Bool taken);
        updateEn.wset(BtbUpdate {pc: pc, nextPc: nextPc, taken: taken});
    endmethod

`ifdef SECURITY
    method Action flush method Action flush;
        for (Integer i = 0; i < valueOf(SupSizeX2); i = i + 1) records[i].clear;
    endmethod
    method flush_done = records[0].clearDone;
`else
    method flush = noAction;
    method flush_done = True;
`endif
endmodule
