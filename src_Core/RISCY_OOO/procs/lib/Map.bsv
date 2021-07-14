
// Copyright (c) 2021 Jonathan Woodruff
//
// All rights reserved.
//
// This software was developed by SRI International and the University of
// Cambridge Computer Laboratory (Department of Computer Science and
// Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
// DARPA SSITH research programme.
//
// This work was supported by NCSC programme grant 4212611/RFA 15971 ("SafeBet").
//
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

import DReg::*;
import RegFile::*;
import Vector::*;
import RWBramCore::*;
import Ehr::*;

typedef struct {
    ky key;
    ix index;
} MapKeyIndex#(type ky, type ix) deriving(Bits, Eq, FShow);
typedef struct {
    ky key;
    vl value;
} MapKeyValue#(type ky, type vl) deriving(Bits, Eq, FShow);
typedef struct {
    ky key;
    ix index;
    vl value;
} MapKeyIndexValue#(type ky, type ix, type vl) deriving(Bits, Eq, FShow);

// Type parameters are for index and key (which together are the "address"),
// the value stored in the map, and the associativity of the storage.
interface Map#(type ky, type ix, type vl, numeric type as);
    method Action update(MapKeyIndex#(ky,ix) key, vl value);
    method Maybe#(vl) lookup(MapKeyIndex#(ky,ix) lookup_key);
    method Action clear;
    method Bool clearDone;
endinterface

module mkMapLossy(Map#(ky,ix,vl,as)) provisos (
Bits#(ky,ky_sz), Bits#(vl,vl_sz), Eq#(ky), Arith#(ky),
Bounded#(ix), Literal#(ix), Bits#(ix, ix_sz),
Bitwise#(ix), Eq#(ix), Arith#(ix));
    Vector#(as, RegFile#(ix, MapKeyValue#(ky,vl))) mem
        <- replicateM(mkRegFileWCF(0, maxBound));
    Reg#(Bit#(TLog#(as))) wayNext <- mkReg(0);
    Integer a = valueof(as);

    Reg#(Bool) clearReg <- mkReg(True);
    Reg#(ix) clearCount <- mkReg(0);
    PulseWire didUpdate <- mkPulseWire;
    rule doClear(clearReg && !didUpdate);
        for (Integer i = 0; i < a; i = i + 1) mem[i].upd(clearCount, unpack(0));
        clearCount <= clearCount + 1;
        if (clearCount == ~0) clearReg <= False;
    endrule

    method Action update(MapKeyIndex#(ky,ix) ki, vl value);
        Bit#(TLog#(as)) way = wayNext;
        if (a > 1) begin
          for (Integer i = 0; i < a; i = i + 1)
              if (mem[i].sub(ki.index).key == ki.key) way = fromInteger(i);
        end
        mem[way].upd(ki.index, MapKeyValue{key: ki.key, value: value});
        wayNext <= (wayNext == fromInteger(a-1)) ? 0: wayNext + 1;
        didUpdate.send;
    endmethod
    method Maybe#(vl) lookup(MapKeyIndex#(ky,ix) lu);
        Maybe#(vl) ret = Invalid;
        for (Integer i = 0; i < a; i = i + 1) begin
            let rd = mem[i].sub(lu.index);
            if (rd.key == lu.key) ret = Valid(rd.value);
        end
        return ret;
    endmethod
    method clear if (!clearReg) = clearReg._write(True);
    method clearDone = clearReg;
endmodule

interface MapSplit#(type ky, type ix, type vl, numeric type as);
    method Action update(MapKeyIndex#(ky,ix) key, vl value);
    method Action lookupStart(MapKeyIndex#(ky,ix) lookup_key);
    method Maybe#(vl) lookupRead;
    method Action clear;
    method Bool clearDone;
endinterface

module mkMapLossyBRAM(MapSplit#(ky,ix,vl,as)) provisos (
Bits#(ky,ky_sz), Bits#(vl,vl_sz), Eq#(ky), Arith#(ky),
Bounded#(ix), Literal#(ix), Bits#(ix, ix_sz),
Bitwise#(ix), Eq#(ix), Arith#(ix), PrimIndex#(ix, a__));
    Vector#(as, RWBramCore#(ix, MapKeyValue#(ky,vl))) mem <- replicateM(mkRWBramCoreUG);
    Vector#(as, RWBramCore#(ix, ky)) updateKeys <- replicateM(mkRWBramCoreUG);
    Reg#(MapKeyIndex#(ky,ix)) lookupReg <- mkRegU;
    Reg#(MapKeyIndexValue#(ky,ix,vl)) updateReg <- mkRegU;
    Reg#(Bool) updateFresh <- mkDReg(False);
    Reg#(Bit#(TLog#(as))) wayNext <- mkReg(0);
    Integer a = valueof(as);

    Reg#(Bool) clearReg <- mkReg(True);
    Reg#(ix) clearCount <- mkReg(0);
    (* fire_when_enabled, no_implicit_conditions *)
    rule updateCanon;
        if (updateFresh) begin
            let u = updateReg;
            Bit#(TLog#(as)) way = wayNext;
            for (Integer i = 0; i < a; i = i + 1)
                if (updateKeys[i].rdResp == u.key) way = fromInteger(i);
            // Always write to both the main memory bank and the copy used for updates.
            /*$display("MapUpdate - index: %x, key: %x, value: %x, way: %x",
                     u.index, u.key, u.value, way);*/
            mem[way].wrReq(u.index, MapKeyValue{key: u.key, value: u.value});
            updateKeys[way].wrReq(u.index, u.key);
            wayNext <= (wayNext == fromInteger(a-1)) ? 0 : (wayNext + 1);
        end else if (clearReg) begin
            for (Integer i = 0; i < a; i = i + 1) mem[i].wrReq(clearCount, unpack(0));
            clearCount <= clearCount + 1;
            if (clearCount == ~0) clearReg <= False;
        end
    endrule

    method Action update(MapKeyIndex#(ky,ix) ki, vl value);
        updateReg <= MapKeyIndexValue{key: ki.key, index: ki.index, value: value};
        updateFresh <= True;
        for (Integer i = 0; i < a; i = i + 1) updateKeys[i].rdReq(ki.index);
    endmethod
    method Action lookupStart(MapKeyIndex#(ky,ix) ki);
        lookupReg <= ki;
        for (Integer i = 0; i < a; i = i + 1) mem[i].rdReq(ki.index);
    endmethod
    method Maybe#(vl) lookupRead;
        Maybe#(vl) readVal = Invalid;
        for (Integer i = 0; i < a; i = i + 1) begin
            let resp = mem[i].rdResp;
            if (lookupReg.key == resp.key) readVal = Valid(resp.value);
        end
        // If there has been a recent write, take that one.
        if (updateReg.index == lookupReg.index && updateReg.key == lookupReg.key)
            readVal = Valid(updateReg.value);
        return readVal;
    endmethod
    method clear if (!clearReg) = clearReg._write(True);
    method clearDone = clearReg;
endmodule
