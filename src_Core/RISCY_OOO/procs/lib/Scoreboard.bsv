
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

import Vector::*;
import Fifo::*;
import ProcTypes::*;
import CsrFile::*; // for mkReadOnlyReg
import Ehr::*;
import GetPut::*;

interface Scoreboard#(numeric type size, type t);
  method Action insert(Maybe#(t) r);
  method Action remove;
  method Bool search1(Maybe#(t) r);
  method Bool search2(Maybe#(t) r);
  method Bool search3(Maybe#(t) r);
  method Action clear;
endinterface

interface PhyRegsScoreboard;
  method Action insert(Maybe#(PhyDst) r);
  method Action remove(Maybe#(PhyDst) r);
  method Bool search(PhyRegs r);
  method Action clear;
endinterface

function Bool isFound(Maybe#(t) x, Maybe#(t) k) provisos (Eq#(t));
   if(x matches tagged Valid .xv &&& k matches tagged Valid .kv &&& kv == xv)
     return True;
   else
     return False;
endfunction

// search CF {enq, deq, first, notEmpty, notFull}
// deq CF enq
// search < clear
module mkCFScoreboard(Scoreboard#(size,t)) provisos (Bits#(t,tSz), Eq#(t));
  SFifo#(size, Maybe#(t), Maybe#(t))  f <- mkCFSFifo(isFound);

  method insert = f.enq;

  method remove = f.deq;

  method search1 = f.search;
  method search2 = f.search;
  method search3 = f.search;

  method clear = f.clear;
endmodule

function PhyRegsScoreboard phyRegsScoreboardWrapper(Scoreboard#(size, PhyRIndx) sb);
    return (interface PhyRegsScoreboard;
            method Action insert(Maybe#(PhyDst) r);
                if (r matches tagged Valid .valid_dst) begin
                    sb.insert(tagged Valid (valid_dst.indx));
                end else begin
                    sb.insert(tagged Invalid);
                end
            endmethod
            method Action remove(Maybe#(PhyDst) r);
                sb.remove;
            endmethod
            method Bool search(PhyRegs r);
                return sb.search1(r.src1) || sb.search2(r.src2) || sb.search3(r.src3);
            endmethod
            method Action clear;
                sb.clear;
            endmethod
        endinterface);
endfunction

module mkBitVectorScoreboard(PhyRegsScoreboard);
    Vector#(NumPhyReg, Reg#(Bool)) bitvector <- replicateM(mkReg(False));
    bitvector[0] <- mkReadOnlyReg(False);

    method Action insert(Maybe#(PhyDst) r);
        if (r matches tagged Valid .valid_dst) begin
            bitvector[pack(valid_dst.indx)] <= True;
        end
    endmethod
    method Action remove(Maybe#(PhyDst) r);
        if (r matches tagged Valid .valid_dst) begin
            bitvector[pack(valid_dst.indx)] <= False;
        end
    endmethod
    method Bool search(PhyRegs r);
        Bool found = False;
        if (r.src1 matches tagged Valid .valid_reg) begin
            found = found || bitvector[pack(valid_reg)];
        end
        if (r.src2 matches tagged Valid .valid_reg) begin
            found = found || bitvector[pack(valid_reg)];
        end
        if (r.src3 matches tagged Valid .valid_reg) begin
            found = found || bitvector[pack(valid_reg)];
        end
        if (r.dst matches tagged Valid .valid_dst) begin
            found = found || bitvector[pack(valid_dst.indx)];
        end
        return found;
    endmethod
    method Action clear;
        writeVReg(bitvector, replicate(False));
    endmethod
endmodule

// notEmpty < first < deq < search < notFull < enq < clear
module mkPipelineScoreboard(Scoreboard#(size,t)) provisos (Bits#(t,tSz), Eq#(t));
  SFifo#(size, Maybe#(t), Maybe#(t)) f <- mkPipelineSFifo(isFound);

  method insert = f.enq;

  method remove = f.deq;

  method search1 = f.search;
  method search2 = f.search;
  method search3 = f.search;

  method clear = f.clear;
endmodule

interface CountScoreboard#(numeric type size, type t);
  method Action insert(Maybe#(t) r);
  method Action remove;
  method Bit#(TLog#(TAdd#(size, 1))) search1(Maybe#(t) r);
  method Bit#(TLog#(TAdd#(size, 1))) search2(Maybe#(t) r);
  method Bit#(TLog#(TAdd#(size, 1))) search3(Maybe#(t) r);
  method Action clear;
endinterface

// search CF {enq, deq, first, notEmpty, notFull}
// deq CF enq
// search < clear
module mkCFCountScoreboard(CountScoreboard#(size,t)) provisos (Bits#(t,tSz), Eq#(t));
  SCountFifo#(size, Maybe#(t), Maybe#(t))  f <- mkCFSCountFifo(isFound);

  method insert = f.enq;

  method remove = f.deq;

  method search1 = f.search;
  method search2 = f.search;
  method search3 = f.search;

  method clear = f.clear;
endmodule

// Scoreboard to use with real register renaming (no WAW hazards)
interface SbLookup;
    method RegsReady get(PhyRegs r);
endinterface
interface SbSetBusy;
    method Action set(Maybe#(PhyDst) dst);
endinterface
interface RenamingScoreboard#(numeric type setReadyNum, numeric type lazyLookupNum);
    // eager look up when insert to reservation station, must see setReady & previous setBusy effects
    interface Vector#(SupSize, SbLookup) eagerLookup;
    interface Vector#(SupSize, SbSetBusy) setBusy;
    interface Vector#(setReadyNum, Put#(PhyRIndx)) setReady;
    // lazy look up in reg read, no need to see setReady effect (reg read rule will check bypass)
    interface Vector#(lazyLookupNum, SbLookup) lazyLookup;
endinterface

// ordering: setReady < eagerLookup, setBusy
// lazyLookup just check EHR port 0 using wires
module mkRenamingScoreboard(RenamingScoreboard#(setReadyNum, lazyLookupNum)) provisos (
    NumAlias#(portNum, TAdd#(SupSize, setReadyNum)) 
);
    // sb[i] == False => register ready to read
    // sb[i] == True => register will be written soon, not ready to read
    Vector#(NumPhyReg, Ehr#(portNum, Bool)) sb <- replicateM(mkEhr(False));

    function Integer setReady_port(Integer i) = i;
    function Integer lookup_port(Integer i) = i + valueof(setReadyNum);
    function Integer setBusy_port(Integer i) = i + valueof(setReadyNum);

    Vector#(setReadyNum, Put#(PhyRIndx)) setReadyIfc = ?;
    for(Integer i = 0; i < valueof(setReadyNum); i = i+1) begin
        setReadyIfc[i] = (interface Put;
            method Action put(PhyRIndx dst);
                Integer ehrPort = setReady_port(i);
                sb[dst][ehrPort] <= False;
            endmethod
        endinterface);
    end

    // wire to do lazy lookup
    Wire#(Vector#(NumPhyReg, Bool)) sbWire <- mkBypassWire;
    (* fire_when_enabled, no_implicit_conditions *)
    rule setSbWire;
        sbWire <= readVEhr(0, sb);
    endrule

    // function to derive lookup interface
    function SbLookup getLookupIfc(Vector#(NumPhyReg, Bool) sbVec);
        return (interface SbLookup;
            method RegsReady get(PhyRegs r);
                // everything is ready by default
                RegsReady ret = RegsReady{src1: True, src2: True, src3: True, dst: True};
                if (r.src1 matches tagged Valid .x) begin
                    ret.src1 = !sbVec[x];
                end
                if (r.src2 matches tagged Valid .x) begin
                    ret.src2 = !sbVec[x];
                end
                if (r.src3 matches tagged Valid .x) begin
                    ret.src3 = !sbVec[x];
                end
                return ret;
            endmethod
        endinterface);
    endfunction

    function SbLookup getEagerLookupIfc(Integer i) = getLookupIfc(readVEhr(lookup_port(i), sb));

    function SbSetBusy getSetBusyIfc(Integer i);
        return (interface SbSetBusy;
            method Action set(Maybe#(PhyDst) dst);
                if (dst matches tagged Valid .valid_dst) begin
                    sb[valid_dst.indx][setBusy_port(i)] <= True;
                end
            endmethod
        endinterface);
    endfunction

    interface setBusy = map(getSetBusyIfc, genVector);
    interface eagerLookup = map(getEagerLookupIfc, genVector);

    interface lazyLookup = replicate(getLookupIfc(sbWire));

    interface setReady = setReadyIfc;
endmodule


// BYPASS scoreboard to use with register renaming in an in-order core
// setReady < lookup < setBusy
interface InorderRenamingScoreboard#(
    numeric type setReadyNum, numeric type lookupNum
);
    interface Vector#(lookupNum, SbLookup) lookup; // together with RF read
    interface Vector#(SupSize, SbSetBusy) setBusy; // at rename
    interface Vector#(setReadyNum, Put#(PhyRIndx)) setReady; // exe finishes
endinterface

module mkInorderRenamingScoreboard(
    InorderRenamingScoreboard#(setReadyNum, lookupNum)
) provisos (
    NumAlias#(portNum, TAdd#(SupSize, setReadyNum)) 
);
    // sb[i] == False => register ready to read
    // sb[i] == True => register will be written soon, not ready to read
    Vector#(NumPhyReg, Ehr#(portNum, Bool)) sb <- replicateM(mkEhr(False));

    function Integer setReady_port(Integer i) = i;
    function Integer lookup_port = valueof(setReadyNum);
    function Integer setBusy_port(Integer i) = i + valueof(setReadyNum);

    // function to derive set ready interface
    function Put#(PhyRIndx) getSetReadyIfc(Integer i);
        return (interface Put;
            method Action put(PhyRIndx dst);
                sb[dst][setReady_port(i)] <= False;
            endmethod
        endinterface);
    endfunction

    // function to derive lookup interface
    function SbLookup getLookupIfc(Integer i);
        Vector#(NumPhyReg, Bool) sbVec = readVEhr(lookup_port, sb);
        return (interface SbLookup;
            method RegsReady get(PhyRegs r);
                // everything is ready by default
                RegsReady ret = RegsReady{src1: True, src2: True, src3: True, dst: True};
                if (r.src1 matches tagged Valid .x) begin
                    ret.src1 = !sbVec[x];
                end
                if (r.src2 matches tagged Valid .x) begin
                    ret.src2 = !sbVec[x];
                end
                if (r.src3 matches tagged Valid .x) begin
                    ret.src3 = !sbVec[x];
                end
                return ret;
            endmethod
        endinterface);
    endfunction

    // function to derive set busy interface
    function SbSetBusy getSetBusyIfc(Integer i);
        return (interface SbSetBusy;
            method Action set(Maybe#(PhyDst) dst);
                if (dst matches tagged Valid .valid_dst) begin
                    sb[valid_dst.indx][setBusy_port(i)] <= True;
                end
            endmethod
        endinterface);
    endfunction

    interface setBusy = map(getSetBusyIfc, genVector);
    interface lookup = map(getLookupIfc, genVector);
    interface setReady = map(getSetReadyIfc, genVector);
endmodule
