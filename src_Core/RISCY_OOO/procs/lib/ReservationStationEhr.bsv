
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
import ConfigReg::*;
import HasSpecBits::*;
import Ehr::*;
import GetPut::*;
import Assert::*;

typedef struct{
    a data;
    PhyRegs regs;
    InstTag tag;
    // speculation
    SpecBits spec_bits;
    Maybe#(SpecTag) spec_tag;
    // scheduling
    RegsReady regs_ready;
} ToReservationStation#(type a) deriving(Bits, Eq, FShow);

interface ReservationStation#(
    numeric type size, numeric type setRegReadyNum, type a
);
    method Action enq(ToReservationStation#(a) x);
    method Bool canEnq;

    method Action setRobEnqTime(InstTime t);
    method ToReservationStation#(a) dispatchData;
    method Action doDispatch;

    interface Vector#(setRegReadyNum, Put#(Maybe#(PhyRIndx))) setRegReady;

    // For count-based scheduling when there are multiple reservation stations
    // for the same inst type. This method only takes effect when module
    // parameter countValid is true.
    method Bit#(TLog#(TAdd#(size, 1))) approximateCount;

    // performance: count full cycles
    method Bool isFull_ehrPort0;

    interface SpeculationUpdate specUpdate;
endinterface

typedef Bit#(TAdd#(1, TLog#(NumInstTags))) VirtualInstTime;

module mkReservationStation#(Bool lazySched, Bool lazyEnq, Bool countValid)(
    ReservationStation#(size, setRegReadyNum, a)
) provisos (
    NumAlias#(regsReadyPortNum, TAdd#(1, setRegReadyNum)),
    Alias#(idxT, Bit#(TLog#(size))),
    Alias#(countT, Bit#(TLog#(TAdd#(size, 1)))),
    Log#(TAdd#(1, size), TLog#(TAdd#(size, 1))),
    Bits#(a, aSz), FShow#(a),
    Add#(1, b__, size)
);
    Integer valid_wrongSpec_port = 0;
    Integer valid_dispatch_port = 0; // write valid
    Integer valid_enq_port = 1; // write valid

    Integer sb_wrongSpec_port = 0;
    Integer sb_dispatch_port = 0;
    Integer sb_enq_port = 0; // write spec_bits
    Integer sb_correctSpec_port = 1; // write spec_bits

    function Integer ready_set_port(Integer i) = i; // write regs_ready by each setRegReady ifc
    Integer ready_enq_port = valueof(setRegReadyNum); // write regs_ready

    Vector#(size, Ehr#(2,Bool))                      valid      <- replicateM(mkEhr(False));
    Vector#(size, Reg#(a))                           data       <- replicateM(mkRegU);
    Vector#(size, Reg#(PhyRegs))                     regs       <- replicateM(mkRegU);
    Vector#(size, Reg#(InstTag))                     tag        <- replicateM(mkRegU);
    Vector#(size, Reg#(Maybe#(SpecTag)))             spec_tag   <- replicateM(mkRegU);
    Vector#(size, Ehr#(2, SpecBits))                 spec_bits  <- replicateM(mkEhr(?));
    Vector#(size, Ehr#(regsReadyPortNum, RegsReady)) regs_ready <- replicateM(mkEhr(?));

    // wrong spec conflict with enq and dispatch
    RWire#(void) wrongSpec_enq_conflict <- mkRWire;
    RWire#(void) wrongSpec_dispatch_conflict <- mkRWire;

    // approximate count of valid entries
    Reg#(countT) validEntryCount <- mkConfigReg(0);

    if(countValid) begin
        (* fire_when_enabled, no_implicit_conditions *)
        rule countValidEntries;
            validEntryCount <= pack(countElem(True, readVEhr(0, valid)));
        endrule
    end

    // enq time in ROB, used as pivot to get virtual inst time (dispatch happens before ROB enq)
    Wire#(InstTime) robEnqTime <- mkDWire(0);
    // mapping to virtual inst time as follow:
    // valid entry time t --> t < robEnqTime ? t + 2^log(NumInstTags) : t
    function VirtualInstTime getVTime(InstTag instTag);
        InstTime t = instTag.t;
        return t < robEnqTime ? zeroExtend(t) + fromInteger(valueof(TExp#(TLog#(NumInstTags)))) : zeroExtend(t);
    endfunction
    Vector#(size, VirtualInstTime) vTime = map(getVTime, readVReg(tag));
    // function to find the oldest entry (smaller virtual time) which satisfy certain constraint
    function Maybe#(idxT) findOldest(Vector#(size, Bool) pred);
        function idxT getOlder(idxT a, idxT b);
            if(!pred[a]) begin
                return b;
            end
            else if(!pred[b]) begin
                return a;
            end
            else begin
                return vTime[a] < vTime[b] ? a : b;
            end
        endfunction
        Vector#(size, idxT) idxVec = genWith(fromInteger);
        idxT idx = fold(getOlder, idxVec);
        return pred[idx] ? Valid (idx) : Invalid;
    endfunction

    // search for row to dispatch, we do it lazily
    // i.e. look at the EHR port 0 of ready bits, so there is no bypassing
    staticAssert(lazySched, "Only support lazy schedule now");
    
    Vector#(size, Wire#(Bool)) ready_wire <- replicateM(mkBypassWire);
    (* fire_when_enabled, no_implicit_conditions *)
    rule setReadyWire;
        function Action setReady(Integer i);
        action
            ready_wire[i] <= allRegsReady(regs_ready[i][0]);
        endaction
        endfunction
        Vector#(size, Integer) idxVec = genVector;
        joinActions(map(setReady, idxVec));
    endrule

    function Bool get_ready(Wire#(Bool) r);
        return r;
    endfunction
    Vector#(size, Bool) can_schedule = zipWith( \&& , readVEhr(valid_dispatch_port, valid), map(get_ready, ready_wire) );
    
    // oldest index to dispatch
    let can_schedule_index = findOldest(can_schedule);

    // ifc to set reg ready
    Vector#(setRegReadyNum, Put#(Maybe#(PhyRIndx))) setRegReadyIfc = ?;
    for(Integer k = 0; k < valueof(setRegReadyNum); k = k+1) begin
        setRegReadyIfc[k] = (interface Put;
            method Action put(Maybe#(PhyRIndx) x);
                Integer ehrPort = ready_set_port(k);
                function Action setReady(Integer i);
                action
                    // function to set ready for element i
                    // This compares Maybe types.
                    // If both are invalid, regs_ready.srcX must be True.
                    RegsReady new_regs_ready = regs_ready[i][ehrPort];
                    if (x == regs[i].src1) begin
                        new_regs_ready.src1 = True;
                    end
                    if (x == regs[i].src2) begin
                        new_regs_ready.src2 = True;
                    end
                    if (x == regs[i].src3) begin
                        new_regs_ready.src3 = True;
                    end
                    regs_ready[i][ehrPort] <= new_regs_ready;
                endaction
                endfunction
                Vector#(size, Integer) idxVec = genVector;
                joinActions(map(setReady, idxVec));
            endmethod
        endinterface);
    end

    // search to find enq slot
    Maybe#(UInt#(TLog#(size))) enqP = ?;
    function Bool entryInvalid(Bool v) = !v;

    if(lazyEnq) begin
        Wire#(Maybe#(UInt#(TLog#(size)))) enqP_wire <- mkBypassWire;
        (* fire_when_enabled, no_implicit_conditions *)
        rule setWireForEnq;
            enqP_wire <= findIndex(entryInvalid, readVEhr(0, valid));
        endrule
        enqP = enqP_wire;
    end
    else begin
        enqP = findIndex(entryInvalid, readVEhr(valid_enq_port, valid));
    end

    //rule debugRes(!isValid(enqP));
    //    $fdisplay(stdout, "cannot enqueue in the reservation station this cycle");
    //endrule

    method Action enq(ToReservationStation#(a) x) if (enqP matches tagged Valid .idx);
        $display("  [mkReservationStationRow::_write] ", fshow(x));
        valid[idx][valid_enq_port] <= True;
        data[idx] <= x.data;
        regs[idx] <= x.regs;
        tag[idx] <= x.tag;
        spec_tag[idx] <= x.spec_tag;
        spec_bits[idx][sb_enq_port] <= x.spec_bits;
        regs_ready[idx][ready_enq_port] <= x.regs_ready;
        // conflict with wrong spec
        wrongSpec_enq_conflict.wset(?);
    endmethod
    method Bool canEnq = isValid(enqP);

    method Action setRobEnqTime(InstTime t);
        robEnqTime <= t;
    endmethod

    method ToReservationStation#(a) dispatchData if (can_schedule_index matches tagged Valid .i);
        return ToReservationStation{
            data: data[i],
            regs: regs[i],
            tag: tag[i],
            spec_bits: spec_bits[i][sb_dispatch_port],
            spec_tag: spec_tag[i],
            regs_ready: RegsReady { // must be all true
                src1: True,
                src2: True,
                src3: True,
                dst: True
            }
        };
    endmethod

    method Action doDispatch if (can_schedule_index matches tagged Valid .i);
        valid[i][valid_dispatch_port] <= False;
        // conflict with wrong spec
        wrongSpec_dispatch_conflict.wset(?);
    endmethod

    method countT approximateCount;
        return validEntryCount;
    endmethod

    interface setRegReady = setRegReadyIfc;

    method Bool isFull_ehrPort0;
        return readVEhr(0, valid) == replicate(True);
    endmethod

    interface SpeculationUpdate specUpdate;
        method Action incorrectSpeculation(Bool kill_all, SpecTag x);
            function Action wrongSpec(Integer i);
            action
                if(kill_all || spec_bits[i][sb_wrongSpec_port][x] == 1) begin
                    valid[i][valid_wrongSpec_port] <= False;
                end
            endaction
            endfunction
            Vector#(size, Integer) idxVec = genVector;
            joinActions(map(wrongSpec, idxVec));
            // conflict with enq, dispatch
            wrongSpec_enq_conflict.wset(?);
            wrongSpec_dispatch_conflict.wset(?);
        endmethod

        method Action correctSpeculation(SpecBits mask);
            function Action correctSpec(Integer i);
            action
                spec_bits[i][sb_correctSpec_port] <= spec_bits[i][sb_correctSpec_port] & mask;
            endaction
            endfunction
            Vector#(size, Integer) idxVec = genVector;
            joinActions(map(correctSpec, idxVec));
        endmethod
    endinterface
endmodule
