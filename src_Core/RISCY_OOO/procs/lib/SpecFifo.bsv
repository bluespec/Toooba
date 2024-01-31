

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

import ProcTypes::*;
import HasSpecBits::*;
import Vector::*;
import Ehr::*;
import DReg::*;
import Assert::*;
import Types::*;
import ConfigReg::*;

typedef struct {
    t data;
    SpecBits spec_bits;
} ToSpecFifo#(type t) deriving(Bits, Eq, FShow);

interface SpecFifo#(
    numeric type size, type t,
    numeric type validPortNum, // valid EHR port num
    numeric type sbPortNum // specBits EHR port num
);
    method Action enq(ToSpecFifo#(t) x);
    method Bool notFull;
    method Action deq;
    method ToSpecFifo#(t) first;
    method Bool notEmpty;
    interface SpeculationUpdate specUpdate;
endinterface

// scheduling parameter
typedef struct {
    // valid EHR port assignment
    Integer validDeqPort;
    Integer validEnqPort;
    Integer validWrongSpecPort;
    // specBits EHR port assignment
    // correct spec is always the last
    Integer sbDeqPort;
    Integer sbEnqPort;
    Integer sbWrongSpecPort;
    // method/rule conflicting with wrongSpec
    // conflicts between wrong spec and correct spec are enforced at top level (Core.bsv)
    Bool wrongSpec_conflict_enq;
    Bool wrongSpec_conflict_deq;
    Bool wrongSpec_conflict_canon;
} SpecFifoSched deriving(Eq);

module mkSpecFifo#(
    SpecFifoSched sched, // schedule EHR ordering
    Bool lazyEnq // whether we calculate enq guard lazily
)(
    SpecFifo#(size, t, validPortNum, sbPortNum)
) provisos (
    Alias#(idxT, Bit#(TLog#(size))),
    Bits#(t, _tsz),
    FShow#(t)
);
    staticAssert(sched.validDeqPort < sched.validEnqPort, "enq must schedule after deq");

    // correct spec is always the last
    Integer sbCorrectSpecPort = valueof(sbPortNum) - 1;

    Vector#(size, Ehr#(validPortNum, Bool))  valid    <- replicateM(mkEhr(False));
    Vector#(size, Reg#(t))                   row      <- replicateM(mkRegU);
    Vector#(size, Ehr#(sbPortNum, SpecBits)) specBits <- replicateM(mkEhr(?));

    Reg#(idxT) enqP <- mkReg(0);
    Ehr#(2, idxT) deqP_ehr <- mkEhr(0);
    Reg#(idxT) deqP = deqP_ehr[0]; // port 0 is for deq and canon_deqP

    // make incorrectSpeculation conflict with others
    PulseWire dummyPulseWire = interface PulseWire;
        method Bool _read = False;
        method Action send = noAction;
    endinterface;
    PulseWire wrongSpec_enq_conflict = dummyPulseWire;
    PulseWire wrongSpec_deq_conflict = dummyPulseWire;
    PulseWire wrongSpec_canon_conflict = dummyPulseWire;
    if(sched.wrongSpec_conflict_enq) begin
        wrongSpec_enq_conflict <- mkPulseWire;
    end
    if(sched.wrongSpec_conflict_deq) begin
        wrongSpec_deq_conflict <- mkPulseWire;
    end
    if(sched.wrongSpec_conflict_canon) begin
        wrongSpec_canon_conflict <- mkPulseWire;
    end

    function idxT getNextPtr(idxT p);
        return p == fromInteger(valueOf(size) - 1) ? 0 : p + 1;
    endfunction

    Bool empty_for_canon = all( \== (False) , readVEhr(sched.validDeqPort, valid) );
    rule canon_deqP(!valid[deqP][sched.validDeqPort] && (enqP != deqP || !empty_for_canon)
                     && !wrongSpec_canon_conflict); // make conflict with incorrect spec
        // element at deqP was killed, so increment deqP
        deqP <= getNextPtr(deqP);
    endrule

    // calculate guard for enq, we can do aggressively or lazily
    idxT deqP_for_enq = ?;
    Bool empty_for_enq = ?;
    Bool valid_for_enq = ?;
    if(lazyEnq) begin
        // use the stale deqP & valid before canon_deqP or deq fires
        // because deq, canon_deqP, wrongSpec only set valid to false and move deqP forward
        // this just makes enq fire less aggressively
        Wire#(idxT) deqP_for_enq_wire <- mkBypassWire;
        Wire#(Bool) empty_for_enq_wire <- mkBypassWire;
        Wire#(Bool) valid_for_enq_wire <- mkBypassWire;
        (* fire_when_enabled, no_implicit_conditions *)
        rule setWireForEnq;
            deqP_for_enq_wire <= deqP;
            empty_for_enq_wire <= all( \== (False) , readVEhr(0, valid) );
            valid_for_enq_wire <= valid[enqP][0];
        endrule
        deqP_for_enq = deqP_for_enq_wire;
        empty_for_enq = empty_for_enq_wire;
        valid_for_enq = valid_for_enq_wire;
    end
    else begin
        deqP_for_enq = deqP_ehr[1]; // read up-to-date deqP
        empty_for_enq = all( \== (False) , readVEhr(sched.validEnqPort, valid) );
        valid_for_enq = valid[enqP][sched.validEnqPort];
    end

    method Action enq(ToSpecFifo#(t) x) if ((empty_for_enq || enqP != deqP_for_enq)
                                            && !wrongSpec_enq_conflict); // make conflict with incorrect spec
        // [sizhuo] I don't think valid bit needs to be checked here
        doAssert(!valid_for_enq, "enq entry cannot be valid");
        enqP <= getNextPtr(enqP);
        valid[enqP][sched.validEnqPort] <= True;
        row[enqP] <= x.data;
        specBits[enqP][sched.sbEnqPort] <= x.spec_bits;
    endmethod

    method Bool notFull = (empty_for_enq || enqP != deqP_for_enq);

    method Action deq if (valid[deqP][sched.validDeqPort]
                          && !wrongSpec_deq_conflict); // make conflict with incorrect spec
        valid[deqP][sched.validDeqPort] <= False;
        deqP <= getNextPtr(deqP);
    endmethod

    method ToSpecFifo#(t) first if (valid[deqP][sched.validDeqPort]);
        return ToSpecFifo{
            data: row[deqP],
            spec_bits: specBits[deqP][sched.sbDeqPort]
        };
    endmethod

    method Bool notEmpty = valid[deqP][sched.validDeqPort];

    interface SpeculationUpdate specUpdate;
        method Action correctSpeculation(SpecBits mask);
            // clear spec bits for all entries
            function Action correctSpec(Integer i);
            action
                SpecBits sb = specBits[i][sbCorrectSpecPort];
                specBits[i][sbCorrectSpecPort] <= sb & mask;
            endaction
            endfunction
            Vector#(size, Integer) idxVec = genVector;
            joinActions(map(correctSpec, idxVec));
        endmethod

        method Action incorrectSpeculation(Bool kill_all, SpecTag specTag);
            // clear entries
            function Action incorrectSpec(Integer i);
            action
                SpecBits sb = specBits[i][sched.sbWrongSpecPort];
                if(kill_all || sb[specTag] == 1) begin
                    valid[i][sched.validWrongSpecPort] <= False; // ordered after deq
                end
            endaction
            endfunction
            Vector#(size, Integer) idxVec = genVector;
            joinActions(map(incorrectSpec, idxVec));
            // make conflict with others
            wrongSpec_enq_conflict.send;
            wrongSpec_canon_conflict.send;
            wrongSpec_deq_conflict.send;
        endmethod
    endinterface
endmodule

typedef struct {
    Bool kill_all;
    SpecTag specTag;
} IncorrectSpeculation deriving(Bits, Eq, FShow);

module mkSpecFifoCF#(
    Bool lazyEnq // whether we calculate enq guard lazily
)(
    SpecFifo#(size, t, validPortNum, sbPortNum)
) provisos (
    Alias#(idxT, Bit#(TLog#(size))),
    Bits#(t, _tsz),
    FShow#(t)
);
    // correct spec is always the last
    Integer sbCorrectSpecPort = valueof(sbPortNum) - 1;

    Ehr#(3, Vector#(size, Bool))             valid    <- mkEhr(replicate(False));
    Vector#(size, Reg#(t))                   row      <- replicateM(mkConfigRegU);
    Ehr#(3, Vector#(size, SpecBits))         specBits <- mkEhr(?);

    Reg#(Maybe#(SpecBits))                correctSpec <- mkDReg(Invalid);
    Reg#(Maybe#(IncorrectSpeculation))  incorrectSpec <- mkDReg(Invalid);

    Reg#(idxT) enqP <- mkConfigReg(0);
    Ehr#(2, idxT) deqP_ehr <- mkEhr(0);
    Reg#(idxT) deqP = deqP_ehr[0]; // port 0 is for deq and canon_deqP

    function idxT getNextPtr(idxT p);
        return p == fromInteger(valueOf(size) - 1) ? 0 : p + 1;
    endfunction

    Bool empty_for_canon = all( \== (False) , valid[0] );
    rule canon_deqP(!valid[1][deqP] && (enqP != deqP || !empty_for_canon));
        // element at deqP was killed, so increment deqP
        deqP <= getNextPtr(deqP);
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule canon_speculation;
        Vector#(size, SpecBits) newSpecBits = specBits[0];
        Vector#(size, Bool) newValid = valid[0];
        // Fold in CorrectSpec update:
        if (correctSpec matches tagged Valid .mask) begin
            // clear spec bits for all entries
            for (Integer i=0; i<valueOf(size); i=i+1)
                newSpecBits[i] = newSpecBits[i] & mask;
        end
        // Fold in IncorrectSpec update:
        if (incorrectSpec matches tagged Valid .incSpec) begin
            // clear entries
            for (Integer i=0; i<valueOf(size); i=i+1)
                if(incSpec.kill_all || newSpecBits[i][incSpec.specTag] == 1'b1)
                    newValid[i] = False;
        end
        specBits[0] <= newSpecBits;
        valid[0] <= newValid;
    endrule

    // calculate guard for enq, we can do aggressively or lazily
    idxT deqP_for_enq = ?;
    Bool empty_for_enq = ?;
    Bool valid_for_enq = ?;
    if(lazyEnq) begin
        // use the stale deqP & valid before canon_deqP or deq fires
        // because deq, canon_deqP, wrongSpec only set valid to false and move deqP forward
        // this just makes enq fire less aggressively
        Wire#(idxT) deqP_for_enq_wire <- mkBypassWire;
        Wire#(Bool) empty_for_enq_wire <- mkBypassWire;
        Wire#(Bool) valid_for_enq_wire <- mkBypassWire;
        (* fire_when_enabled, no_implicit_conditions *)
        rule setWireForEnq;
            deqP_for_enq_wire <= deqP;
            empty_for_enq_wire <= all( \== (False) , valid[1] );
            valid_for_enq_wire <= valid[1][enqP];
        endrule
        deqP_for_enq = deqP_for_enq_wire;
        empty_for_enq = empty_for_enq_wire;
        valid_for_enq = valid_for_enq_wire;
    end
    else begin
        deqP_for_enq = deqP_ehr[1]; // read up-to-date deqP
        empty_for_enq = all( \== (False) , valid[2] );
        valid_for_enq = valid[2][enqP];
    end

    method Action enq(ToSpecFifo#(t) x) if (empty_for_enq || enqP != deqP_for_enq);
        // [sizhuo] I don't think valid bit needs to be checked here
        doAssert(!valid_for_enq, "enq entry cannot be valid");
        row[enqP] <= x.data;
        valid[2][enqP] <= True;
        specBits[2][enqP] <= x.spec_bits;
        enqP <= getNextPtr(enqP);
    endmethod

    method Bool notFull = (empty_for_enq || enqP != deqP_for_enq);

    method Action deq if (valid[1][deqP]);
        valid[1][deqP] <= False;
        deqP <= getNextPtr(deqP);
    endmethod

    method ToSpecFifo#(t) first if (valid[1][deqP]);
        return ToSpecFifo{
            data: row[deqP],
            spec_bits: specBits[1][deqP]
        };
    endmethod

    method Bool notEmpty = valid[1][deqP];

    interface SpeculationUpdate specUpdate;
        method correctSpeculation(mask) = correctSpec._write(Valid(mask));
        method incorrectSpeculation(kill_all, specTag) =
            incorrectSpec._write(Valid(IncorrectSpeculation{kill_all: kill_all, specTag: specTag}));
    endinterface
endmodule

module mkSpecFifoCF2#(
    Bool lazyEnq // whether we calculate enq guard lazily
)(
    SpecFifo#(size, t, validPortNum, sbPortNum)
) provisos (
    Alias#(idxT, Bit#(TLog#(size))),
    Bits#(t, _tsz),
    FShow#(t)
);
    // correct spec is always the last
    Integer sbCorrectSpecPort = valueof(sbPortNum) - 1;

    Ehr#(2, Vector#(size, Bool))             valid    <- mkEhr(replicate(False));
    Vector#(size, Reg#(t))                   row      <- replicateM(mkConfigRegU);
    Ehr#(3, Vector#(size, SpecBits))         specBits <- mkEhr(?);

    Reg#(Maybe#(SpecBits))                correctSpec <- mkDReg(Invalid);
    Reg#(Maybe#(IncorrectSpeculation))  incorrectSpec <- mkDReg(Invalid);

    PulseWire wrongSpec_conflict <- mkPulseWire;

    Reg#(idxT) enqP <- mkConfigReg(0);
    Ehr#(2, idxT) deqP_ehr <- mkEhr(0);
    Reg#(idxT) deqP = deqP_ehr[0]; // port 0 is for deq and canon_deqP

    function idxT getNextPtr(idxT p);
        return p == fromInteger(valueOf(size) - 1) ? 0 : p + 1;
    endfunction

    Bool empty_for_canon = all( \== (False) , valid[0] );
    rule canon_deqP(!valid[0][deqP] && ((enqP != deqP || !empty_for_canon) && !wrongSpec_conflict));
        // element at deqP was killed, so increment deqP
        deqP <= getNextPtr(deqP);
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule canon_speculation;
        Vector#(size, SpecBits) newSpecBits = specBits[0];
        // Fold in CorrectSpec update:
        if (correctSpec matches tagged Valid .mask) begin
            // clear spec bits for all entries
            for (Integer i=0; i<valueOf(size); i=i+1)
                newSpecBits[i] = newSpecBits[i] & mask;
        end
        specBits[0] <= newSpecBits;
    endrule

    // calculate guard for enq, we can do aggressively or lazily
    idxT deqP_for_enq = ?;
    Bool empty_for_enq = ?;
    Bool valid_for_enq = ?;
    if(lazyEnq) begin
        // use the stale deqP & valid before canon_deqP or deq fires
        // because deq, canon_deqP, wrongSpec only set valid to false and move deqP forward
        // this just makes enq fire less aggressively
        Wire#(idxT) deqP_for_enq_wire <- mkBypassWire;
        Wire#(Bool) empty_for_enq_wire <- mkBypassWire;
        Wire#(Bool) valid_for_enq_wire <- mkBypassWire;
        (* fire_when_enabled, no_implicit_conditions *)
        rule setWireForEnq;
            deqP_for_enq_wire <= deqP;
            empty_for_enq_wire <= all( \== (False) , valid[0] );
            valid_for_enq_wire <= valid[0][enqP];
        endrule
        deqP_for_enq = deqP_for_enq_wire;
        empty_for_enq = empty_for_enq_wire;
        valid_for_enq = valid_for_enq_wire;
    end
    else begin
        deqP_for_enq = deqP_ehr[1]; // read up-to-date deqP
        empty_for_enq = all( \== (False) , valid[1] );
        valid_for_enq = valid[1][enqP];
    end

    method Action enq(ToSpecFifo#(t) x) if ((empty_for_enq || enqP != deqP_for_enq) && !wrongSpec_conflict);
        // [sizhuo] I don't think valid bit needs to be checked here
        doAssert(!valid_for_enq, "enq entry cannot be valid");
        row[enqP] <= x.data;
        valid[1][enqP] <= True;
        specBits[2][enqP] <= x.spec_bits;
        enqP <= getNextPtr(enqP);
    endmethod

    method Bool notFull = (empty_for_enq || enqP != deqP_for_enq);

    method Action deq if (valid[0][deqP] && !wrongSpec_conflict);
        valid[0][deqP] <= False;
        deqP <= getNextPtr(deqP);
    endmethod

    method ToSpecFifo#(t) first if (valid[0][deqP] && !wrongSpec_conflict);
        return ToSpecFifo{
            data: row[deqP],
            spec_bits: specBits[1][deqP]
        };
    endmethod

    method Bool notEmpty = valid[0][deqP];

    interface SpeculationUpdate specUpdate;
        method correctSpeculation(mask) = correctSpec._write(Valid(mask));
        method Action incorrectSpeculation(kill_all, specTag);
            //incorrectSpec._write(Valid(IncorrectSpeculation{kill_all: kill_all, specTag: specTag}));
            Vector#(size, Bool) newValid = valid[0];
            // clear entries
            for (Integer i=0; i<valueOf(size); i=i+1)
                if(kill_all || specBits[0][i][specTag] == 1'b1)
                    newValid[i] = False;
            valid[0] <= newValid;
            wrongSpec_conflict.send();
        endmethod
    endinterface
endmodule

// deq < enq (< correctSpec)
// deq < wrongSpec
// wrongSpec C enq
typedef SpecFifo#(
    size, t, 2, 2
) SpecFifo_SB_deq_enq_SB_deq_wrong_C_enq#(numeric type size, type t);

module mkSpecFifo_SB_deq_enq_SB_deq_wrong_C_enq#(Bool lazyEnq)(
    SpecFifo_SB_deq_enq_SB_deq_wrong_C_enq#(size, t)
) provisos(Bits#(t, w), FShow#(t));
    let sched = SpecFifoSched {
        validDeqPort: 0,
        validEnqPort: 1,
        validWrongSpecPort: 1,
        sbDeqPort: 0,
        sbEnqPort: 0,
        sbWrongSpecPort: 0,
        wrongSpec_conflict_enq: True,
        wrongSpec_conflict_deq: False,
        wrongSpec_conflict_canon: False
    };
    let m <- mkSpecFifo(sched, lazyEnq);
    // let m <- mkSpecFifoCF(lazyEnq);
    return m;
endmodule


// deq < enq (< correctSpec)
// wrongSpec C enq
// wrongSpec C deq
typedef SpecFifo#(
    size, t, 2, 2
) SpecFifo_SB_deq_enq_C_deq_enq#(numeric type size, type t);

module mkSpecFifo_SB_deq_enq_C_deq_enq#(Bool lazyEnq)(
    SpecFifo_SB_deq_enq_C_deq_enq#(size, t)
) provisos(Bits#(t, w), FShow#(t));
    let sched = SpecFifoSched {
        validDeqPort: 0,
        validEnqPort: 1,
        validWrongSpecPort: 0,
        sbDeqPort: 0,
        sbEnqPort: 0,
        sbWrongSpecPort: 0,
        wrongSpec_conflict_enq: True,
        wrongSpec_conflict_deq: True,
        wrongSpec_conflict_canon: True // acutally canon never fire
    };
    let m <- mkSpecFifo(sched, lazyEnq);
    // let m <- mkSpecFifoCF(lazyEnq);
    return m;
endmodule


// deq < enq (< correctSpec)
// wrongSpec C enq
// wrongSpec C deq
typedef SpecFifo#(
    size, t, 2, 2
) SpecFifo_SB_enq_deq_C_enq_deq#(numeric type size, type t);

module mkSpecFifo_SB_enq_deq_C_enq_deq(
    SpecFifo_SB_deq_enq_C_deq_enq#(size, t)
) provisos(Bits#(t, w), FShow#(t));
    let sched = SpecFifoSched {
        validDeqPort: 1,
        validEnqPort: 0,
        validWrongSpecPort: 0,
        sbDeqPort: 0,
        sbEnqPort: 0,
        sbWrongSpecPort: 0,
        wrongSpec_conflict_enq: True,
        wrongSpec_conflict_deq: True,
        wrongSpec_conflict_canon: True // acutally canon never fire
    };
    let m <- mkSpecFifo(sched, True);
    return m;
endmodule

module mkSpecFifoUG#(Bool lazyEnq)(
    SpecFifo#(size, t, validPortNum, sbPortNum)
) provisos (
    Alias#(idxT, Bit#(TLog#(size))),
    Bits#(t, _tsz),
    FShow#(t)
);
    SpecFifo#(size, t, validPortNum, sbPortNum) m <- mkSpecFifoCF(lazyEnq);
    RWire#(ToSpecFifo#(t)) first_w <- mkRWire;
    RWire#(ToSpecFifo#(t)) enq_w <- mkRWire;
    PulseWire deq_w <- mkPulseWire;

    rule doFirst;
        first_w.wset(m.first);
    endrule

    rule doDeq(deq_w);
        m.deq;
    endrule

    rule doEnq(enq_w.wget matches tagged Valid .e);
        m.enq(e);
    endrule

    method enq = enq_w.wset;
    method notFull = m.notFull;
    method deq = deq_w.send;
    method ToSpecFifo#(t) first = fromMaybe(?,first_w.wget);
    method notEmpty = m.notEmpty;
    interface SpeculationUpdate specUpdate = m.specUpdate;
endmodule

interface SearchableSpecFifo#(
    numeric type size, type t,
    type searchOut, type searchIn
);
    method Action enq(ToSpecFifo#(t) x);
    method Action deq;
    method ToSpecFifo#(t) first;
    method Maybe#(searchOut) search(searchIn in);
    interface SpeculationUpdate specUpdate;
endinterface

module mkSearchableSpecFifoCF#(
    Bool lazyEnq, // whether we calculate enq guard lazily
    Bool unguarded_insert, // only for probabalistic applications
    function Maybe#(searchOut) search_f(searchIn w, t x)
)(
    SearchableSpecFifo#(size, t, searchOut, searchIn)
) provisos (
    Alias#(idxT, Bit#(TLog#(size))),
    Bits#(t, _tsz),
    FShow#(t)
);
    Ehr#(3, Vector#(size, Bool))             valid    <- mkEhr(replicate(False));
    Vector#(size, Reg#(t))                   row      <- replicateM(mkConfigRegU);
    Ehr#(3, Vector#(size, SpecBits))         specBits <- mkEhr(?);

    Reg#(Maybe#(SpecBits))                correctSpec <- mkDReg(Invalid);
    Reg#(Maybe#(IncorrectSpeculation))  incorrectSpec <- mkDReg(Invalid);

    Reg#(idxT) enqP <- mkConfigReg(0);
    Ehr#(2, idxT) deqP_ehr <- mkEhr(0);
    Reg#(idxT) deqP = deqP_ehr[0]; // port 0 is for deq and canon_deqP

    function idxT getNextPtr(idxT p);
        return p == fromInteger(valueOf(size) - 1) ? 0 : p + 1;
    endfunction

    Bool empty_for_canon = all( \== (False) , valid[0] );
    rule canon_deqP(!valid[1][deqP] && (enqP != deqP || !empty_for_canon));
        // element at deqP was killed, so increment deqP
        deqP <= getNextPtr(deqP);
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule canon_speculation;
        Vector#(size, SpecBits) newSpecBits = specBits[0];
        Vector#(size, Bool) newValid = valid[0];
        // Fold in CorrectSpec update:
        if (correctSpec matches tagged Valid .mask) begin
            // clear spec bits for all entries
            for (Integer i=0; i<valueOf(size); i=i+1)
                newSpecBits[i] = newSpecBits[i] & mask;
        end
        // Fold in IncorrectSpec update:
        if (incorrectSpec matches tagged Valid .incSpec) begin
            // clear entries
            for (Integer i=0; i<valueOf(size); i=i+1)
                if(incSpec.kill_all || newSpecBits[i][incSpec.specTag] == 1'b1)
                    newValid[i] = False;
        end
        specBits[0] <= newSpecBits;
        valid[0] <= newValid;
    endrule

    // calculate guard for enq, we can do aggressively or lazily
    idxT deqP_for_enq = ?;
    Bool empty_for_enq = ?;
    Bool valid_for_enq = ?;
    if(lazyEnq) begin
        // use the stale deqP & valid before canon_deqP or deq fires
        // because deq, canon_deqP, wrongSpec only set valid to false and move deqP forward
        // this just makes enq fire less aggressively
        Wire#(idxT) deqP_for_enq_wire <- mkBypassWire;
        Wire#(Bool) empty_for_enq_wire <- mkBypassWire;
        Wire#(Bool) valid_for_enq_wire <- mkBypassWire;
        (* fire_when_enabled, no_implicit_conditions *)
        rule setWireForEnq;
            deqP_for_enq_wire <= deqP;
            empty_for_enq_wire <= all( \== (False) , valid[1] );
            valid_for_enq_wire <= valid[1][enqP];
        endrule
        deqP_for_enq = deqP_for_enq_wire;
        empty_for_enq = empty_for_enq_wire;
        valid_for_enq = valid_for_enq_wire;
    end
    else begin
        deqP_for_enq = deqP_ehr[1]; // read up-to-date deqP
        empty_for_enq = all( \== (False) , valid[2] );
        valid_for_enq = valid[2][enqP];
    end

    Bool notFull = empty_for_enq || enqP != deqP_for_enq;
    method Action enq(ToSpecFifo#(t) x) if (notFull || unguarded_insert);
        if (notFull) begin
            // [sizhuo] I don't think valid bit needs to be checked here
            doAssert(!valid_for_enq, "enq entry cannot be valid");
            row[enqP] <= x.data;
            valid[2][enqP] <= True;
            specBits[2][enqP] <= x.spec_bits;
            enqP <= getNextPtr(enqP);
        end
    endmethod

    method Action deq if (valid[1][deqP]);
        valid[1][deqP] <= False;
        deqP <= getNextPtr(deqP);
    endmethod

    method ToSpecFifo#(t) first if (valid[1][deqP]);
        return ToSpecFifo{
            data: row[deqP],
            spec_bits: specBits[1][deqP]
        };
    endmethod

    method Maybe#(searchOut) search(searchIn in);
        Maybe#(searchOut) ret = Invalid;
        for(Integer i = 0; i < valueOf(size); i = i + 1)
            if (valid[0][i])
                if (search_f(in, row[i]) matches tagged Valid .y)
                    ret = Valid(y);
        return ret;
    endmethod

    interface SpeculationUpdate specUpdate;
        method correctSpeculation(mask) = correctSpec._write(Valid(mask));
        method incorrectSpeculation(kill_all, specTag) =
            incorrectSpec._write(Valid(IncorrectSpeculation{kill_all: kill_all, specTag: specTag}));
    endinterface
endmodule