/*-
 * Copyright (c) 2020 Jonathan Woodruff
 * All rights reserved.
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory (Department of Computer Science and
 * Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
 * DARPA SSITH research programme.
 *
 * @BERI_LICENSE_HEADER_START@
 *
 * Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
 * license agreements.  See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.  BERI licenses this
 * file to you under the BERI Hardware-Software License, Version 1.0 (the
 * "License"); you may not use this file except in compliance with the
 * License.  You may obtain a copy of the License at:
 *
 *   http://www.beri-open-systems.org/legal/license-1-0.txt
 *
 * Unless required by applicable law or agreed to in writing, Work distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations under the License.
 *
 * @BERI_LICENSE_HEADER_END@
 */

import Vector::*;
import Assert::*;

interface IndexedMultisetRemoveIfc#(type idxT);
    method Action remove(idxT i);
endinterface
interface IndexedMultiset#(type idxT, type datT, numeric type remWidth);
    method ActionValue#(idxT) insert(datT d);
    // Reserved data values must be the same as the next inserted value or they will be overwritten.
    method ActionValue#(IndexedMultisetIndices#(idxT)) insertAndReserve(datT ins, datT res);
    method datT lookup(idxT i);
    interface Vector#(remWidth, IndexedMultisetRemoveIfc#(idxT)) rPort;
    method Action clearAll;
endinterface

typedef struct {
    datT data;
    ctr count;
} IndexedMultisetRecord#(type datT, type ctr) deriving(Bits);

typedef struct {
    idxT inserted;
    idxT reserved;
} IndexedMultisetIndices#(type idxT) deriving(Bits);

typedef struct {
    idxT idx;
    datT dat;
} IndexedMultisetInsert#(type idxT, type datT) deriving(Bits);

module mkIndexedMultiset(IndexedMultiset#(idxT, datT, remWidth))
    provisos (
        Bits#(datT, _datsz),
        Eq#(datT),
        Bits#(idxT, _idxsz),
        PrimIndex#(idxT, a__),
        Alias#(wide_idxT, Bit#(TAdd#(_idxsz,2))) // Allow for generous number of duplicates per data value; 4x index capacity.
    );
    Vector#(TExp#(_idxsz), Reg#(IndexedMultisetRecord#(datT,wide_idxT)))
        records <- replicateM(mkReg(unpack(0)));
    let recsRead = readVReg(records);
    RWire#(IndexedMultisetInsert#(idxT,datT)) insertW <- mkRWire;
    RWire#(IndexedMultisetInsert#(idxT,datT)) reserveW <- mkRWire;
    Vector#(remWidth, RWire#(idxT)) removeW <- replicateM(mkRWire);
    PulseWire clearW <- mkPulseWire;

    (* no_implicit_conditions *)
    rule canon;
        Vector#(TExp#(_idxsz),IndexedMultisetRecord#(datT,wide_idxT)) recs = readVReg(records);
        if (insertW.wget matches tagged Valid .r) begin
            recs[r.idx].data = r.dat;
            recs[r.idx].count = recs[r.idx].count + 1;
        end
        if (reserveW.wget matches tagged Valid .r) recs[r.idx].data = r.dat;
        for (Integer i = 0; i < valueOf(remWidth); i = i + 1)
            if (removeW[i].wget matches tagged Valid .r) recs[r].count = recs[r].count - 1;
        if (clearW) recs = replicate(IndexedMultisetRecord{data: ?, count: 0});
        writeVReg(records, recs);
    endrule

    function isEmpty(r) = (r.count == 0);
    function isFull(r) = (r.count == ~0);
    function dataMatch(d, r) = (r.data == d);
    function ActionValue#(idxT) findIdx(datT d, Vector#(TExp#(_idxsz), IndexedMultisetRecord#(datT,wide_idxT)) recs) =
        actionvalue
            let mi = findIndex(dataMatch(d),recs);
            if (!isValid(mi)) mi = findIndex(isEmpty,recs);
            dynamicAssert(isValid(mi), "failed to insert in indexed multiset");
            return unpack(pack(validValue(mi)));
        endactionvalue;

    Vector#(remWidth, IndexedMultisetRemoveIfc#(idxT)) removes;
    for (Integer i = 0; i < valueOf(remWidth); i = i + 1) begin
        removes[i] = interface IndexedMultisetRemoveIfc#(idxT);
            method Action remove(idxT index);
                dynamicAssert(recsRead[index].count != 0, "removed empty item in indexed multiset");
                removeW[i].wset(index);
            endmethod
        endinterface;
    end

    Bool anyEmpty = any(isEmpty,recsRead);
    Bool anyFull = any(isFull,recsRead);
    method ActionValue#(idxT) insert(datT d) if (anyEmpty && !anyFull);
        let i <- findIdx(d,recsRead);
        insertW.wset(IndexedMultisetInsert{idx:i, dat:d});
        return i;
    endmethod

    method ActionValue#(IndexedMultisetIndices#(idxT)) insertAndReserve(datT ins, datT res);
        let insIdx <- findIdx(ins,recsRead);
        insertW.wset(IndexedMultisetInsert{idx:insIdx, dat:ins});
        let newRecs = recsRead;
        newRecs[insIdx].data = ins;
        let resIdx <- findIdx(res,newRecs);
        reserveW.wset(IndexedMultisetInsert{idx:resIdx, dat:res});
        return IndexedMultisetIndices{inserted: insIdx, reserved: resIdx};
    endmethod

    method datT lookup(idxT i) = records[i].data;

    interface rPort = removes;

    method Action clearAll = clearW.send;
endmodule

// An implementation of Indexed Multiset that depends on inserting and removing in the same order.
module mkIndexedMultisetQueue(IndexedMultiset#(Bit#(idxTSz), datT, remWidth))
    provisos (
        Alias#(idxT, Bit#(idxTSz)),
        Bits#(datT, _datsz),
        Eq#(datT)
    );
    Vector#(TExp#(idxTSz), Reg#(datT)) records <- replicateM(mkRegU);
    Vector#(TExp#(idxTSz), datT) recsRead = readVReg(records);

    Reg#(Bit#(TAdd#(idxTSz,1))) lhead <- mkReg(0);
    Reg#(Bit#(TAdd#(idxTSz,1))) ltail <- mkReg(0);
    idxT head = truncate(lhead);
    Bit#(TAdd#(idxTSz,1)) level = lhead - ltail;
    Bool empty = (level==0);
    Bool full  = (level==fromInteger(valueOf(TExp#(idxTSz))));
    Bool almostFull = (level>=fromInteger(valueOf(TExp#(idxTSz)))-1);

    RWire#(datT) insertW <- mkRWire;
    RWire#(datT) reserveW <- mkRWire;
    Vector#(remWidth, RWire#(idxT)) removeW <- replicateM(mkRWire);
    PulseWire clearW <- mkPulseWire;

    function Bit#(TAdd#(idxTSz,1)) updateWidePointer(Bit#(TAdd#(idxTSz,1)) lIdx, idxT idx);
      idxT diff = idx - truncate(lIdx);
      return lIdx + zeroExtend(diff);
    endfunction
    (* no_implicit_conditions *)
    rule canon;
        Vector#(TExp#(idxTSz),datT) recs = recsRead;
        Bit#(TAdd#(idxTSz,1)) nlhead = lhead;
        Bit#(TAdd#(idxTSz,1)) nltail = ltail;
        if (insertW.wget matches tagged Valid .d) begin
            idxT i = truncate(nlhead);
            recs[i] = d;
            nlhead = nlhead + 1;
        end
        if (reserveW.wget matches tagged Valid .d) begin
            idxT i = truncate(nlhead);
            recs[i] = d;
        end
        for (Integer i = 0; i < valueOf(remWidth); i = i + 1)
            if (removeW[i].wget matches tagged Valid .r)
                nltail = updateWidePointer(nltail, r);
        if (clearW) begin
            nlhead = 0;
            nltail = 0;
        end
        lhead <= nlhead;
        ltail <= nltail;
        writeVReg(records, recs);
    endrule

    Vector#(remWidth, IndexedMultisetRemoveIfc#(idxT)) removes;
    for (Integer i = 0; i < valueOf(remWidth); i = i + 1) begin
        removes[i] = interface IndexedMultisetRemoveIfc#(idxT);
            method Action remove(idxT idx) = removeW[i].wset(idx);
        endinterface;
    end

    method ActionValue#(idxT) insert(datT d) if (!full);
        Bool next = (recsRead[head-1]!=d);
        if (next) insertW.wset(d);
        return (next) ? head:head-1;
    endmethod
    // Reserved data values must be the same as the next inserted value or they will be overwritten.
    method ActionValue#(IndexedMultisetIndices#(idxT)) insertAndReserve(datT ins, datT res) if (!almostFull);
        idxT insIdx = head - 1; // Default, assuming a match.
        idxT resIdx = head - 1; // Default, assuming a match.
        if (recsRead[head - 1]!=ins || empty) begin
            insIdx = head;
            insertW.wset(ins); // Increment head.
            if (res!=ins) begin
                resIdx = head + 1;
                reserveW.wset(res); // Increment head again!
            end else resIdx = head;
        end else if (recsRead[head - 1]!=res) begin
            resIdx = head;
            reserveW.wset(res);
        end
        return IndexedMultisetIndices{inserted: insIdx, reserved: resIdx};
    endmethod

    method datT lookup(idxT i) = recsRead[i];

    interface rPort = removes;

    method Action clearAll = clearW.send;
endmodule
