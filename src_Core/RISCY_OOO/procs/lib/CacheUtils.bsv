
// Copyright (c) 2017 Massachusetts Institute of Technology
//
//-
// RVFI_DII + CHERI modifications:
//     Copyright (c) 2020 Alexandre Joannou
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

`include "ProcConfig.bsv"
import BRAM::*;
import Types::*;
import MemoryTypes::*;
import GetPut::*;
import ClientServer::*;
import Connectable::*;
import Vector::*;
import Fifos::*;
import Ehr::*;
import FIFO::*;
import FIFOF::*;
import Performance::*;
import FShow::*;
import MsgFifo::*;

// 64B cache line
typedef 4 CLineNumMemTaggedData;
typedef TMul#(CLineNumMemTaggedData, 2) CLineNumData;
typedef TLog#(CLineNumMemTaggedData) LogCLineNumMemTaggedData;
typedef TLog#(CLineNumData) LogCLineNumData;
typedef Bit#(LogCLineNumData) CLineDataSel;
typedef Bit#(LogCLineNumMemTaggedData) CLineMemTaggedDataSel;
function CLineMemTaggedDataSel getCLineMemTaggedDataSel(Addr a) =
  truncate(a >> valueOf(TLog#(MemDataBytes)));
function CLineDataSel getCLineDataSel(Addr a) =
  truncate(a >> valueOf(TLog#(DataBytes)));
typedef struct {
  Vector#(CLineNumMemTaggedData, MemTag) tag;
  Vector#(CLineNumMemTaggedData, MemData) data;
} CLine deriving (Bits, Eq, FShow);
function Vector#(CLineNumMemTaggedData, MemTaggedData) clineToMemTaggedDataVector(CLine line);
  function f(x,y) = MemTaggedData{tag: x, data: y};
  return zipWith(f, line.tag, line.data);
endfunction
function CLine memTaggedDataVectorToCline(Vector#(CLineNumMemTaggedData, MemTaggedData) line) =
  CLine{tag: map(getTag, line), data: map(getData, line)};
function Data getDataAt(CLine line, CLineDataSel sel);
  Vector#(CLineNumData, Data) data = unpack(pack(line.data));
  return data[sel];
endfunction
function CLine setDataAt(CLine line, CLineDataSel sel, Data data);
  Vector#(CLineNumData, Data) newData = unpack(pack(line.data));
  newData[sel] = data;
  CLineMemTaggedDataSel bigSel = truncateLSB(sel);
  let newLine = line;
  newLine.tag[bigSel] = False;
  newLine.data = unpack(pack(newData));
  return newLine;
endfunction
function CLine setDataAtBE(CLine line, CLineDataSel sel, Data data, ByteEn be);
  let oldData = getDataAt(line, sel);
  return setDataAt(line, sel, mergeDataBE(oldData, data, be));
endfunction
function MemTaggedData getTaggedDataAt(CLine line, CLineMemTaggedDataSel sel) =
  MemTaggedData { tag: line.tag[sel], data: line.data[sel] };
function MemTaggedData getTagsAt(CLine line) =
  MemTaggedData { tag: False, data: cons(zeroExtend(pack(line.tag)), unpack(0)) };
function CLine setTaggedDataAt(CLine line, CLineMemTaggedDataSel sel, MemTaggedData data);
  let newLine = line;
  newLine.tag[sel] = data.tag;
  newLine.data[sel] = data.data;
  return newLine;
endfunction
typedef TMul#(CLineNumMemTaggedData, MemDataSz) CLineDataSz;
typedef TDiv#(CLineDataSz, 8) CLineDataNumBytes;
typedef TLog#(CLineDataNumBytes) LogCLineDataNumBytes;

typedef TMul#(CLineNumMemTaggedData, MemDataSz) CLineMemDataSz;
typedef TMul#(CLineNumMemTaggedData, MemDataBytes) CLineNumMemDataBytes;
typedef TLog#(CLineNumMemDataBytes) LogCLineNumMemDataBytes;
typedef Vector#(CLineNumMemTaggedData, Vector#(MemDataBytes, Bool)) CLineMemDataByteEn;
typedef Vector#(CLineNumData, Vector#(DataBytes, Bool)) CLineDataByteEn;

function Bool isCLineAlignAddr(Addr a);
    Bit#(LogCLineNumMemDataBytes) offset = truncate(a);
    return offset == 0;
endfunction

// cache line addr (drop the offset within cache line)
typedef TSub#(AddrSz, LogCLineNumMemDataBytes) CLineAddrSz;
typedef Bit#(CLineAddrSz) CLineAddr;

// cache line v.s. instruction
typedef TDiv#(CLineMemDataSz, InstSz) CLineNumInst;
typedef Bit#(TLog#(CLineNumInst)) CLineInstSel;

function CLineInstSel getCLineInstSel(Addr a);
    return truncate(a >> valueof(TLog#(TDiv#(InstSz, 8))));
endfunction

// FIFO enq/deq ifc
interface FifoEnq#(type t);
    method Bool notFull;
    method Action enq(t x);
endinterface

function FifoEnq#(t) toFifoEnq(Fifo#(n, t) f);
    return (interface FifoEnq;
        method notFull = f.notFull;
        method enq = f.enq;
    endinterface);
endfunction

function FifoEnq#(t) nullFifoEnq;
    return (interface FifoEnq;
        method Bool notFull = True;
        method Action enq(t x);
            noAction;
        endmethod
    endinterface);
endfunction

instance ToPut#(FifoEnq#(t), t);
    function Put#(t) toPut(FifoEnq#(t) ifc);
        return (interface Put;
            method Action put(t x);
                ifc.enq(x);
            endmethod
        endinterface);
    endfunction
endinstance

interface FifoDeq#(type t);
    method Bool notEmpty;
    method Action deq;
    method t first;
endinterface

function FifoDeq#(t) toFifoDeq(Fifo#(n, t) f);
    return (interface FifoDeq;
        method notEmpty = f.notEmpty;
        method deq = f.deq;
        method first = f.first;
    endinterface);
endfunction

function FifoDeq#(t) nullFifoDeq;
    return (interface FifoDeq;
        method Bool notEmpty = False;
        method Action deq if(False);
            noAction;
        endmethod
        method t first if(False);
            return ?;
        endmethod
    endinterface);
endfunction

instance ToGet#(FifoDeq#(t), t);
    function Get#(t) toGet(FifoDeq#(t) ifc);
        return (interface Get;
            method ActionValue#(t) get;
                ifc.deq;
                return ifc.first;
            endmethod
        endinterface);
    endfunction
endinstance

instance Connectable#(FifoEnq#(t), FifoDeq#(t));
    module mkConnection#(FifoEnq#(t) enq, FifoDeq#(t) deq)(Empty);
        mkConnection(toGet(deq), toPut(enq));
    endmodule
endinstance

instance Connectable#(FifoDeq#(t), FifoEnq#(t));
    module mkConnection#(FifoDeq#(t) deq, FifoEnq#(t) enq)(Empty);
        mkConnection(toGet(deq), toPut(enq));
    endmodule
endinstance
