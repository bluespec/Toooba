
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

import Vector::*;
import Assert::*;
import ClientServer::*;
import GetPut::*;
import CHERICC_Fat::*;
`ifdef RVFI_DII
import RVFI_DII_Types::*;
`endif

typedef 64 AddrSz;
typedef Bit#(AddrSz) Addr;

typedef 64 DataSz;
typedef Bit#(DataSz) Data;
typedef TDiv#(DataSz, 8) DataBytes;
typedef Vector#(DataBytes, Bool) ByteEn;
typedef struct {
  tag_t tag;
  data_t data;
} TaggedData#(type tag_t, type data_t) deriving (Bits, FShow, Eq);
function tag_t getTag(TaggedData#(tag_t, data_t) td) = td.tag;
function data_t getData(TaggedData#(tag_t, data_t) td) = td.data;
typedef Vector#(2, Data) MemData;
typedef Bool MemTag;
typedef SizeOf#(MemData) MemDataSz;
typedef TDiv#(MemDataSz, 8) MemDataBytes;
typedef SizeOf#(MemTag) MemTagSz;
typedef SizeOf#(MemTaggedData) MemTaggedDataSz;
typedef TaggedData#(MemTag, MemData) MemTaggedData;
instance Literal#(MemTaggedData);
  function fromInteger(i);
    Bit#(MemDataSz) val = fromInteger(i);
    return MemTaggedData { tag: False, data: unpack(val) };
  endfunction
  function inLiteralRange(x, i) = i < 2**valueOf(MemDataSz);
endinstance
function data_res mergeDataBE(data_t0 oldData, data_t1 newData, be_t be)
  provisos( Bits#(data_t0, data_sz), Bits#(data_t1, data_sz), Bits#(be_t, be_sz)
          , Bits#(data_res, data_sz)
          , Mul#(be_sz, 8, data_sz));
  Vector#(be_sz, Bit#(8)) oldVec = unpack(pack(oldData));
  Vector#(be_sz, Bit#(8)) newVec = unpack(pack(newData));
  Vector#(be_sz, Bool) beVec = unpack(pack(be));
  function Bit#(8) getNewByte(Integer i) = beVec[i] ? newVec[i] : oldVec[i];
  Vector#(be_sz, Bit#(8)) finalVec = map(getNewByte, genVector);
  return unpack(pack(finalVec));
endfunction
function MemTaggedData mergeMemTaggedDataBE( MemTaggedData oldItem
                                           , MemTaggedData newItem
                                           , be_t be)
  provisos (Bits#(be_t, MemDataBytes)) =
  MemTaggedData { tag : case (pack(be))
                            0: oldItem.tag;
                            -1: newItem.tag;
                            default: False;
                        endcase
                , data: mergeDataBE(oldItem.data, newItem.data, be)};
function MemData dataToMemData(Data x) = unpack(zeroExtend(x));
function MemDataByteEn dataBEToMemDataBE(ByteEn x) = unpack(zeroExtend(pack(x)));
function Data memDataToData(MemData x) = x[0];
function ByteEn memDataBEToDataBE(MemDataByteEn x) = unpack(truncate(pack(x)));
function MemTaggedData toMemTaggedData(t x)
  provisos (Bits#(t, sz), Add#(sz, smthg, MemDataSz)) = MemTaggedData {
  tag: False,
  data: unpack(zeroExtend(pack(x)))
};
function MemTaggedData dataToMemTaggedData(Data x) = toMemTaggedData(x);
function MemTaggedData toMemTaggedDataSelect(t x, Addr addr)
  provisos (Literal#(t), Bits#(t, sz), Add#(sz, smthg, MemDataSz),
  Mul#(ts_in_memdata, sz, SizeOf#(MemData)),
  Div#(SizeOf#(MemData), sz, ts_in_memdata),
  Add#(TLog#(TDiv#(sz,8)), aligned_addr_sz, SizeOf#(Addr)),
  Add#(a__, TLog#(ts_in_memdata), aligned_addr_sz));

  Bit#(aligned_addr_sz) alignedAddr = truncateLSB(addr);
  Bit#(TLog#(ts_in_memdata)) t_index = truncate(alignedAddr);
  Vector#(ts_in_memdata, t) vec = replicate(0);
  vec[t_index] = x;
  return MemTaggedData {
    tag: False,
    data: unpack(pack(vec))
  };
endfunction
function t fromMemTaggedData(MemTaggedData x)
  provisos (Bits#(t, sz), Add#(sz, smthg, MemDataSz)) =
  unpack(truncate(pack(x.data)));
function Tuple2#(t, be_t) fromMemTaggedDataSelect(MemTaggedData x, Bit#(MemDataBytes) be)
  provisos (Bits#(t, sz), Bits#(be_t, be_t_sz), Mul#(be_t_sz, 8, sz),
            Mul#(be_t_sz, elems, MemDataBytes), Add#(sz, smthg, MemDataSz));
  Vector#(elems,be_t) be_vec = unpack(be);
  Vector#(elems,t) x_vec = unpack(pack(x.data));
  function Bool pred(Tuple2#(t, be_t) el) = pack(tpl_2(el)) != 0;
  return fromMaybe(unpack(0), find (pred, zip(x_vec, be_vec)));
endfunction

typedef 32 InstSz;
typedef Bit#(InstSz) Instruction;

// Compressed instructions (16-bit)
typedef 16 Inst16_Sz;
typedef Bit #(Inst16_Sz) Instruction16;

typedef 0 AsidSz; // not really implement ASID
typedef Bit#(AsidSz) Asid;

typedef TLog#(MemDataBytes) IndxShamt;
typedef Vector#(MemDataBytes, Bool) MemDataByteEn;

typedef union tagged {
  void TagMemAccess;
  MemDataByteEn DataMemAccess;
} ByteOrTagEn deriving (FShow, Eq, Bits);

typedef TDiv#(DataSz, Inst16_Sz) DataSzInst;
typedef TLog#(DataSzInst) LgDataSzInst;
typedef Bit#(LgDataSzInst) DataInstOffset;
typedef TDiv#(MemDataSz, Inst16_Sz) MemDataSzInst;
typedef TLog#(MemDataSzInst) LgMemDataSzInst;
typedef Bit#(LgMemDataSzInst) MemDataInstOffset;

// These types show up in many places so they are defined here
typedef enum {Swap, Add, Xor, And, Or, Min, Max, Minu, Maxu, None} AmoFunc deriving(Bits, Eq, FShow, Bounded);
typedef enum {QWord, DWord, Word} AmoWidth deriving(Bits, Eq, FShow, Bounded);
typedef enum { Ld, St, Lr, Sc, Amo, Fence } MemFunc deriving(Bits, Eq, FShow);
typedef struct {
  AmoFunc  func;
  AmoWidth width;
  Bool     aq;
  Bool     rl;
} AmoInst deriving(Bits, Eq, FShow);
typedef struct {
    MemFunc        mem_func; // Ld, St, Lr, Sc, Amo
    AmoFunc        amo_func;
    Bool           unsignedLd;
    ByteOrTagEn    byteOrTagEn; // takes place of double word
    Bool           aq;
    Bool           rl;
    Bool           reg_bounds;
} MemInst deriving(Bits, Eq, FShow);

`ifdef BSIM
function Action doAssert(Bool b, String s) = action if(!b) $fdisplay(stderr, "\n%m: ASSERT FAIL!!"); dynamicAssert(b, s); endaction;
`else
function Action doAssert(Bool b, String s) = dynamicAssert(b, s);
`endif
