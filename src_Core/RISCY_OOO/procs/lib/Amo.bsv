// Copyright (c) 2017 Massachusetts Institute of Technology
//
//-
// RVFI_DII + CHERI modifications:
//     Copyright (c) 2020 Alexandre Joannou
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
import Vector::*;

(* noinline *)
function MemTaggedData amoExec( AmoInst amo_inst, Bit#(2) wordIdx
                              , MemTaggedData current, MemTaggedData inpt );

  Vector#(2, Bit#(64)) dwordOld = current.data;
  Vector#(4, Bit#(32))  wordOld = unpack(pack(current.data));

  function doOp (vOld, vIn) = case (amo_inst.func)
    Swap: return vIn;
    Add:  return vOld + vIn;
    Xor:  return vOld ^ vIn;
    And:  return vOld & vIn;
    Or:   return vOld | vIn;
    Min:  return sMin(vOld, vIn);
    Max:  return sMax(vOld, vIn);
    Minu: return uMin(vOld, vIn);
    Maxu: return uMax(vOld, vIn);
  endcase;

  Bit#(128) tmpData = pack(doOp(pack(current.data), pack(inpt.data)));
  Bit#(128)    mask = ~0;
  Bit#(8)  shftAmnt = 0;
  case (amo_inst.width)
    DWord: begin
      tmpData  = zeroExtend(doOp(dwordOld[wordIdx[1]], truncate(pack(inpt.data))));
      mask     = zeroExtend(64'hffffffffffffffff);
      shftAmnt = zeroExtend(wordIdx[1]) << 6;
    end
    Word: begin
      tmpData  = zeroExtend(doOp(wordOld[wordIdx], truncate(pack(inpt.data))));
      mask     = zeroExtend(32'hffffffff);
      shftAmnt = zeroExtend(wordIdx) << 5;
    end
  endcase
  Bit#(128) newData = (pack(current.data) & ~(mask << shftAmnt)) | (tmpData << shftAmnt);

  Bool newTag = (amo_inst.func == Swap && amo_inst.width == QWord) ? inpt.tag : False;
  return MemTaggedData { tag: newTag, data: unpack(newData) };
endfunction

function Bit#(t) sMax( Bit#(t) a, Bit#(t) b );
  Int#(t) x = max(unpack(a), unpack(b));
  return pack(x);
endfunction
function Bit#(t) sMin( Bit#(t) a, Bit#(t) b );
  Int#(t) x = min(unpack(a), unpack(b));
  return pack(x);
endfunction
function Bit#(t) uMax( Bit#(t) a, Bit#(t) b );
  UInt#(t) x = max(unpack(a), unpack(b));
  return pack(x);
endfunction
function Bit#(t) uMin( Bit#(t) a, Bit#(t) b );
  UInt#(t) x = min(unpack(a), unpack(b));
  return pack(x);
endfunction
