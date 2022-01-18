
// Copyright (c) 2017 Massachusetts Institute of Technology
//
//-
// RVFI_DII + CHERI modifications:
//     Copyright (c) 2020 Peter Rugg
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
import Vector::*;
import CHERICC_Fat::*;
import CHERICap::*;

(* noinline *)
function Maybe#(CapMem) decodeBrPred( CapMem pc, DecodedInst dInst, Bool histTaken, Bool is_32b_inst);
  CapMem pcPlusN = addPc(pc, (is_32b_inst ? 4 : 2));
  Data imm_val = truncate(fromMaybe(?, getDInstImm(dInst)));
  Maybe#(CapMem) nextPc = tagged Invalid;
  CapPipe pcPipe = cast(pc);
  CapMem jTarget = cast(incOffset(pcPipe, imm_val).value);
  if( dInst.iType == J || dInst.iType == CJAL ) begin
    nextPc = tagged Valid jTarget;
  end else if( dInst.iType == Br ) begin
    if( histTaken ) begin
      nextPc = tagged Valid jTarget;
    end else begin
      nextPc = tagged Valid pcPlusN;
    end
  end else if( dInst.iType == Jr || dInst.iType == CCall || dInst.iType == CJALR ) begin
    // target is unknown until RegFetch
    nextPc = tagged Invalid;
  end else begin
    nextPc = tagged Valid pcPlusN;
  end
  return nextPc;
endfunction

// general types for direction predictor

// Function to offset PC by the probable size of an instruction without a full add delay.
function Addr offsetPc(Addr pc, Integer i) = {truncateLSB(pc), pc[7:0] + (fromInteger(i)*4)};

typedef struct {
    Bool taken;
    trainInfoT train; // info that a branch must keep for future training
} DirPredResult#(type trainInfoT) deriving(Bits, Eq, FShow);

interface DirPred#(type trainInfoT);
    method ActionValue#(DirPredResult#(trainInfoT)) pred;
endinterface

interface DirPredictor#(type trainInfoT);
    method Action nextPc(Addr nextPc);
    interface Vector#(SupSize, DirPred#(trainInfoT)) pred;
    method Action update(Bool taken, trainInfoT train, Bool mispred);
    method Action flush;
    method Bool flush_done;
endinterface
