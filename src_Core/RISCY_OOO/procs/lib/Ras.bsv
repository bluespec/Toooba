
// Copyright (c) 2017 Massachusetts Institute of Technology
//
//-
// RVFI_DII + CHERI modifications:
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
import Types::*;
import ProcTypes::*;
import RegFile::*;
import Vector::*;
import Ehr::*;
import CHERICC_Fat::*;
import CHERICap::*;

interface RAS;
    method CapMem first;
    method ActionValue#(RasIndex) pop(Bool doPop);
endinterface

// Local RAS Typedefs SHOULD BE A POWER OF TWO.
typedef 2 NUM_RAS_STACK_IFCS;
typedef 3 NUM_RAS_VALIDS_IFCS;
typedef 16 RasEntries;
typedef Bit#(TLog#(RasEntries)) RasIndex;
typedef RasIndex RasPredTrainInfo;

interface ReturnAddrStack;
    interface Vector#(SupSize, RAS) ras;
    method Bool pendingPush;
    method Action push(CapMem pushAddr);
    method Action write(CapMem pushAddr, RasIndex h);
    method Action setHead(RasIndex h);
    method Action flush;
    method Bool flush_done;
endinterface

(* synthesize *)
module mkRas(ReturnAddrStack) provisos(NumAlias#(TExp#(TLog#(RasEntries)), RasEntries));
    Vector#(RasEntries, Ehr#(NUM_RAS_STACK_IFCS, CapMem)) stack <- replicateM(mkEhr(nullCap));
    Vector#(RasEntries, Ehr#(NUM_RAS_VALIDS_IFCS, Bool)) valids <- replicateM(mkEhr(False));
    // head points past valid data
    // to gracefully overflow, head is allowed to overflow to 0 and overwrite the oldest data
    Ehr#(TAdd#(SupSize, 3), RasIndex) head <- mkEhr(0);

    Bool invalidHead = !(valids[head[0]][0]);
    Reg#(Bit#(6)) delay <- mkReg(0);
    rule resetValidHead;
        if (delay < 32 && invalidHead) delay <= delay + 1;
        else begin
            valids[head[0]][2] <= True;
            delay <= 0;
        end
    endrule
`ifdef SECURITY
    Reg#(Bool) flushDone <- mkReg(True);

    rule doFlush(!flushDone);
        writeVReg(getVEhrPort(stack, 0), replicate(0));
        head[valueof(SupSize) + 1] <= 0;
        flushDone <= True;
    endrule
`endif

    Vector#(SupSize, RAS) rasIfc;
    for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
        rasIfc[i] = (interface RAS;
            // first should use head[i], but this is better for timing.
            // head[0] misbehaves with two pops in the same cycle.
            method CapMem first = stack[head[0]][0];
            method ActionValue#(RasIndex) pop(Bool doPop);
                RasIndex h = head[i];
                if (doPop) begin
                    h = h - 1;
                    //$display("RAS pop head<-%d, val:%x", head[i] - 1, stack[head[i]][0]);
                end
                head[i] <= h;
                return h;
            endmethod
        endinterface);
    end

    method Bool pendingPush = invalidHead;

    method Action push(CapMem pushAddr);
        Reg#(RasIndex) h = head[valueof(SupSize) + 2];
        valids[h+1][0] <= False;
`ifndef NO_SPEC_RSB_PUSH
        stack[h+1][1] <= pushAddr;
        //$display("RAS push stack[%d] <- %x", h+1, pushAddr);
`endif
        h <= h+1;
        //$display("RAS head<-%d", h + 1);
    endmethod

    method Action write(CapMem pushAddr, RasIndex h);
        stack[h][1] <= pushAddr;
        valids[h][1] <= True;
        //$display("RAS write stack[%d] <- %x", h, pushAddr);
    endmethod

    method Action setHead(RasIndex h);
        head[valueof(SupSize)] <= h;
        //$display("RAS fixup head<-%d", h);
    endmethod

    interface ras = rasIfc;

`ifdef SECURITY
    method Action flush if(flushDone);
        flushDone <= False;
    endmethod
    method flush_done = flushDone._read;
`else
    method flush = noAction;
    method flush_done = True;
`endif
endmodule
