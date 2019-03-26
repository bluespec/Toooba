
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
import RegFile::*;
import Vector::*;
import Ehr::*;

interface RAS;
    method Addr first;
    // first pop, then push
    method Action popPush(Bool pop, Maybe#(Addr) pushAddr);
endinterface

interface ReturnAddrStack;
    interface Vector#(SupSize, RAS) ras;
    method Action flush;
    method Bool flush_done;
endinterface

// Local RAS Typedefs SHOULD BE A POWER OF TWO.
typedef 8 RasEntries;
typedef Bit#(TLog#(RasEntries)) RasIndex;

(* synthesize *)
module mkRas(ReturnAddrStack) provisos(NumAlias#(TExp#(TLog#(RasEntries)), RasEntries));
    Vector#(RasEntries, Ehr#(TAdd#(SupSize, 1), Addr)) stack <- replicateM(mkEhr(0));
    // head points past valid data
    // to gracefully overflow, head is allowed to overflow to 0 and overwrite the oldest data
    Ehr#(TAdd#(SupSize, 1), RasIndex) head <- mkEhr(0);

`ifdef SECURITY
    Reg#(Bool) flushDone <- mkReg(True);

    rule doFlush(!flushDone);
        writeVReg(getVEhrPort(stack, valueof(SupSize)), replicate(0));
        head[valueof(SupSize)] <= 0;
        flushDone <= True;
    endrule
`endif

    Vector#(SupSize, RAS) rasIfc;
    for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
        rasIfc[i] = (interface RAS;
            method Addr first;
                return stack[head[i]][i];
            endmethod
            method Action popPush(Bool pop, Maybe#(Addr) pushAddr);
                // first pop, then push
                RasIndex h = head[i];
                if(pop) begin
                    h = h - 1;
                end
                if(pushAddr matches tagged Valid .addr) begin
                    h = h + 1;
                    stack[h][i] <= addr;
                end
                head[i] <= h;
            endmethod
        endinterface);
    end

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
