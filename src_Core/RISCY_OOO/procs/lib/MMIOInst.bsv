
// Copyright (c) 2018 Massachusetts Institute of Technology
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

import Vector::*;
import ConfigReg::*;
import Fifos::*;
import Types::*;
import ProcTypes::*;
import CCTypes::*;
import CacheUtils::*;
import MMIOAddrs::*;

import SoC_Map :: *;    // Bluespec setup

interface MMIOInstToCore;
    interface FifoDeq#(Tuple2#(Addr, SupWayX2Sel)) instReq;
    interface FifoEnq#(Vector#(SupSizeX2, Maybe#(Instruction16))) instResp;
    method Action setHtifAddrs(Addr toHost, Addr fromHost);
endinterface

typedef enum {
    MainMem,
    IODevice,    // BootRom, Flash, ...    (Bluespec setup)
    Fault
} InstFetchTarget deriving(Bits, Eq, FShow);

interface MMIOInst;
    // Figure out where to fetch the inst, i.e., from boot rom or main memory.
    method InstFetchTarget getFetchTarget(Addr phyPc);
    // When req boot rom, need to specify the number of instructions to fetch,
    // i.e., maxWay + 1
    method Action bootRomReq(Addr phyPc, SupWayX2Sel maxWay);
    // The return type is same as I$. An entry is Invalid if it is an access
    // fault or not requested before.
    method ActionValue#(Vector#(SupSizeX2, Maybe#(Instruction16))) bootRomResp;
    interface MMIOInstToCore toCore;
endinterface

(* synthesize *)
module mkMMIOInst(MMIOInst);
    // record tohost/fromhost addrs to check inst access fault. Since this
    // MMIOInst module will be placed inside the fetch stage and we want to
    // synthesize fetch stage, we need to add these two regs instead of passing
    // in two reg interfaces.
    Reg#(DataAlignedAddr) toHostAddr <- mkConfigReg(0);
    Reg#(DataAlignedAddr) fromHostAddr <- mkConfigReg(0);
    // MMIO requests are handled in a very slow manner at platform, so no need
    // to use large FIFO here
    Fifo#(1, Tuple2#(Addr, SupWayX2Sel)) reqQ <- mkCFFifo;
    Fifo#(1, Vector#(SupSizeX2, Maybe#(Instruction16))) respQ <- mkCFFifo;
    // To prevent inst fetch requests from clogging the network, we limit to at
    // most 1 pending req. The resp for the pending req will be buffered in
    // respQ, no affecting other MMIO accesses.
    Fifo#(1, void) pendQ <- mkCFFifo;

    SoC_Map_IFC soc_map <- mkSoC_Map;    // Bluespec setup

    method InstFetchTarget getFetchTarget(Addr phyPc);
        let addr = getDataAlignedAddr(phyPc);
        if (soc_map.m_is_IO_addr (phyPc, True)) begin
            return IODevice;
        end
        else if(addr >= mainMemBaseAddr && (addr < mainMemBoundAddr) &&
                addr != toHostAddr && addr != fromHostAddr) begin
            return MainMem;
        end
        else begin
            return Fault;
        end
    endmethod

    method Action bootRomReq(Addr phyPc, SupWayX2Sel maxWay);
        reqQ.enq(tuple2(phyPc, maxWay));
        pendQ.enq(?);
    endmethod

    method ActionValue#(Vector#(SupSizeX2, Maybe#(Instruction16))) bootRomResp;
        pendQ.deq;
        respQ.deq;
        return respQ.first;
    endmethod

    interface MMIOInstToCore toCore;
        interface instReq = toFifoDeq(reqQ);
        interface instResp = toFifoEnq(respQ);
        method Action setHtifAddrs(Addr toHost, Addr fromHost);
            toHostAddr <= getDataAlignedAddr(toHost);
            fromHostAddr <= getDataAlignedAddr(fromHost);
        endmethod
    endinterface
endmodule
