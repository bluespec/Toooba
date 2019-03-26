
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

// Correct use of the register file implies that the same index can't be used for simultaneous read and write from different rules. If different indices are used reads and writes are conflict free. If the reads and writes are in the same rule, write updates the file at the end of the rule.
// We have imitated this conflict free behavior using config regs.
// If we had used ordinary registers, then read<write
// In many designs where we needed Bypass register file, the bypassing was implemented outside the register file, explicitly.


import Types::*;
import ProcTypes::*;
import Vector::*;
import Ehr::*;
import ConfigReg::*;

interface RFileWr;
    method Action wr( PhyRIndx rindx, Data data );
endinterface

interface RFileRd;
    method Data rd1( PhyRIndx rindx );
    method Data rd2( PhyRIndx rindx );
    method Data rd3( PhyRIndx rindx );
endinterface

interface RFile#(numeric type wrNum, numeric type rdNum);
    interface Vector#(wrNum, RFileWr) write;
    interface Vector#(rdNum, RFileRd) read;
endinterface


// lazy: read EHR port 0 of the regfile
// this must be used together with lazy reservation station
module mkRFile#(Bool lazy)( RFile#(wrNum, rdNum) ) provisos (
    NumAlias#(ehrPortNum, TAdd#(wrNum, 1)) // wr [< rd] (only in case lazy = false)
);
    let verbose = False;

    // phy reg init val must be 0: because x0 is renamed to phy reg 0,
    // which must be 0 at all time
    Vector#(NumPhyReg, Ehr#(ehrPortNum, Data)) rfile <- replicateM(mkEhr(0));

    Vector#(NumPhyReg, Data) rdData = ?;
    if(lazy) begin
        // if being lazy, just return port 0 for read
        Vector#(NumPhyReg, Wire#(Data)) rdWire <- replicateM(mkBypassWire);
        (* fire_when_enabled, no_implicit_conditions *)
        rule setWire;
            for(Integer i = 0; i < valueof(NumPhyReg); i = i+1) begin
                rdWire[i] <= rfile[i][0];
            end
        endrule
        for(Integer i = 0; i < valueof(NumPhyReg); i = i+1) begin
            rdData[i] = rdWire[i];
        end
    end
    else begin
        Integer rd_port = valueof(wrNum); // read the last ehr port
        for(Integer i = 0; i < valueof(NumPhyReg); i = i+1) begin
            rdData[i] = rfile[i][rd_port];
        end
    end

    function Data getRead(PhyRIndx rindx);
        return rdData[rindx];
    endfunction

    Vector#(wrNum, RFileWr) wrIfc = ?;
    for(Integer i = 0; i < valueof(wrNum); i = i+1) begin
        wrIfc[i] = (interface RFileWr;
            method Action wr( PhyRIndx rindx, Data data );
                if (verbose) $display("[RFile] wr_%d: r %h <= %h", i, rindx, data);
                rfile[rindx][i] <= data;
            endmethod
        endinterface);
    end

    Vector#(rdNum, RFileRd) rdIfc = ?;
    for(Integer i = 0; i < valueof(rdNum); i = i+1) begin
        rdIfc[i] = (interface RFileRd;
            method Data rd1( PhyRIndx rindx ) = getRead(rindx);
            method Data rd2( PhyRIndx rindx ) = getRead(rindx);
            method Data rd3( PhyRIndx rindx ) = getRead(rindx);
        endinterface);
    end

    interface write = wrIfc;
    interface read = rdIfc;
endmodule
