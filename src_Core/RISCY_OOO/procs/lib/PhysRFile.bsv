
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

// Correct use of the register file implies that the same index can't be used for simultaneous read and write from different rules. If different indices are used reads and writes are conflict free. If the reads and writes are in the same rule, write updates the file at the end of the rule.
// We have imitated this conflict free behavior using config regs.
// If we had used ordinary registers, then read<write
// In many designs where we needed Bypass register file, the bypassing was implemented outside the register file, explicitly.


import Types::*;
import ProcTypes::*;
import Vector::*;
import Ehr::*;
import ConfigReg::*;
import SpecialRegs::*;

interface RFileWr#(type d);
    method Action wr( PhyRIndx rindx, d data );
endinterface

interface RFileRd#(type d);
    method d rd1( PhyRIndx rindx );
    method d rd2( PhyRIndx rindx );
    method d rd3( PhyRIndx rindx );
endinterface

interface RFile#(numeric type wrNum, numeric type rdNum, type d);
    interface Vector#(wrNum, RFileWr#(d)) write;
    interface Vector#(rdNum, RFileRd#(d)) read;
endinterface


// lazy: read EHR port 0 of the regfile
// this must be used together with lazy reservation station
module mkRFile#(d defaultRegisterValue, Bool lazy)( RFile#(wrNum, rdNum, d) ) provisos (
    NumAlias#(ehrPortNum, TAdd#(wrNum, 1)), Bits#(d, d_Size) // wr [< rd] (only in case lazy = false)
);
    let verbose = False;

    // phy reg init val must be 0: because x0 is renamed to phy reg 0,
    // which must be 0 at all time
    // Using a mkRegOR here assumes there will be a single write per register per cycle.
    // As each register is allocated to a single instruction which will execute once, this should always be true.
    Vector#(NumPhyReg, Vector#(ehrPortNum, Reg#(d))) rfile <- replicateM(mkRegOR(defaultRegisterValue));

    Vector#(NumPhyReg, d) rdData = ?;
    if(lazy) begin
        // if being lazy, just return port 0 for read
        Vector#(NumPhyReg, Wire#(d)) rdWire <- replicateM(mkBypassWire);
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

    function d getRead(PhyRIndx rindx);
        return rdData[rindx];
    endfunction

    Vector#(wrNum, RFileWr#(d)) wrIfc = ?;
    for(Integer i = 0; i < valueof(wrNum); i = i+1) begin
        wrIfc[i] = (interface RFileWr;
            method Action wr( PhyRIndx rindx, d data );
                if (verbose) $display("[RFile] wr_%d: r %h <= %h", i, rindx, data);
                rfile[rindx][i] <= data;
            endmethod
        endinterface);
    end

    Vector#(rdNum, RFileRd#(d)) rdIfc = ?;
    for(Integer i = 0; i < valueof(rdNum); i = i+1) begin
        rdIfc[i] = (interface RFileRd;
            method d rd1( PhyRIndx rindx ) = getRead(rindx);
            method d rd2( PhyRIndx rindx ) = getRead(rindx);
            method d rd3( PhyRIndx rindx ) = getRead(rindx);
        endinterface);
    end

    interface write = wrIfc;
    interface read = rdIfc;
endmodule
