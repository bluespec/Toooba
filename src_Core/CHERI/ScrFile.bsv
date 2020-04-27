
// Copyright (c) 2017 Massachusetts Institute of Technology
// Portions Copyright (c) 2019-2020 Bluespec, Inc.
// CHERI modifications:
//     Copyright (c) 2020 Jonathan Woodruff
//     All rights reserved.
//
//     This software was developed by SRI International and the University of
//     Cambridge Computer Laboratory (Department of Computer Science and
//     Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
//     DARPA SSITH research programme.
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
import DefaultValue::*;
import ConfigReg::*;
import Ehr::*;
import GetPut::*;
import Vector::*;
import CHERICap::*;
import CHERICC_Fat::*;
import ISA_Decls_CHERI::*;

// ================================================================
// BSV additional libs

import Cur_Cycle::*;

// ================================================================
// Project imports from Toooba

import SoC_Map::*;

// ================================================================
// Information returned on traps and mret/sret/uret

typedef struct {
    CapMem new_pcc;
  } Scr_Trap_Updates
deriving (Bits, FShow);

typedef struct {
    CapMem new_pcc;
  } Scr_RET_Updates
deriving (Bits, FShow);

typedef struct {
    Addr top;
    Addr base;
    HardPerms perms;
    } ScrVMInfo
deriving (Bits, FShow);

typedef struct {
    Bool cap_mode;
    } ScrDecodeInfo
deriving (Bits, FShow);

// ================================================================

interface ScrFile;
    // Read
    method CapReg rd(SCR csr);
    // normal write by RWSpecialCap inst to any SCR
    method Action scrInstWr(SCR csr, CapReg x);

    // The WARL transform performed during CSRRx writes to a CSR
    method CapReg warl_xform (SCR csr, CapReg x);

    // Methods for handling traps
    method ActionValue#(Scr_Trap_Updates) trap(CapPipe pc, Bit#(2) prv);
    method ActionValue#(Scr_RET_Updates) sret;
    method ActionValue#(Scr_RET_Updates) mret;

    // Outputs for CSRs that the rest of the processor needs to know about
    method ScrVMInfo ddcCheck;

    // terminate
    method ActionValue#(void) terminate;

endinterface

// same as EHR except that read port 0 is not ordered with other methods. Read
// port 1 will still get bypassing from write port 0.
module mkConfigEhr#(t init)(Ehr#(n, t)) provisos(Bits#(t, w));
    Ehr#(n, t) data <- mkEhr(init);
    Wire#(t) read <- mkBypassWire;

    (* fire_when_enabled, no_implicit_conditions *)
    rule setRead;
        read <= data[0];
    endrule

    Ehr#(n, t) ifc = ?;
    ifc[0] = (interface Reg;
        method _read = read._read;
        method _write = data[0]._write;
    endinterface);
    for(Integer i = 1; i < valueOf(n); i = i+1) begin
        ifc[i] = (interface Reg;
            method _read = data[i]._read;
            method _write = data[i]._write;
        endinterface);
    end
    return ifc;
endmodule

module mkScrFile (ScrFile);
    RiscVISASubset isa = defaultValue;

    let mkCsrReg = mkConfigReg;
    let mkCsrEhr = mkConfigEhr;

    // User level SCRs
    // PCC is implemented in the pipeline.
    Reg#(CapReg) ddc_reg          <- mkCsrReg(defaultValue);

    // User level SCRs with accessSysRegs
    // Reg#(CapReg) utcc_reg      <- mkCsrReg(defaultValue);
    // Reg#(CapReg) utdc_reg      <- mkCsrReg(nullCap);
    // Reg#(CapReg) uScratchC_reg <- mkCsrReg(nullCap);
    // Reg#(CapReg) uepcc_reg     <- mkCsrReg(defaultValue);

    // System level SCRs with accessSysRegs
    Reg#(CapReg) stcc_reg      <- mkCsrReg(defaultValue);
    Reg#(CapReg) stdc_reg      <- mkCsrReg(nullCap);
    Reg#(CapReg) sScratchC_reg <- mkCsrReg(nullCap);
    Reg#(CapReg) sepcc_reg     <- mkCsrReg(defaultValue);

    // Machine level SCRs with accessSysRegs
    Reg#(CapReg) mtcc_reg      <- mkCsrReg(defaultValue);
    Reg#(CapReg) mtdc_reg      <- mkCsrReg(nullCap);
    Reg#(CapReg) mScratchC_reg <- mkCsrReg(nullCap);
    Ehr#(2, CapReg) mepcc_reg  <- mkConfigEhr(defaultValue);

    // Function for getting a csr given an index
    function Reg#(CapReg) get_scr(SCR scr);
        return (case (scr)
            // User SCRs
            //SCR_PCC:       $error("PCC does not actually live in the Special Capability Register File!");
            SCR_DDC:       ddc_reg;
            // User CSRs with accessSysRegs
            // SCR_UTCC:      utcc_reg;
            // SCR_UTDC:      utdc_reg;
            // SCR_UScratchC: uScratchC_reg;
            // SCR_UEPCC:     uepcc_reg;
            // System CSRs with accessSysRegs
            SCR_STCC:      stcc_reg;
            SCR_STDC:      stdc_reg;
            SCR_SScratchC: sScratchC_reg;
            SCR_SEPCC:     sepcc_reg;
            // Machine CSRs with accessSysRegs
            SCR_MTCC:      mtcc_reg;
            SCR_MTDC:      mtdc_reg;
            SCR_MScratchC: mScratchC_reg;
            SCR_MEPCC:     mepcc_reg[1];
        endcase);
    endfunction

   // ================================================================
   // INTERFACE

    method CapReg rd(SCR scr);
        return get_scr(scr)._read;
    endmethod

    method Action scrInstWr(SCR csr, CapReg x);
        get_scr(csr)._write(x);
    endmethod

    method ActionValue#(Scr_Trap_Updates) trap(CapPipe pc, Bit#(2) prv);
        mepcc_reg[0] <= cast(pc);
        return Scr_Trap_Updates{new_pcc: cast(mtcc_reg)};
    endmethod

    method ActionValue#(Scr_RET_Updates) mret;
        return Scr_RET_Updates{new_pcc: cast(mepcc_reg[0])};
    endmethod

    method ActionValue#(Scr_RET_Updates) sret;
        return Scr_RET_Updates{new_pcc: cast(mepcc_reg[0])};
    endmethod

    method ScrVMInfo ddcCheck;
        // for load/store, need to consider MPRV
        return ScrVMInfo {
            top: truncate(getTop(ddc_reg)),
            base: truncate(getBase(ddc_reg)),
            perms: getHardPerms(ddc_reg)
        };
    endmethod

endmodule
