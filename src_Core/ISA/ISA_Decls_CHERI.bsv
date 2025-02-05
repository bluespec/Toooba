/*
 * Copyright (c) 2020 Peter Rugg
 * Copyright (c) 2020 Jonathan Woodruff
 * All rights reserved.

 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory (Department of Computer Science and
 * Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
 * DARPA SSITH research programme.

 * This work was supported by NCSC programme grant 4212611/RFA 15971 ("SafeBet").
 *
 * @BERI_LICENSE_HEADER_START@
 *
 * Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
 * license agreements.  See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.  BERI licenses this
 * file to you under the BERI Hardware-Software License, Version 1.0 (the
 * "License"); you may not use this file except in compliance with the
 * License.  You may obtain a copy of the License at:
 *
 *   http://www.beri-open-systems.org/legal/license-1-0.txt
 *
 * Unless required by applicable law or agreed to in writing, Work distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations under the License.
 *
 * @BERI_LICENSE_HEADER_END@
 */
import ISA_Decls::*;
import CHERICap::*;
import CHERICC_Fat::*;

typedef TMul#(XLEN, 2) CLEN;

// Exception codes

typedef struct { Bit#(5) code; } CHERIException deriving(Bits, Eq);

`define CHERIException(n, v) CHERIException cheriExc``n = CHERIException { code: v };
`include "CHERIExceptions.bsvi"
`undef CHERIException

instance FShow#(CHERIException);
    function Fmt fshow(CHERIException exc);
        return (case(exc.code)
`define CHERIException(n, v) v: $format(`"``cheriExc``n```");
`include "CHERIExceptions.bsvi"
`undef CHERIException
            default: $format("cheriExcUnknown");
        endcase);
    endfunction
endinstance

typedef struct {
    Bit #(6) cheri_exc_reg;
    CHERIException cheri_exc_code;
} CSR_XCapCause deriving(Bits, Eq, FShow);

CSR_XCapCause noCapCause = CSR_XCapCause {cheri_exc_code: cheriExcNone,
                                          cheri_exc_reg: unpack(0)};

// SCR map

typedef struct { Bit#(5) addr; } SCR deriving(Bits, Eq);

`define SCR(n, v) SCR scrAddr``n = SCR { addr: v };
`include "SCRs.bsvi"
// As with CSRs, SCR that catches all unimplemented SCRs
`SCR(None, 5'd10)
`undef SCR

instance FShow#(SCR);
    function Fmt fshow(SCR scr);
        return (case(scr.addr)
`define SCR(n, v) v: $format(`"``scrAddr``n```");
`include "SCRs.bsvi"
`undef SCR
            default: $format("scrAddrNone");
        endcase);
    endfunction
endinstance

function SCR unpackSCR(Bit#(5) addr);
    return (case(addr)
`define SCR(n, v) v: scrAddr``n;
`include "SCRs.bsvi"
`undef SCR
        default: scrAddrNone;
    endcase);
endfunction

function CapPipe update_scr_via_csr (CapPipe old_scr, WordXL new_csr, Bool allow_sealed);
    let new_scr = setAddr(old_scr, new_csr);
    let ret = new_scr.value;
    if (!new_scr.exact || (getKind(old_scr) != UNSEALED && !allow_sealed)) begin
        ret = setValidCap(ret, False);
    end
    return ret;
endfunction

RegName cCallRD = 31;

// Instruction field encodings

// Top-level opcodes
Opcode   op_cap_Manip = 7'h5b;
//Opcode   op_cap_Mem   = 7'h0b; // Not yet implemented

// ================================================================
// op_cap_Manip opcode subdivision

// f3 selects between immediate and 3-reg instructions
Bit #(3) f3_cap_ThreeOp             = 3'h0;
Bit #(3) f3_cap_CIncOffsetImmediate = 3'h1;
Bit #(3) f3_cap_CSetBoundsImmediate = 3'h2;
// 3'h3-3'h7 unused

// ================================================================
// op_cap_ThreeOp opcode subdivision

// f7 selects between 3-reg operations

// 7'h00 unused
Bit #(7) f7_cap_CSpecialRW      = 7'h01;
// 7'h02-7'h07 unused
Bit #(7) f7_cap_CSetBounds      = 7'h08;
Bit #(7) f7_cap_CSetBoundsExact = 7'h09;
// 7'h0a unused
Bit #(7) f7_cap_CSeal           = 7'h0b;
Bit #(7) f7_cap_CUnseal         = 7'h0c;
Bit #(7) f7_cap_CAndPerm        = 7'h0d;
Bit #(7) f7_cap_CSetFlags       = 7'h0e;
Bit #(7) f7_cap_CSetOffset      = 7'h0f;
Bit #(7) f7_cap_CSetAddr        = 7'h10;
Bit #(7) f7_cap_CIncOffset      = 7'h11;
Bit #(7) f7_cap_CToPtr          = 7'h12;
Bit #(7) f7_cap_CFromPtr        = 7'h13;
Bit #(7) f7_cap_CSub            = 7'h14;
// 7'h15 unused
Bit #(7) f7_cap_CSetHigh        = 7'h16;
// 7'h17-7'h1c unused
Bit #(7) f7_cap_CBuildCap       = 7'h1d;
Bit #(7) f7_cap_CCopyType       = 7'h1e;
Bit #(7) f7_cap_CCSeal          = 7'h1f;
Bit #(7) f7_cap_CTestSubset     = 7'h20;
Bit #(7) f7_cap_CSetEqualExact  = 7'h21;
// 7'h22-7'hfb unused
Bit #(7) f7_cap_Stores          = 7'h7c;
Bit #(7) f7_cap_Loads           = 7'h7d;
Bit #(7) f7_cap_TwoSrc          = 7'h7e;
Bit #(7) f7_cap_TwoOp           = 7'h7f;

// ================================================================
// f7_cap_TwoSrc opcode subdivision

// rd selects between 2-reg operations

// 5'h00 unused
Bit #(5) rd_cap_CCall          = 5'h01;
// 5'h02-5'h1f unused

// ================================================================
// f7_cap_TwoOp opcode subdivision

// f5rs2 selects between 2-reg operations (f5rs2 instead of f5 because f5
//        is already used in RISC-V and is in a different position

Bit #(5) f5rs2_cap_CGetPerm    = 5'h00;
Bit #(5) f5rs2_cap_CGetType    = 5'h01;
Bit #(5) f5rs2_cap_CGetBase    = 5'h02;
Bit #(5) f5rs2_cap_CGetLen     = 5'h03;
Bit #(5) f5rs2_cap_CGetTag     = 5'h04;
Bit #(5) f5rs2_cap_CGetSealed  = 5'h05;
Bit #(5) f5rs2_cap_CGetOffset  = 5'h06;
Bit #(5) f5rs2_cap_CGetFlags   = 5'h07;
Bit #(5) f5rs2_cap_CRRL        = 5'h08;
Bit #(5) f5rs2_cap_CRAM        = 5'h09;
Bit #(5) f5rs2_cap_CMove       = 5'h0a;
Bit #(5) f5rs2_cap_CClearTag   = 5'h0b;
Bit #(5) f5rs2_cap_JALR_CAP    = 5'h0c;
Bit #(5) f5rs2_cap_CClearReg   = 5'h0d;
// 5'h0e unused
Bit #(5) f5rs2_cap_CGetAddr    = 5'h0f;
Bit #(5) f5rs2_cap_CClearFPReg = 5'h10;
Bit #(5) f5rs2_cap_CSealEntry  = 5'h11;
Bit #(5) f5rs2_cap_CLoadTags   = 5'h12;
// 5'h13 unused
Bit #(5) f5rs2_cap_JALR_PCC    = 5'h14;
// 5'h15-5'h16 unused
Bit #(5) f5rs2_cap_CGetHigh    = 5'h17;
// 5'h18-5'h1f unused (5'h1f reserved for 1-reg instructions)

// ================================================================
// f7_cap_{Load, Store} opcode subdivision

MemReqSize cap_mem_SIZE_B = 'h0;
MemReqSize cap_mem_SIZE_H = 'h1;
MemReqSize cap_mem_SIZE_W = 'h2;
MemReqSize cap_mem_SIZE_D = 'h3;
//MemReqSize f5rs2_cap_mem_SIZE_Q = 'h4; //TODO

Bit #(1) cap_mem_ddc = 1'h0;
Bit #(1) cap_mem_cap = 1'h1;

Bit #(1) cap_mem_unsigned = 1'h1;
Bit #(1) cap_mem_signed = 1'h0;

// ================================================================
// Other:

// Region in MISC_MEM for LQ
Bit #(3) f3_LQ = 3'h2;
Bit #(3) f3_SQ = 3'b100;

`ifdef RV64
Bit #(3) w_SIZE_CAP = f3_SQ;
Bit #(3) w_SIZE_MAX = f3_SQ;
`else //RV32
Bit #(3) w_SIZE_CAP = f3_SD;
Bit #(3) w_SIZE_MAX = f3_SD;
`endif

Bit #(3) f3_AMO_CAP = w_SIZE_CAP;

// Special cases of Otypes that are extended to XLEN
Bit #(XLEN) otype_unsealed_ext = -1;
Bit #(XLEN) otype_sentry_ext = -2;
Bit #(XLEN) otype_res0_ext = -3;
Bit #(XLEN) otype_res1_ext = -4;
