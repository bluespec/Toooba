// Copyright (c) 2019 Bluespec, Inc.  All Rights Reserved
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

package PLIC_16_CoreNumX2_7;

// ================================================================
// Instantiation of parameterized PLIC to specific parameter values.
//
// ================================================================
// Bluespec lib imports

// None

// ----------------
// BSV additional libs

// None

// ================================================================
// Project imports

import ProcTypes :: *;  // For CoreNum

import SoC_Map :: *;    // For N_External_Interrupt_Sources
import PLIC    :: *;    // For PLIC_IFC, mkPLIC

// ================================================================
// PLIC for this core

typedef  TMul #(CoreNum, 2)  PLIC_N_Targets;
typedef  7                   PLIC_Max_Priority;

typedef  PLIC_IFC #(N_External_Interrupt_Sources,
		    PLIC_N_Targets,
		    PLIC_Max_Priority)             PLIC_IFC_16_CoreNumX2_7;

(* synthesize *)
module mkPLIC_16_CoreNumX2_7 (PLIC_IFC_16_CoreNumX2_7);
   let m <- mkPLIC;
   return m;
endmodule

// ================================================================

endpackage
