// Copyright (c) 2018-2020 Bluespec, Inc. All Rights Reserved.
//
//-
// RVFI_DII + CHERI modifications:
//     Copyright (c) 2020 Alexandre Joannou
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

package CoreW_IFC;

// ================================================================
// This package defines the interface of a CoreW module which
// contains:
//     - mkProc (the RISC-V CPU; this a variant of MIT's RISCY-OOO mkProc)
//            Note: MIT's RISCY-OOO internally has a 'mkCore' and hence this
//                  interface and its module is called 'CoreW', to disambiguate.
//     - mkFabric_2x3
//     - mkNear_Mem_IO_AXI4
//     - mkPLIC_16_CoreNumX2_7
//     - mkTV_Encode          (Tandem-Verification logic, optional: INCLUDE_TANDEM_VERIF)
//     - mkDebug_Module       (RISC-V Debug Module, optional: INCLUDE_GDB_CONTROL)

// ================================================================
// BSV library imports

import Vector        :: *;
import GetPut        :: *;
import ClientServer  :: *;

// ----------------
// BSV additional libs
import AXI4 :: *;

// ================================================================
// Project imports

// Main fabric
import Fabric_Defs  :: *;

// External interrupt request interface
import PLIC  :: *;

`ifdef INCLUDE_GDB_CONTROL
import Debug_Module  :: *;
`endif

`ifdef RVFI_DII
import ProcTypes :: *;
`endif

`ifdef INCLUDE_TANDEM_VERIF
import ProcTypes   :: *;
import Trace_Data2 :: *;
import TV_Info     :: *;
`endif

// ================================================================
// The CoreW interface

interface CoreW_IFC #(numeric type t_n_interrupt_sources);

   // ----------------------------------------------------------------
   // Debugging: set core's verbosity

   method Action  set_verbosity (Bit #(4)  verbosity, Bit #(64)  logdelay);

   // ----------------------------------------------------------------
   // Start

   method Action start (Bool is_running, Bit #(64) tohost_addr, Bit #(64) fromhost_addr);

   // ----------------------------------------------------------------
   // AXI4 Fabric interfaces

   // CPU IMem to Fabric master interface
   interface AXI4_Master #(TAdd#(Wd_MId,1), Wd_Addr, Wd_Data,
                           0, 0, 0, 0, 0) cpu_imem_master;

   // CPU DMem to Fabric master interface
   interface AXI4_Master #(TAdd#(Wd_MId,1), Wd_Addr, Wd_Data,
                           0, 0, 0, 0, 0) cpu_dmem_master;

   // ----------------------------------------------------------------
   // External interrupt sources

   interface Vector #(t_n_interrupt_sources, PLIC_Source_IFC)  core_external_interrupt_sources;

   // ----------------------------------------------------------------
   // Non-maskable interrupt request

   (* always_ready, always_enabled *)
   method Action nmi_req (Bool set_not_clear);

`ifdef RVFI_DII
    interface Toooba_RVFI_DII_Server rvfi_dii_server;
`endif

`ifdef INCLUDE_GDB_CONTROL
   // ----------------------------------------------------------------
   // Optional Debug Module interfaces

   // ----------------
   // DMI (Debug Module Interface) facing remote debugger

   interface DMI dmi;

   // ----------------
   // Facing Platform
   // Non-Debug-Module Reset (reset all except DM)

   interface Client #(Bool, Bool) ndm_reset_client;
`endif

`ifdef INCLUDE_TANDEM_VERIF
   // ----------------------------------------------------------------
   // Optional Tandem Verifier interface output tuples (n,vb),
   // where 'vb' is a vector of bytes
   // with relevant bytes in locations [0]..[n-1]

   interface Get #(Info_CPU_to_Verifier)  tv_verifier_info_get;
`endif

endinterface

// ================================================================
// The Synthesizable CoreW interface (same with Synth AXI)

interface CoreW_IFC_Synth #(numeric type t_n_interrupt_sources);

   // ----------------------------------------------------------------
   // Debugging: set core's verbosity

   method Action  set_verbosity (Bit #(4)  verbosity, Bit #(64)  logdelay);

   // ----------------------------------------------------------------
   // Start

   method Action start (Bool is_running, Bit #(64) tohost_addr, Bit #(64) fromhost_addr);

   // ----------------------------------------------------------------
   // AXI4 Fabric interfaces

   // CPU IMem to Fabric master interface
   interface AXI4_Master_Sig #(TAdd#(Wd_MId,1), Wd_Addr, Wd_Data,
                                 0, 0, 0, 0, 0) cpu_imem_master;

   // CPU DMem to Fabric master interface
   interface AXI4_Master_Sig #(TAdd#(Wd_MId,1), Wd_Addr, Wd_Data,
                                 0, 0, 0, 0, 0) cpu_dmem_master;

   // ----------------------------------------------------------------
   // External interrupt sources

   interface Vector #(t_n_interrupt_sources, PLIC_Source_IFC)  core_external_interrupt_sources;

   // ----------------------------------------------------------------
   // Non-maskable interrupt request

   (* always_ready, always_enabled *)
   method Action nmi_req (Bool set_not_clear);

`ifdef RVFI_DII
    interface Toooba_RVFI_DII_Server rvfi_dii_server;
`endif

`ifdef INCLUDE_GDB_CONTROL
   // ----------------------------------------------------------------
   // Optional Debug Module interfaces

   // ----------------
   // DMI (Debug Module Interface) facing remote debugger

   interface DMI dmi;

   // ----------------
   // Facing Platform
   // Non-Debug-Module Reset (reset all except DM)

   interface Client #(Bool, Bool) ndm_reset_client;
`endif

`ifdef INCLUDE_TANDEM_VERIF
   // ----------------------------------------------------------------
   // Optional Tandem Verifier interface output tuples (n,vb),
   // where 'vb' is a vector of bytes
   // with relevant bytes in locations [0]..[n-1]

   interface Get #(Info_CPU_to_Verifier)  tv_verifier_info_get;
`endif

endinterface

// ================================================================

endpackage
