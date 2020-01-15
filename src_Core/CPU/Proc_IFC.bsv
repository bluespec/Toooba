// Copyright (c) 2016-2019 Bluespec, Inc. All Rights Reserved

package Proc_IFC;

// ================================================================
// BSV library imports

import GetPut       :: *;
import ClientServer :: *;

// ================================================================
// Project imports

import ISA_Decls  :: *;

import AXI4_Types  :: *;
import Fabric_Defs :: *;

`ifdef INCLUDE_GDB_CONTROL
import DM_CPU_Req_Rsp :: *;
`endif

`ifdef INCLUDE_TANDEM_VERIF
import TV_Info  :: *;
`endif

// ================================================================
// CPU interface

// Note: this Proc_IFC is similar, but not identical to CPU_IFC for Piccolo and Flute
//       Specifically, it removes interfaces for software and timer,
//       because the RISCY-OOO mkProc contains those elements.

interface Proc_IFC;
   // Reset
   interface Server #(Token, Token)  hart0_server_reset;

   // ----------------
   // Start the cores running
   method Action start (Addr startpc, Addr tohostAddr, Addr fromhostAddr);

   // ----------------
   // SoC fabric connections

   // Fabric master interface for memory (from LLC)
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User)  master0;

   // Fabric master interface for IO (from MMIOPlatform)
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User)  master1;

   // ----------------
   // External interrupts

   (* always_ready, always_enabled *)
   method Action  m_external_interrupt_req (Bool set_not_clear);

   (* always_ready, always_enabled *)
   method Action  s_external_interrupt_req (Bool set_not_clear);

   // ----------------
   // External interrupt [14] to go into Debug Mode

   (* always_ready, always_enabled *)
   method Action  debug_external_interrupt_req (Bool set_not_clear);

   // ----------------
   // Non-maskable interrupt

   (* always_ready, always_enabled *)
   method Action  non_maskable_interrupt_req (Bool set_not_clear);

   // ----------------
   // Set core's verbosity

   method Action  set_verbosity (Bit #(4)  verbosity);

   // ----------------
   // Optional interface to Tandem Verifier

`ifdef INCLUDE_TANDEM_VERIF
   interface Get #(Trace_Data)  trace_data_out;
`endif

   // ----------------
   // Optional interface to Debug Module

`ifdef INCLUDE_GDB_CONTROL
   // run-control, other
   interface Server #(Bool, Bool)  hart0_server_run_halt;
   interface Put #(Bit #(4))       hart0_put_other_req;

   // GPR access
   interface Server #(DM_CPU_Req #(5,  XLEN), DM_CPU_Rsp #(XLEN)) hart0_gpr_mem_server;

`ifdef ISA_F
   // FPR access
   interface Server #(DM_CPU_Req #(5,  FLEN), DM_CPU_Rsp #(FLEN)) hart0_fpr_mem_server;
`endif

   // CSR access
   interface Server #(DM_CPU_Req #(12, XLEN), DM_CPU_Rsp #(XLEN)) hart0_csr_mem_server;

   interface AXI4_Slave_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User)  debug_module_mem_server;
`endif

endinterface

// ================================================================

endpackage
