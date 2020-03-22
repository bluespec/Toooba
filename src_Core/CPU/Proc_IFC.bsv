// Copyright (c) 2016-2020 Bluespec, Inc. All Rights Reserved

package Proc_IFC;

// ================================================================
// BSV library imports

import Vector       :: *;
import GetPut       :: *;
import ClientServer :: *;

// ================================================================
// Project imports

import ISA_Decls  :: *;

import AXI4  :: *;
import Fabric_Defs :: *;
import SoC_Map :: *;

`ifdef INCLUDE_GDB_CONTROL
import DM_CPU_Req_Rsp :: *;
`endif

`ifdef INCLUDE_TANDEM_VERIF
import ProcTypes   :: *;
import Trace_Data2 :: *;
`endif

`ifdef RVFI_DII
import Types :: *;
`endif

// ================================================================
// CPU interface

// Note: this Proc_IFC is similar, but not identical to CPU_IFC for Piccolo and Flute
//       Specifically, it removes interfaces for software and timer,
//       because the RISCY-OOO mkProc contains those elements.

interface Proc_IFC;

   // ----------------
   // Start the cores running
   // Use toHostAddr = 0 if not monitoring tohost
   method Action start (Addr startpc, Addr tohostAddr, Addr fromhostAddr);

   // ----------------
   // SoC fabric connections

   // Fabric master interface for memory (from LLC)
   interface AXI4_Master_Synth #(Wd_MId, Wd_Addr, Wd_Data,
                                 Wd_AW_User, Wd_W_User, Wd_B_User,
                                 Wd_AR_User, Wd_R_User) master0;

   // Fabric master interface for IO (from MMIOPlatform)
   interface AXI4_Master_Synth #(Wd_MId_2x3, Wd_Addr, Wd_Data,
                                 Wd_AW_User, Wd_W_User, Wd_B_User,
                                 Wd_AR_User, Wd_R_User) master1;

   // ----------------
   // External interrupts

   (* always_ready, always_enabled *)
   method Action  m_external_interrupt_req (Bool set_not_clear);

   (* always_ready, always_enabled *)
   method Action  s_external_interrupt_req (Bool set_not_clear);

   // ----------------
   // Non-maskable interrupt

   (* always_ready, always_enabled *)
   method Action  non_maskable_interrupt_req (Bool set_not_clear);

   // ----------------
   // Set core's verbosity

   method Action  set_verbosity (Bit #(4)  verbosity);

   // ----------------
   // Coherent port into LLC (used by Debug Module, DMA engines, ... to read/write memory)

   interface AXI4_Slave_Synth #(Wd_SId_2x3, Wd_Addr, Wd_Data,
                                Wd_AW_User, Wd_W_User, Wd_B_User,
                                Wd_AR_User, Wd_R_User) debug_module_mem_server;

`ifdef RVFI_DII
   interface Toooba_RVFI_DII_Server rvfi_dii_server;
`endif

   // ----------------
   // Optional interface to Debug Module

`ifdef INCLUDE_GDB_CONTROL
   interface Server #(Bool, Bool)                                 hart0_run_halt_server;
   interface Server #(DM_CPU_Req #(5,  XLEN), DM_CPU_Rsp #(XLEN)) hart0_gpr_mem_server;
`ifdef ISA_F
   interface Server #(DM_CPU_Req #(5,  FLEN), DM_CPU_Rsp #(FLEN)) hart0_fpr_mem_server;
`endif
   interface Server #(DM_CPU_Req #(12, XLEN), DM_CPU_Rsp #(XLEN)) hart0_csr_mem_server;

   // Non-standard
   interface Put #(Bit #(4))                                      hart0_put_other_req;
`endif

`ifdef INCLUDE_TANDEM_VERIF
   // Note: this is a SupSize vector of streams of Trace_Data2 structs,
   // each of which has a serialnum field.  Each of the SupSize
   // streams has serialnums in increasing order.  Each serialnum
   // appears exactly once in exactly one of the streams. Thus, the
   // channels can easily be merged into a single program-order stream.
   interface Vector #(SupSize, Get #(Trace_Data2)) v_to_TV;
`endif

endinterface

// ================================================================

endpackage
