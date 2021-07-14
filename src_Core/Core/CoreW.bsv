// Copyright (c) 2018-2020 Bluespec, Inc. All Rights Reserved.

package CoreW;

// ================================================================
// This package is called 'CoreW' for 'Core Wrapper'
// and corresponds to 'Core' in Piccolo and Flute.
//
// Here in Toooba, we use the name 'CoreW' to avoid a name-clash with
// an inner module called 'Core' in MIT's RISCY-OOO.
//
// The specific correspondence with Piccolo/Flute structure is:
//    Piccolo/Flute    Toooba
//      mkCore         mkCoreW
//                     mkProc
//      mkCPU          mkCore


// This package defines:
//     Core_IFC
//     mkCore #(Core_IFC)
//     mkFabric_2x3    -- specialized AXI4 fabric used inside this core
//
// mkCoreW instantiates:
//     - mkProc (the RISC-V CPU, a version of MIT's RISCY-OOO)
//     - mkFabric_2x3
//     - mkPLIC_16_CoreNumX2_7
//     - mkTV_Encode          (Tandem-Verification logic, optional: INCLUDE_TANDEM_VERIF)
//     - mkDebug_Module       (RISC-V Debug Module, optional: INCLUDE_GDB_CONTROL)
// and connects them all up.

// ================================================================
// BSV library imports

import Vector       :: *;
import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;
import Connectable  :: *;
import Clocks       :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;

// ================================================================
// Project imports

// ----------------
// From RISCY-ooo
import ProcTypes    :: *;

// ----------------
// From Toooba

// Main fabric
import AXI4_Types   :: *;
import AXI4_Fabric  :: *;
import Fabric_Defs  :: *;    // for Wd_Id, Wd_Addr, Wd_Data, Wd_User
import SoC_Map      :: *;

`ifdef INCLUDE_GDB_CONTROL
import Debug_Module  :: *;
`endif

import CoreW_IFC    :: *;
import Proc_IFC     :: *;
import Proc         :: *;

import PLIC                :: *;
import PLIC_16_CoreNumX2_7 :: *;

`ifdef INCLUDE_TANDEM_VERIF
import TV_Info                   :: *;
import Trace_Data2               :: *;
import TV_Encode                 :: *;
import Trace_Data2_to_Trace_Data :: *;
`endif

// TV_Taps needed when both GDB_CONTROL and TANDEM_VERIF are present
`ifdef INCLUDE_GDB_CONTROL
`ifdef INCLUDE_TANDEM_VERIF
import TV_Taps  :: *;
`endif
`endif

import DM_CPU_Req_Rsp ::*;

// ================================================================
// The Core module

(* synthesize *)
module mkCoreW #(Reset dm_power_on_reset)
               (CoreW_IFC #(N_External_Interrupt_Sources));

   // ================================================================
   // Notes on 'reset'

   // This module's default reset (Verilog RST_N) is a
   // 'non-debug-module reset', or 'ndm-reset': it resets everything
   // in mkCoreW other than the optional RISC-V Debug Module (DM).

   // DM is reset ONLY by 'dm_power_on_reset' (parameter of this module).
   // This is expected to be performed exactly once, on power-up.

   // Note: DM has an internal functionality that the DM spec calls
   //   'dm_reset'. This is not really an electrical reset, it is just
   //   a module initializer wholly within the DM to put it into a
   //   known state.  To be able to do a dm_reset, the DM has to be
   //   working already, at least to the point that it can field DMI
   //   requests from the external debugger asking the DM to proform a
   //   dm_reset.

   // DM can ask the environment to perform an 'ndm-reset', which the
   // environment does by asserting the default reset (RST_N).  At the
   // same time, the environment may also reset part or all of the
   // rest of the SoC.

   // DM can also individually reset each hart in mkCPU.
   // 'hart' = hardware thread = independent PC and fetch-and-execute pipeline.
   // mkCPU (instantiated in this module) has one or more harts.
   // This hart-reset logic is entirely within this module.

   // ================================================================
   // The CPU's (hart's) reset is the ``or'' of the default reset
   // (power-on reset) and the Debug Module's 'hart_reset' control.

   let ndm_reset <- exposeCurrentReset;

`ifdef INCLUDE_GDB_CONTROL
   let clk <- exposeCurrentClock;
   Bool    initial_reset_val   = False;
   Integer hart_reset_duration = 10;    // NOTE: assuming 10 cycle reset enough for hart
   Vector #(CoreNum, MakeResetIfc) dm_harts_reset_controller <- replicateM(mkReset(hart_reset_duration, initial_reset_val, clk));

   function Reset proj_new_rst (MakeResetIfc x) = x.new_rst;

   let all_harts_reset <- foldlM (mkResetEither, ndm_reset, map (proj_new_rst, dm_harts_reset_controller));
`else
   let all_harts_reset = ndm_reset;
`endif

   // ================================================================
   // STATE

   // System address map
   SoC_Map_IFC  soc_map  <- mkSoC_Map;

   // RISCY-OOO processor
   // TODO: could have separate resets for each core.
   Proc_IFC proc <- mkProc (reset_by all_harts_reset);

   // A 2x3 fabric for connecting {CPU, Debug_Module} to {Fabric, PLIC}
   Fabric_2x3_IFC  fabric_2x3 <- mkFabric_2x3;

   // PLIC (Platform-Level Interrupt Controller)
   PLIC_IFC_16_CoreNumX2_7  plic <- mkPLIC_16_CoreNumX2_7;

`ifdef INCLUDE_GDB_CONTROL
   // Debug Module
   Debug_Module_IFC  debug_module <- mkDebug_Module (reset_by dm_power_on_reset);
`endif

`ifdef INCLUDE_TANDEM_VERIF
   // The following are a superscalar-wide set of transformers from RISCY-OOO output Trace_Data2
   // to Trace_Data which is input to the TV encoder
   Vector #(SupSize, Trace_Data2_to_Trace_Data_IFC) v_td2_to_td <- replicateM (mkTrace_Data2_to_Trace_Data);

   // The TV encoder transforms Trace_Data structures from the CPU and DM
   // into encoded byte vectors for transmission to the Tandem Verifier
   TV_Encode_IFC tv_encode <- mkTV_Encode;
`endif

   // ================================================================
   // Hart-reset from DM

`ifdef INCLUDE_GDB_CONTROL
   Reg #(Bit #(8))  rg_harts_reset_delay <- mkReg (0);
   Reg #(Bit #(64)) rg_tohost_addr       <- mkReg (0);
   Reg #(Bit #(64)) rg_fromhost_addr     <- mkReg (0);

   for (Integer core = 0; core < valueOf(CoreNum); core = core + 1)
      rule rl_dm_harts_reset (rg_harts_reset_delay == 0);
	 let x <- debug_module.harts_reset_client[core].request.get;
	 dm_harts_reset_controller[core].assertReset;
	 rg_harts_reset_delay <= fromInteger (hart_reset_duration + 200);    // NOTE: heuristic

	 $display ("%0d: %m.rl_dm_harts_reset: asserting harts reset for %0d cycles",
                cur_cycle, hart_reset_duration);
   endrule

   rule rl_dm_harts_reset_wait (rg_harts_reset_delay != 0);
      if (rg_harts_reset_delay == 1) begin
         let pc = soc_map_struct.pc_reset_value;
         Bool is_running = True;
      	proc.start (is_running, pc, rg_tohost_addr, rg_fromhost_addr);
         // We reset all the harts, so we indicate this to the DM, even though it's possible only one hart was requested to reset
         for (Integer core = 0; core < valueOf(CoreNum); core = core + 1)
      	   debug_module.harts_reset_client[core].response.put (is_running);
         $display ("%0d: %m.rl_dm_harts_reset_wait: proc.start (pc %0h, tohostAddr %0h, fromhostAddr %0h",
                   cur_cycle, pc, rg_tohost_addr, rg_fromhost_addr);
      end
      rg_harts_reset_delay <= rg_harts_reset_delay - 1;
   endrule

`endif

`ifdef INCLUDE_GDB_CONTROL
   // ================================================================
   // Direct DM-to-CPU connections for run-control and other misc requests

   mkConnection (debug_module.harts_client_run_halt, proc.harts_run_halt_server);
   mkConnection (debug_module.harts_get_other_req,   proc.harts_put_other_req);
`endif

`ifdef INCLUDE_TANDEM_VERIF
   // ================================================================
   // Direct CPU-to-TV connections for TV trace data

   for (Integer j = 0; j < valueOf (SupSize); j = j + 1) begin
      // CPU Trace_Data2 output streams to Trace_Data2_to_Trace_Data converters
      mkConnection (proc.v_to_TV [j], v_td2_to_td [j].in);
      // Trace_Data2_to_Trace_Data converters to TV encoder
      mkConnection (v_td2_to_td [j].out, tv_encode.v_cpu_in [j]);
   end
`endif

`ifdef INCLUDE_GDB_CONTROL
`ifdef INCLUDE_TANDEM_VERIF
   // ================================================================
   // BEGIN SECTION: DM and TV both present
   // We instantiate 'taps' into connections where DM writes CPU GPRs,
   // FPRs, CSRs, and main memory.  The tap outputs go the TV encoder,
   // to keep the tandem verifier in sync with DM updates to the CPU.

   // Create a tap for DM's memory-writes to the bus, and merge-in the trace data.
   DM_Mem_Tap_IFC dm_mem_tap <- mkDM_Mem_Tap;
   mkConnection (debug_module.master, dm_mem_tap.slave);
   let dm_master_local = dm_mem_tap.master;

   rule rl_merge_dm_mem_trace_data;
      let tmp <- dm_mem_tap.trace_data_out.get;
      tv_encode.dm_in.put (tmp);
   endrule

   // Create a tap for DM's GPR writes to the CPU, and merge-in the trace data.
   DM_GPR_Tap_IFC  dm_gpr_tap_ifc <- mkDM_GPR_Tap;
   mkConnection (debug_module.hart0_gpr_mem_client, dm_gpr_tap_ifc.server);
   mkConnection (dm_gpr_tap_ifc.client, proc.hart0_gpr_mem_server);

   rule rl_merge_dm_gpr_trace_data;
      let tmp <- dm_gpr_tap_ifc.trace_data_out.get;
      tv_encode.dm_in.put (tmp);
   endrule

`ifdef ISA_F_OR_D
   // Create a tap for DM's FPR writes to the CPU, and merge-in the trace data.
   DM_FPR_Tap_IFC  dm_fpr_tap_ifc <- mkDM_FPR_Tap;
   mkConnection (debug_module.hart0_fpr_mem_client, dm_fpr_tap_ifc.server);
   mkConnection (dm_fpr_tap_ifc.client, proc.hart0_fpr_mem_server);

   rule rl_merge_dm_fpr_trace_data;
      let tmp <- dm_fpr_tap_ifc.trace_data_out.get;
      tv_encode.dm_in.put (tmp);
   endrule
`endif
   // for ifdef ISA_F_OR_D

   // Create a tap for DM's CSR writes, and merge-in the trace data.
   DM_CSR_Tap_IFC  dm_csr_tap <- mkDM_CSR_Tap;
   mkConnection(debug_module.hart0_csr_mem_client, dm_csr_tap.server);
   mkConnection(dm_csr_tap.client, proc.hart0_csr_mem_server);

   rule rl_merge_dm_csr_trace_data;
      let tmp <- dm_csr_tap.trace_data_out.get;
      tv_encode.dm_in.put(tmp);
   endrule

`ifdef ISA_F_OR_D
   (* descending_urgency = "rl_merge_dm_fpr_trace_data, rl_merge_dm_gpr_trace_data" *)
`endif
   (* descending_urgency = "rl_merge_dm_gpr_trace_data, rl_merge_dm_csr_trace_data" *)
   (* descending_urgency = "rl_merge_dm_csr_trace_data, rl_merge_dm_mem_trace_data" *)
   rule rl_bogus_for_sched_attributes;
   endrule

   // END SECTION: DM and TV
   // ================================================================
`else    // of ifdef INCLUDE_TANDEM_VERIF
   // ================================================================
   // BEGIN SECTION: DM, no TV

   // Connect DM's GPR interface directly to CPU
   mkConnection (debug_module.harts_gpr_mem_client, proc.harts_gpr_mem_server);

`ifdef ISA_F_OR_D
   // Connect DM's FPR interface directly to CPU
   mkConnection (debug_module.harts_fpr_mem_client, proc.harts_fpr_mem_server);
`endif

   // Connect DM's CSR interface directly to CPU
   mkConnection (debug_module.harts_csr_mem_client, proc.harts_csr_mem_server);

   // DM's bus master is directly the bus master
   let dm_master_local = debug_module.master;

   // END SECTION: DM, no TV
   // ================================================================
`endif    // for ifdef INCLUDE_TANDEM_VERIF
   // ================================================================
`else    // for ifdef INCLUDE_GDB_CONTROL
   // ================================================================
   // BEGIN SECTION: no DM

   // No DM, so 'DM bus master' is AXI4 dummy
   AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User)
   dm_master_local = dummy_AXI4_Master_ifc;

`ifdef INCLUDE_TANDEM_VERIF
   // TV, no DM: stub out the dm input to TV
   Get #(Trace_Data) gs = getstub;
   mkConnection (tv_encode.dm_in, gs);
`endif

`endif    // for ifdef INCLUDE_GDB_CONTROL


   // ================================================================
   // Connect the local 2x3 fabric

   // Masters on the local 2x3 fabric
   mkConnection (proc.master1,  fabric_2x3.v_from_masters [cpu_dmem_master_num]);
   mkConnection (dm_master_local, fabric_2x3.v_from_masters [debug_module_sba_master_num]);

   // Slaves on the local 2x3 fabric
   // Two of the slaves are connected here.
   // The third slave (default slave) is taken out directly to the Core interface
   mkConnection (fabric_2x3.v_to_slaves [plic_slave_num], plic.axi4_slave);
   mkConnection (fabric_2x3.v_to_slaves [llc_slave_num],  proc.debug_module_mem_server);

   // ================================================================
   // Connect external interrupt lines from PLIC to CPU

   rule rl_relay_external_interrupts;    // from PLIC
      Vector #(CoreNum, Bool) meips;
      Vector #(CoreNum, Bool) seips;

      for (Integer i = 0; i < valueof(CoreNum); i = i + 1) begin
	 meips [i] = plic.v_targets [2 * i].m_eip;
	 seips [i] = plic.v_targets [2 * i + 1].m_eip;
      end

      proc.m_external_interrupt_req (meips);
      proc.s_external_interrupt_req (seips);
   endrule

   // ================================================================
   // INTERFACE

   // ----------------------------------------------------------------
   // Debugging: set core's verbosity, htif addrs

   method Action  set_verbosity (Bit #(4)  verbosity, Bit #(64)  logdelay);
      // Warning: ignoring logdelay
      proc.set_verbosity (verbosity);
   endmethod

   // ----------------------------------------------------------------
   // Start

   method Action start (Bool is_running, Bit #(64) tohost_addr, Bit #(64) fromhost_addr);
      plic.set_addr_map (zeroExtend (soc_map.m_plic_addr_base),
			 zeroExtend (soc_map.m_plic_addr_lim));

      let pc = soc_map_struct.pc_reset_value;
      proc.start (is_running, pc, tohost_addr, fromhost_addr);

`ifdef INCLUDE_GDB_CONTROL
      // Save for potential future use by rl_dm_harts_reset
      rg_tohost_addr   <= tohost_addr;
      rg_fromhost_addr <= fromhost_addr;
`endif

      $display ("%0d: %m.method start: proc.start (pc %0h, tohostAddr %0h, fromhostAddr %0h)",
		cur_cycle, pc, tohost_addr, fromhost_addr);
   endmethod

   // ----------------------------------------------------------------
   // AXI4 Fabric interfaces

   // IMem to Fabric master interface
   interface AXI4_Master_IFC  cpu_imem_master = proc.master0;

   // DMem to Fabric master interface
   interface AXI4_Master_IFC  cpu_dmem_master = fabric_2x3.v_to_slaves [default_slave_num];

   // ----------------------------------------------------------------
   // External interrupt sources

   interface core_external_interrupt_sources = plic.v_sources;

   // ----------------------------------------------------------------
   // Non-maskable interrupt request

   method Action nmi_req (Bool set_not_clear);
      // TODO: fixup; passing const False for now
      proc.non_maskable_interrupt_req (False);
   endmethod

`ifdef INCLUDE_GDB_CONTROL
   // ----------------------------------------------------------------
   // Optional DM interfaces

   // ----------------
   // DMI (Debug Module Interface) facing remote debugger

   interface DMI dmi = debug_module.dmi;

   // ----------------
   // Facing Platform

   // Non-Debug-Module Reset (reset all except DM)
   interface Client ndm_reset_client = debug_module.ndm_reset_client;
`endif

`ifdef INCLUDE_TANDEM_VERIF
   // ----------------------------------------------------------------
   // Optional TV interface

   interface Get tv_verifier_info_get;
      method ActionValue #(Info_CPU_to_Verifier) get();
         match { .n, .v } <- tv_encode.out.get;
         return (Info_CPU_to_Verifier { num_bytes: n, vec_bytes: v });
      endmethod
   endinterface
`endif

endmodule: mkCoreW

// ================================================================
// 2x3 Fabric for this Core
// Masters: CPU DMem, Debug Module System Bus Access, External access

// ----------------
// Fabric port numbers for masters

typedef 2  Num_Masters_2x3;

typedef Bit #(TLog #(Num_Masters_2x3))  Master_Num_2x3;

Master_Num_2x3  cpu_dmem_master_num         = 0;
Master_Num_2x3  debug_module_sba_master_num = 1;

// ----------------
// Fabric port numbers for slaves

typedef 3  Num_Slaves_2x3;

typedef Bit #(TLog #(Num_Slaves_2x3))  Slave_Num_2x3;

Slave_Num_2x3  default_slave_num = 0;    // for I/O, uncached memory, etc.
Slave_Num_2x3  plic_slave_num    = 1;    // PLIC mem-mapped registers
Slave_Num_2x3  llc_slave_num     = 2;    // Normal cached memory (connects to coherent Last-Level Cache)

// ----------------
// Specialization of parameterized AXI4 fabric for 2x3 Core fabric

typedef AXI4_Fabric_IFC #(Num_Masters_2x3,
			  Num_Slaves_2x3,
			  Wd_Id,
			  Wd_Addr,
			  Wd_Data,
			  Wd_User)  Fabric_2x3_IFC;

// ----------------

(* synthesize *)
module mkFabric_2x3 (Fabric_2x3_IFC);

   // System address map
   SoC_Map_IFC  soc_map  <- mkSoC_Map;

   // ----------------
   // Slave address decoder
   // Any addr is legal, and there is only one slave to service it.

   function Tuple2 #(Bool, Slave_Num_2x3) fn_addr_to_slave_num_2x3  (Fabric_Addr addr);
      if (   (soc_map.m_mem0_controller_addr_base <= addr)
	  && (addr < soc_map.m_mem0_controller_addr_lim))
	 return tuple2 (True, llc_slave_num);

      else if (   (soc_map.m_plic_addr_base <= addr)
	       && (addr < soc_map.m_plic_addr_lim))
	 return tuple2 (True, plic_slave_num);

      else
	 return tuple2 (True, default_slave_num);
   endfunction

   AXI4_Fabric_IFC #(Num_Masters_2x3, Num_Slaves_2x3, Wd_Id, Wd_Addr, Wd_Data, Wd_User)
       fabric <- mkAXI4_Fabric (fn_addr_to_slave_num_2x3);

   return fabric;
endmodule: mkFabric_2x3

// ================================================================

endpackage
