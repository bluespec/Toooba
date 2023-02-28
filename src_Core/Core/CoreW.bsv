// Copyright (c) 2018-2020 Bluespec, Inc. All Rights Reserved.
//
//-
// RVFI_DII + CHERI modifications:
//     Copyright (c) 2020-2022 Alexandre Joannou
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
//
// This material is based upon work supported by the DoD Information Analysis
// Center Program Management Office (DoD IAC PMO), sponsored by the Defense
// Technical Information Center (DTIC) under Contract No. FA807518D0004.  Any
// opinions, findings and conclusions or recommendations expressed in this
// material are those of the author(s) and do not necessarily reflect the views
// of the Air Force Installation Contracting Agency (AFICA).
//-

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
//
// mkCoreW instantiates:
//     - mkProc (the RISC-V CPU, a version of MIT's RISCY-OOO)
//     - mkPLIC_16_CoreNumX2_7
//     - mkTV_Encode          (Tandem-Verification logic, optional: INCLUDE_TANDEM_VERIF)
//     - mkDebug_Module       (RISC-V Debug Module, optional: INCLUDE_GDB_CONTROL)
// and connects them all up.

// ================================================================
// BSV library imports

import Vector       :: *;
import FIFO         :: *;
import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;
import Connectable  :: *;
import Clocks       :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;
import Routable   :: *;
import BlueAXI4   :: *;
import SourceSink :: *;
import TagControllerAXI :: *;
import CacheCore  :: *;

// ================================================================
// Project imports

// ----------------
// From RISCY-ooo
import ProcTypes    :: *;
`ifdef PERFORMANCE_MONITORING
import StatCounters::*;
`endif

// ----------------
// From Toooba

// Main fabric
import Fabric_Defs  :: *;    // for Wd_Id, Wd_Addr, Wd_Data...
import SoC_Map      :: *;

`ifdef INCLUDE_GDB_CONTROL
import Debug_Module  :: *;
`endif

import WindCoreInterface :: *;
import Proc_IFC          :: *;
import Proc              :: *;

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

typedef WindCoreMid #( // AXI lite subordinate control port parameters
                       21, 32, 0, 0, 0, 0, 0
                       // AXI manager 0 port parameters
                     , TAdd #(Wd_MId, 1), Wd_Addr, Wd_Data, 0, 0, 0, 0, 0
                       // AXI manager 1 port parameters
                     , TAdd #(Wd_MId, 1), Wd_Addr, Wd_Data_Periph, 0, 0, 0, 0, 0
                       // AXI subordinate 0 port parameters
                     , Wd_CoreW_Bus_MId, Wd_Addr, Wd_Data_Periph
                     , Wd_AW_User_Periph, Wd_W_User_Periph, Wd_B_User_Periph
                     , Wd_AR_User_Periph, Wd_R_User_Periph
                       // Number of interrupt lines
                     , t_n_irq) CoreW_IFC #(numeric type t_n_irq);

//(* synthesize *)
`ifdef RVFI_DII
module mkCoreW (Tuple2#(Toooba_RVFI_DII_Server, CoreW_IFC #(t_n_irq)));
`else
module mkCoreW (CoreW_IFC #(t_n_irq));
`endif
   Clock clk <- exposeCurrentClock;
   Reset rst <- exposeCurrentReset;
   let newRst <- mkReset (0, True, clk, reset_by rst);
   match {.otherRst
`ifdef RVFI_DII
          , .rvfi
`endif
          , .ifc} 
     <- mkCoreW_reset ( rst, reset_by newRst.new_rst);
   (* no_implicit_conditions, fire_when_enabled *)
   rule rl_forward_debug_reset (otherRst);
      newRst.assertReset;
   endrule
`ifdef RVFI_DII
   return tuple2(rvfi, ifc);
`else
   return ifc;
`endif
endmodule

// The interface to this module is a convenience to avoid exposing the reset
// hacks to the nicer outer interface, and not have to use a large amount of
// reset_by to decouple the debug module from the rest...
module mkCoreW_reset #(Reset porReset)
`ifdef RVFI_DII
                      (Tuple3#(PulseWire, Toooba_RVFI_DII_Server, CoreW_IFC #(t_n_irq)));
`else
                      (Tuple2#(PulseWire, CoreW_IFC #(t_n_irq)));
`endif

   // ================================================================
   // Notes on 'reset'

   // This module's default reset (Verilog RST_N) is a
   // 'non-debug-module reset', or 'ndm-reset': it resets everything
   // in mkCoreW other than the optional RISC-V Debug Module (DM).

   // DM is reset ONLY by 'porReset' (parameter of this module).
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

   // handle uncached interface
   AXI4_Master #( Wd_CoreW_Bus_MId, Wd_Addr, Wd_Data_Periph, 0, 0, 0, 0, 0)
       proc_uncached = prepend_AXI4_Master_id (0, zero_AXI4_Master_user (proc.master1));
   // Bridge for uncached expernal bus transactions.
   let uncached_mem_shim <- mkAXI4ShimFF(reset_by all_harts_reset);

   // handle cached interface
   // AXI4 tagController
   TagControllerAXI #(Wd_MId, Wd_Addr, Wd_Data)
     tagController <- mkTagControllerAXI (reset_by all_harts_reset); // TODO double check if reseting like this is good enough
   mkConnection (proc.master0, tagController.slave, reset_by all_harts_reset);
   AXI4_Shim#(TAdd #(Wd_MId, 1), Wd_Addr, Wd_Data, 0, 0, 0, 0, 0)
      tag_controller_deburster <- mkBurstToNoBurst;
   mkConnection (tagController.master, tag_controller_deburster.slave, reset_by all_harts_reset);


`ifdef PERFORMANCE_MONITORING
   rule report_tagController_events;
      EventsCacheCore cache_core_evts = tagController.events;
      EventsTGC evts = unpack(0);
      evts.evt_WRITE = zeroExtend(pack(cache_core_evts.evt_WRITE));
      evts.evt_WRITE_MISS = zeroExtend(pack(cache_core_evts.evt_WRITE_MISS));
      evts.evt_READ = zeroExtend(pack(cache_core_evts.evt_READ));
      evts.evt_READ_MISS = zeroExtend(pack(cache_core_evts.evt_READ_MISS));
      evts.evt_EVICT = zeroExtend(pack(cache_core_evts.evt_EVICT));
`ifdef USECAP
      evts.evt_SET_TAG_WRITE = zeroExtend(pack(cache_core_evts.evt_SET_TAG_WRITE));
      evts.evt_SET_TAG_READ = zeroExtend(pack(cache_core_evts.evt_SET_TAG_READ));
`endif
      proc.events_tgc(evts);
   endrule
`endif

   // PLIC (Platform-Level Interrupt Controller)
   PLIC_IFC_16_CoreNumX2_7  plic <- mkPLIC_16_CoreNumX2_7 (reset_by all_harts_reset);

`ifdef INCLUDE_GDB_CONTROL
   // Debug Module
   Debug_Module_IFC  debug_module <- mkDebug_Module (reset_by porReset);
`endif

`ifdef INCLUDE_TANDEM_VERIF
   // The following are a superscalar-wide set of transformers from RISCY-OOO output Trace_Data2
   // to Trace_Data which is input to the TV encoder
   Vector #(SupSize, Trace_Data2_to_Trace_Data_IFC) v_td2_to_td <- replicateM (mkTrace_Data2_to_Trace_Data);

   // The TV encoder transforms Trace_Data structures from the CPU and DM
   // into encoded byte vectors for transmission to the Tandem Verifier
   TV_Encode_IFC tv_encode <- mkTV_Encode;
`endif

   Reg #(Bit #(64)) rg_tohost_addr <- mkReg (0);
   function do_release (restartRunning, to_host_addr) = action
      $display ( "%0d: %m do_release(restartRunning: "
               , cur_cycle, fshow (restartRunning), ", to_host_addr: %0h)"
               , to_host_addr );
      rg_tohost_addr <= to_host_addr;
      plic.set_addr_map (zeroExtend (soc_map.m_plic_addr_range.base),
                         zeroExtend (rangeTop(soc_map.m_plic_addr_range)));
      proc.start ( restartRunning
                 , soc_map_struct.pc_reset_value
                 , to_host_addr
                 , 0 );
   endaction;

   // ================================================================
   // Start the proc a suitable time after a PoR
   UInt#(8) initial_wait = 100; // heuristic -- better to wait till "all out of reset" received from corew
   Reg #(UInt#(8)) rg_corew_start_after_por <- mkReg(initial_wait, reset_by porReset);
   rule rl_step_0 (rg_corew_start_after_por != 0);
      let n = rg_corew_start_after_por - 1;
      rg_corew_start_after_por <= n;
      if (n==0) begin
        $display ("%0d: %m.rl_step_0, n = 0, do_release", cur_cycle);
        do_release (True, rg_tohost_addr);
      end
   endrule

   // ================================================================
   // Hart-reset from DM

`ifdef INCLUDE_GDB_CONTROL
   Reg #(Bool)      rg_harts_reset_running <- mkReg (False);
   Reg #(Bit #(8))  rg_harts_reset_delay   <- mkReg (0);
   Reg #(Bit #(64)) rg_fromhost_addr       <- mkReg (0);

   for (Integer core = 0; core < valueOf(CoreNum); core = core + 1)
      rule rl_dm_harts_reset (rg_harts_reset_delay == 0);
         let x <- debug_module.harts_reset_client[core].request.get;
         dm_harts_reset_controller[core].assertReset;
         rg_harts_reset_delay <= fromInteger (hart_reset_duration + 200); // NOTE: heuristic
         rg_harts_reset_running <= x;
         $display ("%0d: %m.rl_dm_harts_reset: asserting harts reset for %0d cycles",
                cur_cycle, hart_reset_duration);
      endrule

   rule rl_dm_harts_reset_wait (rg_harts_reset_delay != 0);
      if (rg_harts_reset_delay == 1) begin
         let pc = soc_map_struct.pc_reset_value;
         proc.start (rg_harts_reset_running, pc, rg_tohost_addr, rg_fromhost_addr);
         // We reset all the harts, so we indicate this to the DM, even though it's possible only one hart was requested to reset
         for (Integer core = 0; core < valueOf(CoreNum); core = core + 1)
         debug_module.harts_reset_client[core].response.put (?);
         $display ("%0d: %m.rl_dm_harts_reset_wait: proc.start (pc %0h, tohostAddr %0h, fromhostAddr %0h",
                   cur_cycle, pc, rg_tohost_addr, rg_fromhost_addr);
      end
      rg_harts_reset_delay <= rg_harts_reset_delay - 1;
   endrule

`endif

`ifdef INCLUDE_GDB_CONTROL
   // ================================================================
   // Direct DM-to-CPU connections for run-control and other misc requests

   mkConnection (debug_module.harts_client_run_halt, proc.harts_run_halt_server, reset_by porReset);
   mkConnection (debug_module.harts_get_other_req,   proc.harts_put_other_req, reset_by porReset);
   mkConnection (debug_module.harts_is_running,      proc.harts_is_running, reset_by porReset);
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
   AXI4_Master #( Wd_CoreW_Bus_MId, Wd_Addr, Wd_Data_Periph
                , Wd_AW_User_Periph, Wd_W_User_Periph, Wd_B_User_Periph
                , Wd_AR_User_Periph, Wd_R_User_Periph )
     dm_master_local = dm_mem_tap.master;

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
   mkConnection (debug_module.harts_gpr_mem_client, proc.harts_gpr_mem_server, reset_by porReset);

`ifdef ISA_F_OR_D
   // Connect DM's FPR interface directly to CPU
   mkConnection (debug_module.harts_fpr_mem_client, proc.harts_fpr_mem_server, reset_by porReset);
`endif

   // Connect DM's CSR interface directly to CPU
   mkConnection (debug_module.harts_csr_mem_client, proc.harts_csr_mem_server, reset_by porReset);

   // DM's bus master is directly the bus master
   AXI4_Master #( Wd_CoreW_Bus_MId, Wd_Addr, Wd_Data_Periph
                , Wd_AW_User_Periph, Wd_W_User_Periph, Wd_B_User_Periph
                , Wd_AR_User_Periph, Wd_R_User_Periph )
     dm_master_local = debug_module.master;

   // END SECTION: DM, no TV
   // ================================================================
`endif    // for ifdef INCLUDE_TANDEM_VERIF
   // ================================================================
`else    // for ifdef INCLUDE_GDB_CONTROL
   // ================================================================
   // BEGIN SECTION: no DM

   // No DM, so 'DM bus master' is AXI4 dummy
   AXI4_Master #( Wd_CoreW_Bus_MId, Wd_Addr, Wd_Data_Periph
                , Wd_AW_User_Periph, Wd_W_User_Periph, Wd_B_User_Periph
                , Wd_AR_User_Periph, Wd_R_User_Periph )
     dm_master_local = culDeSac;

`ifdef INCLUDE_TANDEM_VERIF
   // TV, no DM: stub out the dm input to TV
   Get #(Trace_Data) gs = getstub;
   mkConnection (tv_encode.dm_in, gs);
`endif

`endif    // for ifdef INCLUDE_GDB_CONTROL


   // ================================================================
   // new internal AXI4 manager from interace subordinate port
   let subShim <- mkAXI4Shim;

   // ================================================================
   // Connect the local bus

   // Masters on the local bus
   Vector #( CoreW_Bus_Num_Masters
           , AXI4_Master #( Wd_CoreW_Bus_MId, Wd_Addr, Wd_Data_Periph
                          , Wd_AW_User_Periph, Wd_W_User_Periph
                          , Wd_B_User_Periph
                          , Wd_AR_User_Periph, Wd_R_User_Periph ))
      master_vector = newVector;
   master_vector[cpu_uncached_master_num]     = proc_uncached;
   master_vector[debug_module_sba_master_num] = dm_master_local;
   master_vector[sub_ifc_master_num]          = subShim.master;

   // Slaves on the local bus
   // default slave is forwarded out directly to the Core interface
   Vector #( CoreW_Bus_Num_Slaves
           , AXI4_Slave #( Wd_CoreW_Bus_SId, Wd_Addr, Wd_Data_Periph
                         , Wd_AW_User_Periph, Wd_W_User_Periph, Wd_B_User_Periph
                         , Wd_AR_User_Periph, Wd_R_User_Periph ))
      slave_vector = newVector;
   slave_vector[default_slave_num] = uncached_mem_shim.slave;
   slave_vector[llc_slave_num]     = zero_AXI4_Slave_user (proc.debug_module_mem_server);
   slave_vector[plic_slave_num]    = zero_AXI4_Slave_user (plic.axi4_slave);

   function Vector #(CoreW_Bus_Num_Slaves, Bool) route (Bit #(Wd_Addr) addr);
      Vector #(CoreW_Bus_Num_Slaves, Bool) res = replicate(False);
      if (inRange(soc_map.m_mem0_controller_addr_range, addr))
        res[llc_slave_num] = True;
      else if (inRange(soc_map.m_plic_addr_range, addr))
        res[plic_slave_num] = True;
      else
        res[default_slave_num] = True;
      //Bit #(24) topBits = truncateLSB(addr); //XXX TODO Tag controller masks to 40 bits
      //if (topBits != 0) res = replicate(False);
      return res;
   endfunction

   mkAXI4Bus (route, master_vector, slave_vector, reset_by all_harts_reset);

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

`ifdef INCLUDE_GDB_CONTROL
   // ================================================================
   // Connect external debug module interface

   let dbgShim <- mkAXI4LiteShim (reset_by porReset);
   let dbgSub = dbgShim.slave;

   rule rl_debug_module_read_req;
      let arFlit <- get (dbgShim.master.ar);
      debug_module.dmi.read_addr (truncate (arFlit.araddr >> 2));
   endrule
   rule rl_debug_module_read_rsp;
      let x <- debug_module.dmi.read_data;
      dbgShim.master.r.put(AXI4Lite_RFlit { rdata: x, rresp: OKAY, ruser: ?});
   endrule
   rule rl_debug_module_write_req;
      let awFlit <- get (dbgShim.master.aw);
      let wFlit <- get (dbgShim.master.w);
      dbgShim.master.b.put(defaultValue);
      debug_module.dmi.write (truncate (awFlit.awaddr >> 2), wFlit.wdata);
   endrule

   let innerReset <- mkPulseWire (reset_by porReset);
   Reg #(UInt #(8)) ndm_reset_delay <- mkReg (0, reset_by porReset);
   Reg #(Bool) ndm_reset_restart_running <- mkReg (False, reset_by porReset);
   rule rl_debug_module_send_reset (ndm_reset_delay == 0);
      let restartRunning <- debug_module.ndm_reset_client.request.get;
      ndm_reset_delay <= 110;
      ndm_reset_restart_running <= restartRunning;
      innerReset.send;
   endrule
   rule rl_debug_module_count_reset_delay (ndm_reset_delay > 1);
      ndm_reset_delay <= ndm_reset_delay - 1;
   endrule
   rule rl_debug_module_ack_reset (ndm_reset_delay == 1);
      debug_module.ndm_reset_client.response.put (?);
      do_release (ndm_reset_restart_running, rg_tohost_addr);
      ndm_reset_delay <= 0;
   endrule
`else
   let dbgSub <- mkError_AXI4Lite_Slave (reset_by porReset);
   let innerReset <- mkPulseWire (reset_by porReset);
`endif

   // ================================================================
   // Connect external interrupts to the PLIC and Proc

   Vector #(t_n_irq, Reg #(Bool)) irq_reg
      <- replicateM (mkReg (False));
   Vector #(t_n_irq, Put #(Bool)) irq_ifc;
   for (Integer i = 0; i < valueof (t_n_irq); i = i + 1) begin
      irq_ifc [i] = interface Put;
         method put = writeReg (irq_reg[i]);
      endinterface;
      rule rl_connect_irq;
         plic.v_sources[i].m_interrupt_req (irq_reg[i]);
      endrule
   end

   let nmirq_reg <- mkReg (False);
   let nmirq_ifc = interface Put;
      method put = writeReg (nmirq_reg);
   endinterface;
   rule rl_connect_nmirq;
      proc.non_maskable_interrupt_req (nmirq_reg);
   endrule

   // ================================================================
   // Connect other control and status signals

   let f_ctrl_reqs <- mkFIFO1;
   let f_ctrl_rsps <- mkFIFO1;

   rule rl_ctrl_req;
      $display ("%0d: %m.rl_ctrl_req", cur_cycle);
      case (f_ctrl_reqs.first) matches
         tagged ReleaseReq: do_release (True, rg_tohost_addr);
         tagged ReleaseAndSetToHostAddrReq .tohost_addr:
           do_release (True, tohost_addr);
         tagged StatusReq: $display ("StatusReq not supported in Toooba");
      endcase
      f_ctrl_reqs.deq;
   endrule

   rule rl_ctrl_rsp;
      f_ctrl_rsps.enq (StatusRsp(?));
   endrule

   // ================================================================
   // INTERFACE

   let ifc = interface CoreW_IFC;
      // debug related signals
      // ---------------------
      interface debug_subordinate = dbgSub;

      // interrupt related signals
      // -------------------------
      interface irq = irq_ifc;
      interface nmirq = nmirq_ifc;

      // other control and status signals
      // --------------------------------
      interface controlStatusServer = toGPServer (f_ctrl_reqs, f_ctrl_rsps);

      // memory interfaces
      // -----------------
      // Cached master to Fabric master interface
      interface manager_0 = tag_controller_deburster.master;
      // Uncached master to Fabric master interface
      interface manager_1 = prepend_AXI4_Master_id
            (0, zero_AXI4_Master_user (uncached_mem_shim.master));
      interface subordinate_0 = subShim.slave;
   endinterface;

/*
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
*/

`ifdef RVFI_DII
   return tuple3 (innerReset, proc.rvfi_dii_server, ifc);
`else
   return tuple2 (innerReset, ifc);
`endif
endmodule: mkCoreW_reset

// ================================================================
// internal bus for this Core
// Masters: CPU DMem, Debug Module System Bus Access, External access

// ----------------
// Fabric port numbers for masters

CoreW_Bus_Master_Num cpu_uncached_master_num     = 0;
CoreW_Bus_Master_Num debug_module_sba_master_num = 1;
CoreW_Bus_Master_Num sub_ifc_master_num          = 2;

// ----------------
// Fabric port numbers for slaves

CoreW_Bus_Slave_Num default_slave_num = 0;    // for I/O, uncached memory, etc.
CoreW_Bus_Slave_Num plic_slave_num    = 1;    // PLIC mem-mapped registers
CoreW_Bus_Slave_Num llc_slave_num     = 2;    // Normal cached memory (connects to coherent Last-Level Cache)

// ================================================================

endpackage
