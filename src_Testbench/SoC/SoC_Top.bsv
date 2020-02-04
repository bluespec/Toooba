// Copyright (c) 2016-2020 Bluespec, Inc. All Rights Reserved.

package SoC_Top;

// ================================================================
// This package is the SoC "top-level".

// (Note: there will be further layer(s) above this for
//    simulation top-level, FPGA top-level, etc.)

// ================================================================
// Exports

export SoC_Top_IFC (..), mkSoC_Top;

// ================================================================
// BSV library imports

import FIFOF         :: *;
import GetPut        :: *;
import ClientServer  :: *;
import Connectable   :: *;
import Memory        :: *;
import Clocks        :: *;

// ----------------
// BSV additional libs

import Cur_Cycle   :: *;
import GetPut_Aux  :: *;

// ================================================================
// Project imports

// Main fabric
import AXI4_Types     :: *;
import AXI4_Fabric    :: *;
import AXI4_Deburster :: *;

import Fabric_Defs :: *;
import SoC_Map     :: *;
import SoC_Fabric  :: *;

// SoC components (CPU, mem, and IPs)

import CoreW_IFC :: *;
import CoreW     :: *;
import PLIC      :: *;    // For interface to PLIC interrupt sources, in CoreW_IFC

import Boot_ROM       :: *;
import Mem_Controller :: *;
import UART_Model     :: *;

`ifdef INCLUDE_CAMERA_MODEL
import Camera_Model   :: *;
`endif

`ifdef INCLUDE_ACCEL0
import AXI4_Accel_IFC :: *;
import AXI4_Accel     :: *;
`endif

`ifdef INCLUDE_TANDEM_VERIF
import TV_Info :: *;
`endif

`ifdef INCLUDE_GDB_CONTROL
import Debug_Module     :: *;
`endif

// ================================================================
// The outermost interface of the SoC

interface SoC_Top_IFC;
   // Set core's verbosity
   method Action  set_verbosity (Bit #(4)  verbosity, Bit #(64)  logdelay);

`ifdef INCLUDE_GDB_CONTROL
   // DMI (Debug Module Interface) facing remote debugger
   interface DMI dmi;

   // Non-Debug-Module Reset (reset all except DM)
   interface Client #(Bool, Bool) ndm_reset_client;
`endif

`ifdef INCLUDE_TANDEM_VERIF
   // To tandem verifier
   interface Get #(Info_CPU_to_Verifier) tv_verifier_info_get;
`endif

   // External real memory
   interface MemoryClient #(Bits_per_Raw_Mem_Addr, Bits_per_Raw_Mem_Word)  to_raw_mem;

   // UART0 to external console
   interface Get #(Bit #(8)) get_to_console;
   interface Put #(Bit #(8)) put_from_console;

   // Catch-all status; return-value can identify the origin (0 = none)
   (* always_ready *)
   method Bit #(8) status;

   // Start CPU execution
   // For ISA tests: watch memory writes to <tohost> addr
   method Action start (Fabric_Addr  tohost_addr, Fabric_Addr  fromhost_addr);
endinterface

// ================================================================
// Local types and constants

typedef enum {SOC_START,
	      SOC_RESETTING,
	      SOC_IDLE} SoC_State
deriving (Bits, Eq, FShow);

// ================================================================
// The module

(* synthesize *)
module mkSoC_Top #(Reset dm_power_on_reset)
                 (SoC_Top_IFC);
   Integer verbosity = 0;    // Normally 0; non-zero for debugging

   Reg #(SoC_State) rg_state <- mkReg (SOC_START);

   // SoC address map specifying base and limit for memories, IPs, etc.
   SoC_Map_IFC soc_map <- mkSoC_Map;

   // Core: CPU + Near_Mem_IO (CLINT) + PLIC + Debug module (optional) + TV (optional)
   // The Debug Module has its own RST_N reset signal (which comes
   // from outside this module as a paramter)
   CoreW_IFC #(N_External_Interrupt_Sources)  corew <- mkCoreW (dm_power_on_reset);

   // SoC Fabric
   Fabric_AXI4_IFC  fabric <- mkFabric_AXI4;

   // SoC Boot ROM
   Boot_ROM_IFC  boot_rom <- mkBoot_ROM;
   // AXI4 Deburster in front of Boot_ROM
   AXI4_Deburster_IFC #(Wd_Id,
			Wd_Addr,
			Wd_Data,
			Wd_User)  boot_rom_axi4_deburster <- mkAXI4_Deburster_A;

   // SoC Memory
   Mem_Controller_IFC  mem0_controller <- mkMem_Controller;
   // AXI4 Deburster in front of SoC Memory
   AXI4_Deburster_IFC #(Wd_Id,
			Wd_Addr,
			Wd_Data,
			Wd_User)  mem0_controller_axi4_deburster <- mkAXI4_Deburster_A;

   // SoC IPs
   UART_IFC   uart0  <- mkUART;

`ifdef INCLUDE_ACCEL0
   // Accel0 master to fabric
   AXI4_Accel_IFC  accel0 <- mkAXI4_Accel;
`endif

   // ----------------
   // SoC fabric master connections
   // Note: see 'SoC_Map' for 'master_num' definitions

   // CPU IMem master to fabric
   mkConnection (corew.cpu_imem_master,  fabric.v_from_masters [imem_master_num]);

   // CPU DMem master to fabric
   mkConnection (corew.cpu_dmem_master,  fabric.v_from_masters [dmem_master_num]);

`ifdef INCLUDE_ACCEL0
   // accel to fabric
   mkConnection (accel0.master,  fabric.v_from_masters [accel0_master_num]);
`endif

   // ----------------
   // SoC fabric slave connections
   // Note: see 'SoC_Map' for 'slave_num' definitions

   // Fabric to Deburster to Boot ROM
   mkConnection (fabric.v_to_slaves [boot_rom_slave_num], boot_rom_axi4_deburster.from_master);
   mkConnection (boot_rom_axi4_deburster.to_slave,        boot_rom.slave);

   // Fabric to Deburster to Mem Controller
   mkConnection (fabric.v_to_slaves [mem0_controller_slave_num], mem0_controller_axi4_deburster.from_master);
   mkConnection (mem0_controller_axi4_deburster.to_slave,        mem0_controller.slave);

   // Fabric to UART0
   mkConnection (fabric.v_to_slaves [uart0_slave_num],  uart0.slave);

`ifdef INCLUDE_ACCEL0
   // Fabric to accel0
   mkConnection (fabric.v_to_slaves [accel0_slave_num], accel0.slave);
`endif

`ifdef HTIF_MEMORY
   AXI4_Slave_IFC#(Wd_Id, Wd_Addr, Wd_Data, Wd_User) htif <- mkAxi4LRegFile(bytes_per_htif);

   mkConnection (fabric.v_to_slaves [htif_slave_num], htif);
`endif

   // ----------------
   // Connect interrupt sources for CPU external interrupt request inputs.

   (* fire_when_enabled, no_implicit_conditions *)
   rule rl_connect_external_interrupt_requests;
      Bool intr = uart0.intr;

      // UART
      corew.core_external_interrupt_sources [irq_num_uart0].m_interrupt_req (intr);
      Integer last_irq_num = irq_num_uart0;

`ifdef INCLUDE_ACCEL0
      Bool intr_accel0 = accel0.interrupt_req;
      core.core_external_interrupt_sources [irq_num_accel0].m_interrupt_req (intr_accel0);
      last_irq_num = irq_num_accel0;
`endif

      // Tie off remaining interrupt request lines (1..N)
      for (Integer j = last_irq_num + 1; j < valueOf (N_External_Interrupt_Sources); j = j + 1)
	 corew.core_external_interrupt_sources [j].m_interrupt_req (False);

      // Non-maskable interrupt request. [Tie-off; TODO: connect to genuine sources]
      corew.nmi_req (False);
   endrule

   // ================================================================
   // MODULE INITIALIZATIONS

   function Action fa_reset_start_actions;
      action
	 mem0_controller.server_reset.request.put (?);
	 uart0.server_reset.request.put (?);
	 fabric.reset;
      endaction
   endfunction

   function Action fa_reset_complete_actions;
      action
	 let mem0_controller_rsp <- mem0_controller.server_reset.response.get;
	 let uart0_rsp           <- uart0.server_reset.response.get;

	 // Initialize address maps of slave IPs
	 boot_rom.set_addr_map (soc_map.m_boot_rom_addr_base,
				soc_map.m_boot_rom_addr_lim);

	 mem0_controller.set_addr_map (soc_map.m_mem0_controller_addr_base,
				       soc_map.m_mem0_controller_addr_lim);

	 uart0.set_addr_map (soc_map.m_uart0_addr_base, soc_map.m_uart0_addr_lim);

`ifdef INCLUDE_ACCEL0
	 accel0.init (fabric_default_id,
		      soc_map.m_accel0_addr_base,
		      soc_map.m_accel0_addr_lim);
`endif

	 if (verbosity != 0) begin
	    $display ("  SoC address map:");
	    $display ("  Boot ROM:        0x%0h .. 0x%0h",
		      soc_map.m_boot_rom_addr_base,
		      soc_map.m_boot_rom_addr_lim);
	    $display ("  Mem0 Controller: 0x%0h .. 0x%0h",
		      soc_map.m_mem0_controller_addr_base,
		      soc_map.m_mem0_controller_addr_lim);
	    $display ("  UART0:           0x%0h .. 0x%0h",
		      soc_map.m_uart0_addr_base,
		      soc_map.m_uart0_addr_lim);
	 end
      endaction
   endfunction

   // ----------------
   // Initial reset

   rule rl_reset_start_initial (rg_state == SOC_START);
      fa_reset_start_actions;
      rg_state <= SOC_RESETTING;

      $display ("%0d: %m.rl_reset_start_initial ...", cur_cycle);
   endrule

   rule rl_reset_complete_initial (rg_state == SOC_RESETTING);
      fa_reset_complete_actions;
      rg_state <= SOC_IDLE;

      $display ("%0d: %m.rl_reset_complete_initial", cur_cycle);
   endrule

   // ================================================================
   // INTERFACE

   method Action  set_verbosity (Bit #(4)  verbosity1, Bit #(64)  logdelay);
      corew.set_verbosity (verbosity1, logdelay);
   endmethod

   // To external controller (E.g., GDB)
`ifdef INCLUDE_GDB_CONTROL
   // DMI (Debug Module Interface) facing remote debugger
   interface DMI dmi = corew.dmi;

   // Non-Debug-Module Reset (reset all except DM)
   interface Client ndm_reset_client = corew.ndm_reset_client;
`endif

`ifdef INCLUDE_TANDEM_VERIF
   // To tandem verifier
   interface tv_verifier_info_get = corew.tv_verifier_info_get;
`endif

   // External real memory
   interface to_raw_mem = mem0_controller.to_raw_mem;

   // UART to external console
   interface get_to_console   = uart0.get_to_console;
   interface put_from_console = uart0.put_from_console;

   // Catch-all status; return-value can identify the origin (0 = none)
   method Bit #(8) status;
      return mem0_controller.status;
   endmethod

   // Start CPU execution
   // For ISA tests: watch memory writes to <tohost> addr
   method Action start (Fabric_Addr  tohost_addr, Fabric_Addr  fromhost_addr);
      Bool watch_tohost = (tohost_addr != 0);
      mem0_controller.set_watch_tohost (watch_tohost, tohost_addr);
      corew.start (tohost_addr, fromhost_addr);
      $display ("%0d: %m.method start (tohost %0h, fromhost %0h)",
		cur_cycle, tohost_addr, fromhost_addr);
   endmethod
endmodule: mkSoC_Top

// ================================================================
// Specialization of parameterized AXI4 Deburster for this SoC.

(* synthesize *)
module mkAXI4_Deburster_A (AXI4_Deburster_IFC #(Wd_Id,
						Wd_Addr,
						Wd_Data,
						Wd_User));
   let m <- mkAXI4_Deburster;
   return m;
endmodule

// ================================================================

endpackage
