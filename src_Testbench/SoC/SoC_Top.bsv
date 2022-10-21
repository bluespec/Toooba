// Copyright (c) 2016-2020 Bluespec, Inc. All Rights Reserved.

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
import Vector        :: *;

// ----------------
// BSV additional libs

import Cur_Cycle   :: *;
import GetPut_Aux  :: *;
import Routable    :: *;
import AXI4        :: *;
import AXI4Lite    :: *;

// ================================================================
// Project imports

import Fabric_Defs :: *;
import SoC_Map     :: *;

// SoC components (CPU, mem, and IPs)

import WindCoreInterface :: *;
import CoreW     :: *;
import PLIC      :: *;

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

`ifdef RVFI_DII
import RVFI_DII_Types :: *;
import ProcTypes :: *;
`endif

`ifdef INCLUDE_GDB_CONTROL
import Debug_Module     :: *;
`endif

// ================================================================
// The outermost interface of the SoC

interface SoC_Top_IFC;

`ifdef INCLUDE_GDB_CONTROL
  interface AXI4Lite_Slave #(21, 32, 0, 0, 0, 0, 0) debug_subordinate;
`endif

`ifdef INCLUDE_TANDEM_VERIF
   // To tandem verifier
   interface Get #(Info_CPU_to_Verifier) tv_verifier_info_get;
`elsif RVFI_DII
   interface Toooba_RVFI_DII_Server rvfi_dii_server;
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
   Integer verbosity = 2;    // Normally 0; non-zero for debugging

   Reg #(SoC_State) rg_state <- mkReg (SOC_START);

   // SoC address map specifying base and limit for memories, IPs, etc.
   SoC_Map_IFC soc_map <- mkSoC_Map;

   // Core: CPU + Near_Mem_IO (CLINT) + PLIC + Debug module (optional) + TV (optional)
   // The Debug Module has its own RST_N reset signal (which comes
   // from outside this module as a paramter)
`ifdef RVFI_DII
   match {.core_rvfi_dii_server , .coreifc} <- mkCoreW;
   CoreW_IFC #(N_External_Interrupt_Sources) corew = coreifc;
`else
   CoreW_IFC #(N_External_Interrupt_Sources) corew <- mkCoreW;
`endif

   // SoC Boot ROM
   Boot_ROM_IFC  boot_rom <- mkBoot_ROM;
   // AXI4 Deburster in front of Boot_ROM
   AXI4_Shim#(Wd_SId, Wd_Addr, Wd_Data, 0, 0, 0, 0, 0)
      boot_rom_axi4_deburster <- mkBurstToNoBurst;

   // SoC Memory
   Mem_Controller_IFC  mem0_controller <- mkMem_Controller;
   // AXI4 Deburster in front of SoC Memory
   AXI4_Shim#(Wd_SId, Wd_Addr, Wd_Data, 0, 0, 0, 0, 0)
      mem0_controller_axi4_deburster <- mkBurstToNoBurst;

   // SoC IPs
   UART_IFC   uart0  <- mkUART;

`ifdef INCLUDE_ACCEL0
   // Accel0 master to fabric
   AXI4_Accel_IFC  accel0 <- mkAXI4_Accel;
`endif

   // ----------------
   // SoC fabric master connections
   // Note: see 'SoC_Map' for 'master_num' definitions

   Vector#(Num_Masters, AXI4_Master #(TAdd#(Wd_MId,1), Wd_Addr, Wd_Data,
                                      0, 0, 0, 0, 0))
      master_vector = newVector;

   // CPU IMem master to fabric
   master_vector[imem_master_num] = corew.manager_0;

   // CPU DMem master to fabric
   master_vector[dmem_master_num] = corew.manager_1;

   // ----------------
   // SoC fabric slave connections
   // Note: see 'SoC_Map' for 'slave_num' definitions

   Vector#(Num_Slaves, AXI4_Slave #(Wd_SId, Wd_Addr, Wd_Data,
                                    0, 0, 0, 0, 0))
      slave_vector = newVector;
   Vector#(Num_Slaves, Range#(Wd_Addr)) route_vector = newVector;

   // Fabric to Boot ROM
   mkConnection(boot_rom_axi4_deburster.master, boot_rom.slave);
   slave_vector[boot_rom_slave_num] = boot_rom_axi4_deburster.slave;
   route_vector[boot_rom_slave_num] = soc_map.m_boot_rom_addr_range;

   // Fabric to Mem Controller
   mkConnection(mem0_controller_axi4_deburster.master, mem0_controller.slave);
   slave_vector[mem0_controller_slave_num] = mem0_controller_axi4_deburster.slave;
   route_vector[mem0_controller_slave_num] = soc_map.m_mem0_controller_addr_range;

   // Fabric to UART0
   slave_vector[uart0_slave_num] = zero_AXI4_Slave_user(uart0.slave);
   route_vector[uart0_slave_num] = soc_map.m_uart0_addr_range;

`ifdef INCLUDE_ACCEL0
   // Fabric to accel0
   slave_vector[accel0_slave_num] = zero_AXI4_Slave_user(accel0.slave);
   route_vector[accel0_slave_num] = soc_map.m_accel0_addr_range;
`endif

`ifdef HTIF_MEMORY
   AXI4_Slave_IFC#(Wd_Id, Wd_Addr, Wd_Data, Wd_User) htif <- mkAxi4LRegFile(bytes_per_htif);

   slave_vector[htif_slave_num] = htif;
   route_vector[htif_slave_num] = soc_map.m_htif_addr_range;
`endif

   // SoC Fabric
   let bus <- mkAXI4Bus (routeFromMappingTable(route_vector),
                         master_vector, slave_vector);

   // ----------------
   // Connect interrupt sources for CPU external interrupt request inputs.

   (* fire_when_enabled, no_implicit_conditions *)
   rule rl_connect_external_interrupt_requests;
      Bool intr = uart0.intr;

      // UART
      corew.irq [irq_num_uart0].put (intr);
      Integer last_irq_num = irq_num_uart0;

`ifdef INCLUDE_ACCEL0
      Bool intr_accel0 = accel0.interrupt_req;
      corew.irq [irq_num_accel0].put (intr_accel0);
      last_irq_num = irq_num_accel0;
`endif

      // Tie off remaining interrupt request lines (1..N)
      for (Integer j = last_irq_num + 1; j < valueOf (N_External_Interrupt_Sources); j = j + 1)
         corew.irq [j].put (False);

      // Non-maskable interrupt request. [Tie-off; TODO: connect to genuine sources]
      corew.nmirq.put (False);
   endrule

   // ================================================================
   // MODULE INITIALIZATIONS

   function Action fa_reset_start_actions;
      action
         mem0_controller.server_reset.request.put (?);
         uart0.server_reset.request.put (?);
      endaction
   endfunction

   function Action fa_reset_complete_actions;
      action
         let mem0_controller_rsp <- mem0_controller.server_reset.response.get;
         let uart0_rsp           <- uart0.server_reset.response.get;
         // Initialize address maps of slave IPs
         boot_rom.set_addr_map (rangeBase(soc_map.m_boot_rom_addr_range),
                                rangeTop(soc_map.m_boot_rom_addr_range));

         mem0_controller.set_addr_map (rangeBase(soc_map.m_mem0_controller_addr_range),
                                       rangeTop(soc_map.m_mem0_controller_addr_range));

         uart0.set_addr_map (rangeBase(soc_map.m_uart0_addr_range),
                             rangeTop(soc_map.m_uart0_addr_range));

`ifdef INCLUDE_ACCEL0
         accel0.init (fabric_default_id,
                      soc_map.m_accel0_addr_range.base,
                      rangeTop(soc_map.m_accel0_addr_range));
`endif

         if (verbosity != 0) begin
            $display ("  SoC address map:");
            $display ("  Boot ROM:        0x%0h .. 0x%0h",
                      rangeBase(soc_map.m_boot_rom_addr_range),
                      rangeTop(soc_map.m_boot_rom_addr_range));
            $display ("  Mem0 Controller: 0x%0h .. 0x%0h",
                      rangeBase(soc_map.m_mem0_controller_addr_range),
                      rangeTop(soc_map.m_mem0_controller_addr_range));
            $display ("  UART0:           0x%0h .. 0x%0h",
                      rangeBase(soc_map.m_uart0_addr_range),
                      rangeTop(soc_map.m_uart0_addr_range));
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

   // To external controller (E.g., GDB)
`ifdef INCLUDE_GDB_CONTROL
  interface debug_subordinate = corew.debug_subordinate;
`endif

`ifdef INCLUDE_TANDEM_VERIF
   // To tandem verifier
   interface tv_verifier_info_get = corew.tv_verifier_info_get;
`elsif RVFI_DII
   interface rvfi_dii_server = core_rvfi_dii_server;
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
   method Action start (Fabric_Addr tohost_addr, Fabric_Addr fromhost_addr)
      if (rg_state == SOC_IDLE);
      Bool watch_tohost = (tohost_addr != 0);
      mem0_controller.set_watch_tohost (watch_tohost, tohost_addr);
      Bool is_running = True;
      corew.controlStatusServer.request.put
        (ReleaseAndSetToHostAddrReq (tohost_addr));
      $display ("%0d: %m.method start (tohost %0h, fromhost %0h)",
                cur_cycle, tohost_addr, fromhost_addr);
   endmethod
endmodule: mkSoC_Top

// ================================================================

endpackage
