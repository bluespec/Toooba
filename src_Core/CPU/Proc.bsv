package Proc;

// Copyright (c) 2018 Massachusetts Institute of Technology
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

// Portions Copyright (c) 2019 Bluespec, Inc.

// ================================================================
// BSV lib imports

import Assert       :: *;
import Vector       :: *;
import GetPut       :: *;
import ClientServer :: *;
import Connectable  :: *;
import FIFOF        :: *;
import ConfigReg    :: *;

// ----------------
// BSV additional libs

import Cur_Cycle      :: *;
import GetPut_Aux     :: *;

// ================================================================
// Project imports

// ----------------
// From MIT RISCY-OOO

import Types::*;
import ProcTypes::*;
import L1CoCache::*;
import L2Tlb::*;
import CCTypes::*;
import CacheUtils::*;
import LLCache::*;
import MemLoader::*;
import L1LLConnect::*;
import LLCDmaConnect::*;
import MMIOAddrs::*;
import MMIOCore::*;
import DramCommon::*;
import Performance::*;

// ----------------
// From Tooba

import ISA_Decls  :: *;

import Core              :: *;
import Proc_IFC          :: *;
import MMIOPlatform      :: *;
import LLC_AXI4_Adapter  :: *;
import MMIO_AXI4_Adapter :: *;

import SoC_Map      :: *;
import AXI4_Types   :: *;
import Fabric_Defs  :: *;

`ifdef INCLUDE_TANDEM_VERIF
import TV_Info  :: *;
`endif

`ifdef INCLUDE_GDB_CONTROL
import DM_CPU_Req_Rsp  :: *;
`endif

// ================================================================
// CPU run-states
// TODO: Reset from GDB etc.

typedef enum {CPU_RUNNING                 // Normal operation
`ifdef INCLUDE_GDB_CONTROL
	      ,
              CPU_ENTERING_DEBUG_MODE,    // On GDB breakpoint, while waiting for fence completion
	      CPU_DEBUG_MODE              // Halted for debugger
`endif
   } CPU_State
deriving (Eq, Bits, FShow);

function Bool fn_is_running (CPU_State  cpu_state);
   return (cpu_state == CPU_RUNNING);
endfunction

// ================================================================

(* synthesize *)
module mkProc (Proc_IFC);

   // ----------------
    // cores
    Vector#(CoreNum, Core) core = ?;
    for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
        core[i] <- mkCore(fromInteger(i));
    end

   // ----------------
   // Verbosity control for debugging

   // Verbosity: 0=quiet; 1=instruction trace; 2=more detail
   Reg #(Bit #(4))  cfg_verbosity <- mkConfigReg (0);

   // ----------------
   // CPU run/debug states

   Reg #(CPU_State)  rg_state <- mkReg (CPU_RUNNING);

   // ----------------
   // Reset requests and responses    (TODO: to be implemented)

   FIFOF #(Bit #(0))  f_reset_reqs <- mkFIFOF;
   FIFOF #(Bit #(0))  f_reset_rsps <- mkFIFOF;

`ifdef INCLUDE_GDB_CONTROL
   // ----------------
   // Communication to/from External debug module    (TODO: to be implemented)

   // Debugger run-control
   FIFOF #(Bool)  f_run_halt_reqs <- mkFIFOF;
   FIFOF #(Bool)  f_run_halt_rsps <- mkFIFOF;

   // Debugger GPR read/write request/response
   FIFOF #(DM_CPU_Req #(5,  XLEN)) f_gpr_reqs <- mkFIFOF1;
   FIFOF #(DM_CPU_Rsp #(XLEN))     f_gpr_rsps <- mkFIFOF1;

`ifdef ISA_F
   // Debugger FPR read/write request/response
   FIFOF #(DM_CPU_Req #(5,  FLEN)) f_fpr_reqs <- mkFIFOF1;
   FIFOF #(DM_CPU_Rsp #(FLEN))     f_fpr_rsps <- mkFIFOF1;
`endif

   // Debugger CSR read/write request/response
   FIFOF #(DM_CPU_Req #(12, XLEN)) f_csr_reqs <- mkFIFOF1;
   FIFOF #(DM_CPU_Rsp #(XLEN))     f_csr_rsps <- mkFIFOF1;

`endif

   // ----------------
   // Tandem Verification    (TODO: to be implemented)

`ifdef INCLUDE_TANDEM_VERIF
   FIFOF #(Trace_Data) f_trace_data  <- mkFIFOF;
`endif

   // ----------------
   // MMIO

   MMIO_AXI4_Adapter_IFC mmio_axi4_adapter <- mkMMIO_AXI4_Adapter;

   // MMIO platform
   Vector#(CoreNum, MMIOCoreToPlatform) mmioToP;
   for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
      mmioToP[i] = core[i].mmioToPlatform;
   end
   MMIOPlatform mmioPlatform <- mkMMIOPlatform (mmioToP,
						mmio_axi4_adapter.core_side);

   // last level cache
   LLCache llc <- mkLLCache;

   // connect LLC to L1 caches
   Vector#(L1Num, ChildCacheToParent#(L1Way, void)) l1 = ?;
   for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
      l1[i] = core[i].dCacheToParent;
      l1[i + valueof(CoreNum)] = core[i].iCacheToParent;
   end
   mkL1LLConnect(llc.to_child, l1);

   // ================================================================
   // LLC's DMA connections

    // Core's tlbToMem
    Vector#(CoreNum, TlbMemClient) tlbToMem = ?;
    for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
        tlbToMem[i] = core[i].tlbToMem;
    end

   // Stub out memLoader (TODO: can be Debug Module's access)
   let memLoaderStub = interface MemLoaderMemClient;
			  interface memReq = nullFifoDeq;
			  interface respSt = nullFifoEnq;
		       endinterface;

    mkLLCDmaConnect(llc.dma, memLoaderStub, tlbToMem);

   // ================================================================
   // interface LLC to AXI4

   LLC_AXI4_Adapter_IFC  llc_axi4_adapter <- mkLLC_AXi4_Adapter (llc.to_mem);

   // ================================================================
   // Connect stats

   for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
      rule broadcastStats;
         Bool doStats <- core[i].sendDoStats;
         for(Integer j = 0; j < valueof(CoreNum); j = j+1) begin
	    core[j].recvDoStats(doStats);
         end
         llc.perf.setStatus(doStats);
      endrule
   end

   // ================================================================
   // Stub out deadlock and renameDebug interfaces

   for(Integer j = 0; j < valueof(CoreNum); j = j+1) begin
      rule rl_dummy1;
	 let x <- core[j].deadlock.dCacheCRqStuck.get;
      endrule
      rule rl_dummy2;
	 let x <- core[j].deadlock.dCachePRqStuck.get;
      endrule
      rule rl_dummy3;
	 let x <- core[j].deadlock.iCacheCRqStuck.get;
      endrule
      rule rl_dummy4;
	 let x <- core[j].deadlock.iCachePRqStuck.get;
      endrule
      rule rl_dummy5;
	 let x <- core[j].deadlock.renameInstStuck.get;
      endrule
      rule rl_dummy6;
	 let x <- core[j].deadlock.renameCorrectPathStuck.get;
      endrule
      rule rl_dummy7;
	 let x <- core[j].deadlock.commitInstStuck.get;
      endrule
      rule rl_dummy8;
	 let x <- core[j].deadlock.commitUserInstStuck.get;
      endrule
      rule rl_dummy9;
	 let x <- core[j].deadlock.checkStarted.get;
      endrule

      rule rl_dummy20;
	 let x <- core[j].renameDebug.renameErr.get;
      endrule
   end

   // ================================================================
   // Reset

   rule rl_reset;
      let x <- pop (f_reset_reqs);

      llc_axi4_adapter.reset;
      mmio_axi4_adapter.reset;

      f_reset_rsps.enq (?);
   endrule

   // ----------------
   // Termination detection

   for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
      rule rl_terminate;
	 let x <- core[i].coreIndInv.terminate;
	 $display ("Core %d terminated", i);
      endrule
   end

   // Print out values written 'tohost'
   rule rl_tohost;
      let x <- mmioPlatform.to_host;
      $display ("%0d: mmioPlatform.rl_tohost: 0x%0x (= %0d)", cur_cycle, x, x);
      if (x != 0) begin
	 // Standard RISC-V ISA tests finish by writing a value tohost with x[0]==1.
	 // Further when x[63:1]==0, all tests within the program pass,
	 // otherwise x[63:1] = the test within the program that failed.
	 let failed_testnum = (x >> 1);
	 if (failed_testnum == 0)
	    $display ("PASS");
	 else
	    $display ("FAIL %0d", failed_testnum);
	 $finish (0);
      end
   endrule

   // ================================================================
   // ================================================================
   // ================================================================
   // DEBUGGER ACCESS

`ifdef INCLUDE_GDB_CONTROL

   // ----------------
   // Debug Module Run (resume) control

   // Run command when in debug mode
   rule rl_debug_run ((f_run_halt_reqs.first == True)
		      && (! f_gpr_reqs.notEmpty)
		      && (! f_fpr_reqs.notEmpty)
		      && (! f_csr_reqs.notEmpty)
		      && (rg_state == CPU_DEBUG_MODE));
      // if (cfg_verbosity > 1)
	 $display ("%0d: %m.rl_debug_run", cur_cycle);

      f_run_halt_reqs.deq;
      core[0].debug_resume;
      rg_state <= CPU_RUNNING;

      // Notify debugger that we've started running
      f_run_halt_rsps.enq (True);
   endrule

   // Run command when already running
   rule rl_debug_run_redundant ((f_run_halt_reqs.first == True)
				&& (! f_gpr_reqs.notEmpty)
				&& (! f_fpr_reqs.notEmpty)
				&& (! f_csr_reqs.notEmpty)
				&& fn_is_running (rg_state));
      // if (cfg_verbosity > 1)
	 $display ("%0d: %m.rl_debug_run_redundant", cur_cycle);

      f_run_halt_reqs.deq;

      // Notify debugger that we're running
      f_run_halt_rsps.enq (True);
   endrule

   // ----------------
   // Debug Module Halt control

   rule rl_debug_halt ((f_run_halt_reqs.first == False) && fn_is_running (rg_state));
      // if (cfg_verbosity > 1)
	 $display ("%0d: %m.rl_debug_halt", cur_cycle);

      f_run_halt_reqs.deq;

      // Debugger 'halt' request (e.g., GDB '^C' command)
      // This is just like an interrupt.
      core[0].debug_halt;
   endrule

   // Monitors when we've reached halted state while running (halt,
   // step or EBREAK) and notifies DM
   rule rl_debug_halted (fn_is_running (rg_state) && core [0].is_debug_halted);
      // Notify debugger that we've halted
      f_run_halt_rsps.enq (False);
      // Stop executing rules until ready to restart from debugger
      rg_state <= CPU_DEBUG_MODE;

      // if (cfg_verbosity > 1)
	 $display ("%0d: %m.rl_debug_halted", cur_cycle);
   endrule

   rule rl_debug_halt_redundant ((f_run_halt_reqs.first == False) && (! fn_is_running (rg_state)));
      // if (cfg_verbosity > 1)
	 $display ("%0d: %m.rl_debug_halt_redundant", cur_cycle);

      f_run_halt_reqs.deq;

      // Notify debugger that we've 'halted'
      f_run_halt_rsps.enq (False);

      $display ("%0d: %m.rl_debug_halt_redundant: CPU already halted; state = ",
		cur_cycle, fshow (rg_state));
   endrule

   // ----------------
   // Debug Module CSR read/write

   rule rl_debug_csr_read ((rg_state == CPU_DEBUG_MODE) && (! f_csr_reqs.first.write));
      let req <- pop (f_csr_reqs);
      Bit #(12) csr_addr = req.address;
      let data = core [0].csr_read (csr_addr);
      let rsp = DM_CPU_Rsp {ok: True, data: data};
      f_csr_rsps.enq (rsp);
      // if (cur_verbosity > 1)
	 $display ("%m.rl_debug_read_csr: csr %0d => 0x%0h",
		   csr_addr, data);
   endrule

   rule rl_debug_csr_write ((rg_state == CPU_DEBUG_MODE) && f_csr_reqs.first.write);
      let req <- pop (f_csr_reqs);
      Bit #(12) csr_addr = req.address;
      let data = req.data;
      core [0].csr_write (csr_addr, data);
      let rsp = DM_CPU_Rsp {ok: True, data: ?};
      f_csr_rsps.enq (rsp);

      // if (cur_verbosity > 1)
	 $display ("%m.rl_debug_write_csr: csr 0x%0h <= 0x%0h", csr_addr, data);
   endrule

   rule rl_debug_csr_access_busy (rg_state != CPU_DEBUG_MODE);
      let req <- pop (f_csr_reqs);
      let rsp = DM_CPU_Rsp {ok: False, data: ?};
      f_csr_rsps.enq (rsp);

      // if (cur_verbosity > 1)
	 $display ("%m.rl_debug_csr_access_busy");
   endrule

   // ----------------
   // Debug Module GPR read/write

   rule rl_debug_read_gpr ((rg_state == CPU_DEBUG_MODE) && (! f_gpr_reqs.first.write));
      let req <- pop (f_gpr_reqs);
      Bit #(5) regname = req.address;

      let data = core [0].gpr_read (regname);

      let rsp = DM_CPU_Rsp {ok: True, data: data};
      f_gpr_rsps.enq (rsp);
      if (cur_verbosity > 1)
	 $display ("%0d: %m.rl_debug_read_gpr: reg %0d => 0x%0h",
		   mcycle, regname, data);
   endrule

   rule rl_debug_write_gpr ((rg_state == CPU_DEBUG_MODE) && f_gpr_reqs.first.write);
      let req <- pop (f_gpr_reqs);
      Bit #(5) regname = req.address;
      let data = req.data;
      core [0].gpr_write (regname, data);

      let rsp = DM_CPU_Rsp {ok: True, data: ?};
      f_gpr_rsps.enq (rsp);

      if (cur_verbosity > 1)
	 $display ("%0d: %m.rl_debug_write_gpr: reg %0d <= 0x%0h",
		   mcycle, regname, data);
   endrule

   rule rl_debug_gpr_access_busy (rg_state != CPU_DEBUG_MODE);
      let req <- pop (f_gpr_reqs);
      let rsp = DM_CPU_Rsp {ok: False, data: ?};
      f_gpr_rsps.enq (rsp);

      if (cur_verbosity > 1) $display ("%0d: %m.rl_debug_gpr_access_busy", mcycle);
   endrule

   // ----------------
   // Debug Module FPR read/write

`ifdef ISA_F
   rule rl_debug_read_fpr ((rg_state == CPU_DEBUG_MODE) && (! f_fpr_reqs.first.write));
      let req <- pop (f_fpr_reqs);
      Bit #(5) regname = req.address;
      let data = core [0].fpr_read (regname);
      let rsp = DM_CPU_Rsp {ok: True, data: data};
      f_fpr_rsps.enq (rsp);
      if (cur_verbosity > 1)
	 $display ("%0d: %m.rl_debug_read_fpr: reg %0d => 0x%0h",
		   mcycle, regname, data);
   endrule

   rule rl_debug_write_fpr ((rg_state == CPU_DEBUG_MODE) && f_fpr_reqs.first.write);
      let req <- pop (f_fpr_reqs);
      Bit #(5) regname = req.address;
      let data = req.data;
      core [0].fpr_write (regname, data);

      let rsp = DM_CPU_Rsp {ok: True, data: ?};
      f_fpr_rsps.enq (rsp);

      if (cur_verbosity > 1)
	 $display ("%0d: %m.rl_debug_write_fpr: reg %0d <= 0x%0h",
		   mcycle, regname, data);
   endrule

   rule rl_debug_fpr_access_busy (rg_state != CPU_DEBUG_MODE);
      let req <- pop (f_fpr_reqs);
      let rsp = DM_CPU_Rsp {ok: False, data: ?};
      f_fpr_rsps.enq (rsp);

      if (cur_verbosity > 1)
	 $display ("%0d: %m.rl_debug_fpr_access_busy", mcycle);
   endrule
`endif

`endif

   // ================================================================
   // ================================================================
   // ================================================================
   // INTERFACE

   // Reset
   interface Server  hart0_server_reset = toGPServer (f_reset_reqs, f_reset_rsps);

   // ----------------
   // Start the cores running
   method Action start (Addr startpc, Addr tohostAddr, Addr fromhostAddr);
      action
	 for(Integer i = 0; i < valueof(CoreNum); i = i+1)
	    core[i].coreReq.start (startpc, tohostAddr, fromhostAddr);
      endaction

      mmioPlatform.start (tohostAddr, fromhostAddr);

      $display ("Proc.start: startpc = 0x%0h, tohostAddr = 0x%0h, fromhostAddr = %0h",
		startpc, tohostAddr, fromhostAddr);
   endmethod

   // ----------------
   // SoC fabric connections

   // Fabric master interface for memory (from LLC)
   interface  master0 = llc_axi4_adapter.mem_master;

   // Fabric master interface for IO (from MMIOPlatform)
   interface  master1 = mmio_axi4_adapter.mmio_master;

   // ----------------
   // External interrupts

   method Action  m_external_interrupt_req (x);
      core[0].setMEIP (pack (x));
   endmethod

   method Action  s_external_interrupt_req (x);
      core[0].setSEIP (pack (x));
   endmethod

   // ----------------
   // External interrupt [14] to go into Debug Mode

   method Action  debug_external_interrupt_req (Bool set_not_clear);
      core[0].setDEIP (pack (set_not_clear));
   endmethod

   // ----------------
   // Non-maskable interrupt

   // TODO: fixup: NMIs should send CPU to an NMI vector (TBD in SoC_Map)
   method Action  non_maskable_interrupt_req (Bool set_not_clear) = noAction;

   // ----------------
   // For tracing

   method Action  set_verbosity (Bit #(4)  verbosity);
      cfg_verbosity <= verbosity;
   endmethod

   // ----------------
   // Optional interface to Tandem Verifier

`ifdef INCLUDE_TANDEM_VERIF
   interface Get  trace_data_out = toGet (f_trace_data);
`endif

   // ----------------
   // Optional interface to Debug Module

`ifdef INCLUDE_GDB_CONTROL
   // run-control, other
   interface Server  hart0_server_run_halt = toGPServer (f_run_halt_reqs, f_run_halt_rsps);

   interface Put  hart0_put_other_req;
      method Action  put (Bit #(4) req);
	 cfg_verbosity <= req;
      endmethod
   endinterface

   // GPR access
   interface Server  hart0_gpr_mem_server = toGPServer (f_gpr_reqs, f_gpr_rsps);

`ifdef ISA_F
   // FPR access
   interface Server  hart0_fpr_mem_server = toGPServer (f_fpr_reqs, f_fpr_rsps);
`endif

   // CSR access
   interface Server  hart0_csr_mem_server = toGPServer (f_csr_reqs, f_csr_rsps);
`endif

endmodule: mkProc

// ================================================================

endpackage
