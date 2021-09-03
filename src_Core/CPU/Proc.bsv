package Proc;

// Copyright (c) 2018 Massachusetts Institute of Technology
// Portions Copyright (c) 2019-2021 Bluespec, Inc.
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

// ================================================================
// BSV lib imports

import Assert       :: *;
import Vector       :: *;
import GetPut       :: *;
import ClientServer :: *;
import Connectable  :: *;
import FIFO         :: *;
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
// From Toooba

import ISA_Decls  :: *;

import SoC_Map      :: *;
import AXI4_Types   :: *;
import AXI_Widths   :: *;
import Fabric_Defs  :: *;

import CPU       :: *;    // Was 'Core' in RISCY_OOO
import Proc_IFC  :: *;

import DMA_Cache         :: *;
import L1_IFC_Adapter    :: *;
import MMIO              :: *;
import MMIO_AXI4_Adapter :: *;

import MMIOPlatform              :: *;
import MMIOPlatform_AXI4_Adapter :: *;

`ifdef MEM_512b
import LLC_AXI4_Adapter_2 :: *;
`else
import LLC_AXI4_Adapter   :: *;
`endif

`ifdef INCLUDE_GDB_CONTROL
import DM_CPU_Req_Rsp  :: *;
`endif

`ifdef INCLUDE_TANDEM_VERIF
import ProcTypes   :: *;
import Trace_Data2 :: *;
`endif

// ================================================================

(* synthesize *)
module mkProc (Proc_IFC);

   Integer verbosity_mmio_axi4_adapter = 0;
   Integer verbosity_DMA_L1_L2         = 0;

`ifdef OPTION_DMA_CACHE
   messageM ("\nINFO: mkProc: instantiating DMA Cache connecting to L2/LLC");
`else
   messageM ("\nINFO: mkProc: omitting DMA Cache");
`endif

   // ----------------
   // cores
   Vector#(CoreNum, Core) core = ?;
   for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
      core[i] <- mkCPU(fromInteger(i));
   end

   // ----------------
   // MMIO

   MMIOPlatform_AXI4_Adapter_IFC  mmioplatform_axi4_adapter
                                  <- mkMMIOPlatform_AXI4_Adapter;

   // MMIO platform
   Vector#(CoreNum, MMIOCoreToPlatform) mmioToP;
   for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
      mmioToP[i] = core[i].mmioToPlatform;
   end
   MMIOPlatform mmioPlatform <- mkMMIOPlatform (mmioToP,
						mmioplatform_axi4_adapter.core_side);

`ifdef OPTION_DMA_CACHE
   DMA_Cache_IFC  dma_cache  <- mkDMA_Cache;

   // Adapter (vector size 1) for MMIO interface of dma_cache to AXI4
   MMIO_AXI4_Adapter_IFC #(1)
       mmio_axi4_adapter <- mkMMIO_AXI4_Adapter (fromInteger (verbosity_mmio_axi4_adapter));

   mkConnection (dma_cache.mmio_client,   mmio_axi4_adapter.v_mmio_server [0]);
`endif

   // last level cache
   LLCache llc <- mkLLCache;

   // ----------------
   // connect core L1 I- and D-caches to LLC
   Vector #(L1Num, ChildCacheToParent#(L1Way, void)) l1 = ?;
   for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
      l1[i]                    = core[i].dCacheToParent;
      l1[i + valueof(CoreNum)] = core[i].iCacheToParent;
   end

`ifdef OPTION_DMA_CACHE
   // Instantiate L1_IFC adapter connected to dma_cache
   Integer l1_client_id = 2 * valueOf (CoreNum);
   let ifc_DMA_L1 <- mkL1_IFC_Adapter (verbosity_DMA_L1_L2,
				       l1_client_id,
				       dma_cache.l1_to_l2_client,
				       dma_cache.l2_to_l1_server);
   // and connect it to LLC
   l1 [fromInteger (l1_client_id)] = ifc_DMA_L1;
`endif

   mkL1LLConnect(llc.to_child, l1);

   // ================================================================
   // LLC's DMA connections

    // Core's tlbToMem
    Vector#(CoreNum, TlbMemClient) tlbToMem = ?;
    for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
        tlbToMem[i] = core[i].tlbToMem;
    end

   // Note: mkLLCDmaConnect is Toooba version, different from riscy-ooo version
   // llc_mem_server is an AXI4_S that serves the Debug Module
   let llc_mem_server <- mkLLCDmaConnect(llc.dma, tlbToMem);

   // ================================================================
   // interface Back-side of LLC to AXI4

   LLC_AXI4_Adapter_IFC  llc_axi4_adapter <- mkLLC_AXi4_Adapter (llc.to_mem);

   // ================================================================
   // Connect stats

   FIFO#(Bool) statReqs <- mkFIFO;

   for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
      rule recvStatReq;
         Bool doStats <- core[i].sendDoStats;
         statReqs.enq(doStats);
      endrule
   end

   rule broadcastStats;
      for(Integer j = 0; j < valueof(CoreNum); j = j+1) begin
         core[j].recvDoStats(statReqs.first);
      end
      llc.perf.setStatus(statReqs.first);
      statReqs.deq;
   endrule

`ifdef PERFORMANCE_MONITORING
   rule broadcastPerfEvents;
       for(Integer j = 0; j < valueof(CoreNum); j = j+1) begin
           core[j].events_llc(llc.events);
       end
   endrule
`endif

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
   // Termination detection

   for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
      rule rl_terminate;
	 let x <- core[i].coreIndInv.terminate;
	 $display ("Core %d terminated", i);
      endrule
   end

   // ================================================================
   // Print out values written 'tohost'

   /* DELETE: OLD
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
	 // $finish (0);
      end
   endrule
    */


`ifdef INCLUDE_GDB_CONTROL
   let emptyPut = interface Put
       method put (x) = noAction;
   endinterface;
   function proj_run_halt_server (x) = x.hart_run_halt_server;
   function proj_gpr_mem_server (x) = x.hart_gpr_mem_server;
`ifdef ISA_F
   function proj_fpr_mem_server (x) = x.hart_fpr_mem_server;
`endif
   function proj_csr_mem_server (x) = x.hart_csr_mem_server;
`endif

   // ================================================================
   // ================================================================
   // ================================================================
   // INTERFACE

   AXI4_Slave_IFC #(Wd_Id_Dma,
		    Wd_Addr_Dma,
		    Wd_Data_Dma,
		    Wd_User_Dma) dma_server_tieoff = dummy_AXI4_Slave_ifc;

   // ----------------
   // SoC fabric connections

   // M interface for memory (from LLC)
   interface  master0 = llc_axi4_adapter.mem_master;

   // M interface for IO (from MMIOPlatform)
   interface  master1 = mmioplatform_axi4_adapter.mmio_master;

   // M interface for IO (from DMA_Cache)
   interface  master2 = mmio_axi4_adapter.mem_master;

   // Interface to 'coherent DMA' port of L2 cache
`ifdef OPTION_DMA_CACHE
   interface dma_server = dma_cache.axi4_s;
`else
   interface dma_server = dma_server_tieoff;
`endif

   // ----------------
   // External interrupts

   method Action  m_external_interrupt_req (x);
      for(Integer i = 0; i < valueof(CoreNum); i = i+1)
         core[i].setMEIP (pack (x[i]));
   endmethod

   method Action  s_external_interrupt_req (x);
      for(Integer i = 0; i < valueof(CoreNum); i = i+1)
         core[i].setSEIP (pack (x[i]));
   endmethod

   // ----------------
   // Non-maskable interrupt

   // TODO: fixup: NMIs should send CPU to an NMI vector (TBD in SoC_Map)
   method Action  non_maskable_interrupt_req (Bool set_not_clear) = noAction;

   // ----------------
   // Coherent port into LLC (used by Debug Module, DMA engines, ... to read/write memory)

   interface  debug_module_mem_server = llc_mem_server;

   // ----------------
   // Optional interface to Debug Module

`ifdef INCLUDE_GDB_CONTROL

   // run/halt, gpr, mem and csr control goes to cores
   interface harts_run_halt_server = map (proj_run_halt_server, core);

   // currently "other req" not core specific - only affected cfg_verbosity which
   // is not read anywhere!
   interface harts_put_other_req = replicate(emptyPut);

   interface harts_gpr_mem_server  = map(proj_gpr_mem_server, core);
`ifdef ISA_F
   interface harts_fpr_mem_server  = map(proj_fpr_mem_server, core);
`endif
   interface harts_csr_mem_server  = map(proj_csr_mem_server, core);

`endif

`ifdef INCLUDE_TANDEM_VERIF
   interface v_to_TV = core [0].v_to_TV;
`endif

   // ----------------------------------------------------------------
   // Misc. control and status

   // ----------------
   // For tracing

   method Action  set_verbosity (Bit #(4)  verbosity);
      noAction;
   endmethod

   // ----------------
   // For ISA tests: watch memory writes to <tohost> addr

`ifdef WATCH_TOHOST
   method Action ma_set_watch_tohost (Bool watch_tohost, Bit #(64) tohost_addr);
      mmioPlatform.ma_set_watch_tohost (watch_tohost, tohost_addr);
   endmethod

   method Bit #(64) mv_tohost_value;
      return mmioPlatform.mv_tohost_value;
   endmethod
`endif

   // ----------------
   // Start the hart(s)
   // Use toHostAddr = 0 if not monitoring tohost
   method Action start (Bool running, Addr startpc, Addr tohostAddr, Addr fromhostAddr);
      $display ("%0d: %m.m_start: ", cur_cycle);
      $display ("    running %0d  startpc %0h  tohostAddr %0h  fromhostAddr %0h",
		running, startpc, tohostAddr, fromhostAddr);
      for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
	 core[i].coreReq.start (running, startpc, tohostAddr, fromhostAddr);
	 $display ("    start CPU[%0d]", i);
      end

      mmioPlatform.start (tohostAddr, fromhostAddr);
   endmethod

   // Inform core that DDR4 has been initialized and is ready to accept requests
   method Action ma_ddr4_ready;
      llc_axi4_adapter.ma_ddr4_ready;    // Enable memory access
   endmethod

   // Misc. status; 0 = running, no error
   method Bit #(8) mv_status;
      return 0;    // TODO: return a more meaningful value on fatal errors?
   endmethod

endmodule: mkProc

// ================================================================

endpackage
