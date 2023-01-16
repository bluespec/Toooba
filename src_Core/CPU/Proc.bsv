package Proc;

// Note: this module corresponds to module 'mkCPU' in Piccolo/Flute.

// Copyright (c) 2018 Massachusetts Institute of Technology
// Portions Copyright (c) 2019-2020 Bluespec, Inc.
//
//-
// RVFI_DII + CHERI modifications:
//     Copyright (c) 2020 Jessica Clarke
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
import FIFOF        :: *;
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
`ifdef PERFORMANCE_MONITORING
import StatCounters::*;
`endif

// ----------------
// From Tooba

import ISA_Decls  :: *;

import Core              :: *;
import Proc_IFC          :: *;
import MMIOPlatform      :: *;
import LLC_AXI4_Adapter  :: *;
import MMIO_AXI4_Adapter :: *;

import SoC_Map      :: *;
import AXI4   :: *;
import Fabric_Defs  :: *;

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

   // ----------------
    // cores
    Vector#(CoreNum, Core) core = ?;
    for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
        core[i] <- mkCore(fromInteger(i));
    end

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

   // Note: mkLLCDmaConnect is Toooba version, different from riscy-ooo version
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
   Reg#(EventsTGC) events_tgc_reg <- mkRegU;
   rule broadcastPerfEvents;
       for(Integer j = 0; j < valueof(CoreNum); j = j+1) begin
           core[j].events_llc(llc.events);
           core[j].events_tgc(events_tgc_reg);
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
   // Send and print perf requests 

   Reg#(Bit#(4)) perfCnt <- mkReg(0);
   Reg#(Bool) requestSent <- mkReg(False);
   rule rl_sendLLCPerfReq if (!requestSent);
      perfCnt <= (perfCnt == 12) ? 0 : perfCnt + 1;
      case (perfCnt)
         'b1001: llc.perf.req(LLCNormalMemLdCnt);
         'b1010: llc.perf.req(LLCNormalMemLdLat);
         'b1011: llc.perf.req(LLCInstructionLdCnt);
         'b1100: llc.perf.req(LLCInstructionLdLat);
      endcase
      requestSent <= True;
   endrule
   rule rl_printLLCPerfResp;
      let perfResp <- llc.perf.resp;
      requestSent <= False;
      if (perfResp.pType == LLCNormalMemLdCnt) begin
         $display("%0d: LLC data miss count: %d", cur_cycle, unpack(perfResp.data));
      end
      else if (perfResp.pType == LLCNormalMemLdLat) begin
         $display("%0d: LLC data miss latency: %d", cur_cycle, unpack(perfResp.data));
      end
      else if (perfResp.pType == LLCInstructionLdCnt) begin
         $display("%0d: LLC instruction miss count: %d", cur_cycle, unpack(perfResp.data));
      end
      else if (perfResp.pType == LLCInstructionLdLat) begin
         $display("%0d: LLC instruction miss latency: %d", cur_cycle, unpack(perfResp.data));
      end
   endrule

   for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
      rule rl_sendPerfReq if (!requestSent);
         case (perfCnt)
            'b0000: core[i].coreReq.perfReq(ICache, zeroExtend(pack(L1ILdCnt)));
            'b0001: core[i].coreReq.perfReq(ICache, zeroExtend(pack(L1ILdMissCnt)));
            'b0010: core[i].coreReq.perfReq(ICache, zeroExtend(pack(L1ILdMissLat)));
            'b0011: core[i].coreReq.perfReq(DCache, zeroExtend(pack(L1DLdCnt)));
            'b0100: core[i].coreReq.perfReq(DCache, zeroExtend(pack(L1DLdMissCnt)));
            'b0101: core[i].coreReq.perfReq(DCache, zeroExtend(pack(L1DLdMissLat)));
            'b0110: core[i].coreReq.perfReq(DCache, zeroExtend(pack(L1DStCnt)));
            'b0111: core[i].coreReq.perfReq(DCache, zeroExtend(pack(L1DStMissCnt)));
            'b1000: core[i].coreReq.perfReq(DCache, zeroExtend(pack(L1DStMissLat)));
         endcase
      endrule

      rule rl_printPerfResp;
         let perfResp <- core[i].coreIndInv.perfResp;
         requestSent <= False;
         if (perfResp.loc == ICache) begin
            if (perfResp.pType == zeroExtend(pack(L1ILdCnt))) begin
               $display("%0d: L1I load count: %d", cur_cycle, unpack(perfResp.data));
            end
            else if (perfResp.pType == zeroExtend(pack(L1ILdMissCnt))) begin
               $display("%0d: L1I load miss count: %d", cur_cycle, unpack(perfResp.data));
            end
            else if (perfResp.pType == zeroExtend(pack(L1ILdMissLat))) begin
               $display("%0d: L1I load miss latency: %d", cur_cycle, unpack(perfResp.data));
            end
         end
         else if (perfResp.loc == DCache) begin 
            if (perfResp.pType == zeroExtend(pack(L1DLdCnt))) begin
               $display("%0d: L1D load count: %d", cur_cycle, unpack(perfResp.data));
            end
            else if (perfResp.pType == zeroExtend(pack(L1DLdMissCnt))) begin
               $display("%0d: L1D load miss count: %d", cur_cycle, unpack(perfResp.data));
            end
            else if (perfResp.pType == zeroExtend(pack(L1DLdMissLat))) begin
               $display("%0d: L1D load miss latency: %d", cur_cycle, unpack(perfResp.data));
            end
            if (perfResp.pType == zeroExtend(pack(L1DStCnt))) begin
               $display("%0d: L1D store count: %d", cur_cycle, unpack(perfResp.data));
            end
            else if (perfResp.pType == zeroExtend(pack(L1DStMissCnt))) begin
               $display("%0d: L1D store miss count: %d", cur_cycle, unpack(perfResp.data));
            end
            else if (perfResp.pType == zeroExtend(pack(L1DStMissLat))) begin
               $display("%0d: L1D store miss latency: %d", cur_cycle, unpack(perfResp.data));
            end
         end
      endrule
   end

   // ================================================================
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


`ifdef INCLUDE_GDB_CONTROL
   let emptyPut = interface Put
       method put (x) = noAction;
   endinterface;
   function proj_run_halt_server (x) = x.hart_run_halt_server;
   function proj_gpr_mem_server (x) = x.hart_gpr_mem_server;
   function proj_core_is_running (x) = x.core_is_running;
`ifdef ISA_F
   function proj_fpr_mem_server (x) = x.hart_fpr_mem_server;
`endif
   function proj_csr_mem_server (x) = x.hart_csr_mem_server;
`endif

   // ================================================================
   // ================================================================
   // ================================================================
   // INTERFACE

   // ----------------
   // Start the cores running
   // Use toHostAddr = 0 if not monitoring tohost
   method Action start (Bool running, Addr startpc, Addr tohostAddr, Addr fromhostAddr);
      action
         for(Integer i = 0; i < valueof(CoreNum); i = i+1)
	    core[i].coreReq.start (running, startpc, tohostAddr, fromhostAddr);
      endaction

      mmioPlatform.start (tohostAddr, fromhostAddr);

      $display ("%0d: %m.method start: startpc %0h, tohostAddr %0h, fromhostAddr %0h",
                cur_cycle, startpc, tohostAddr, fromhostAddr);
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
   // For tracing

   method Action  set_verbosity (Bit #(4)  verbosity);
      noAction;
   endmethod

   // ----------------
   // Coherent port into LLC (used by Debug Module, DMA engines, ... to read/write memory)

   interface  debug_module_mem_server = llc_mem_server;

`ifdef RVFI_DII
   interface Toooba_RVFI_DII_Server rvfi_dii_server = core[0].rvfi_dii_server;
`endif

   // ----------------
   // Optional interface to Debug Module

`ifdef INCLUDE_GDB_CONTROL

   // run/halt, gpr, mem and csr control goes to cores
   interface harts_run_halt_server = map (proj_run_halt_server, core);

   // currently "other req" not core specific - only affected cfg_verbosity which
   // is not read anywhere!
   interface harts_put_other_req = replicate(emptyPut);

   interface harts_gpr_mem_server  = map(proj_gpr_mem_server, core);

   interface harts_is_running = map (proj_core_is_running, core);
`ifdef ISA_F
   interface harts_fpr_mem_server  = map(proj_fpr_mem_server, core);
`endif
   interface harts_csr_mem_server  = map(proj_csr_mem_server, core);

`endif

`ifdef INCLUDE_TANDEM_VERIF
   interface v_to_TV = core [0].v_to_TV;
`endif

`ifdef PERFORMANCE_MONITORING
    method events_tgc = events_tgc_reg._write;
`endif

endmodule: mkProc

// ================================================================

endpackage
