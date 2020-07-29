
// Copyright (c) 2017 Massachusetts Institute of Technology
// Portions Copyright (c) 2019-2020 Bluespec, Inc.
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

`include "ProcConfig.bsv"

import BrPred::*;
import DirPredictor::*;
import Btb::*;
import ClientServer::*;
import Connectable::*;
import Decode::*;
import Ehr::*;
import Fifos::*;
import GetPut::*;
import MemoryTypes::*;
import Types::*;
import ProcTypes::*;
import CCTypes::*;
import DefaultValue::*;
import Ras::*;
import EpochManager::*;
import Performance::*;
import Vector::*;
import Assert::*;
import Cntrs::*;
import ConfigReg::*;
import TlbTypes::*;
import ITlb::*;
import CCTypes::*;
import L1CoCache::*;
import MMIOInst::*;

import Cur_Cycle :: *;

// ================================================================
// For fv_decode_C function and related types and definitions

import ISA_Decls        :: *;
import CPU_Decode_C     :: *;

// ================================================================

interface FetchStage;
    // pipeline
    interface Vector#(SupSize, SupFifoDeq#(FromFetchStage)) pipelines;

    // tlb and mem connections
    interface ITlb iTlbIfc;
    interface ICoCache iMemIfc;
    interface MMIOInstToCore mmioIfc;

    // starting and stopping
    method Action start(Addr pc);
    method Action stop();

    // redirection methods
    method Action setWaitRedirect;
    method Action redirect(Addr pc);
`ifdef INCLUDE_GDB_CONTROL
   method Action setWaitFlush;
`endif
    method Action done_flushing();
    method Action train_predictors(
        Addr pc, Addr next_pc, IType iType, Bool taken,
        DirPredTrainInfo dpTrain, Bool mispred, Bool isCompressed
    );

    // security
    method Bool emptyForFlush;
    method Action flush_predictors;
    method Bool flush_predictors_done;

    // debug
    method FetchDebugState getFetchState;

    // performance
    interface Perf#(DecStagePerfType) perf;
endinterface

typedef struct {
    Addr pc;
    Epoch mainEp;
    Bool waitForRedirect;
    Bool waitForFlush;
} FetchDebugState deriving(Bits, Eq, FShow);

typedef struct {
    Addr pc;
    Maybe#(Addr) pred_next_pc;
    Bool decode_epoch;
    Epoch main_epoch;
} Fetch1ToFetch2 deriving(Bits, Eq, FShow);

typedef struct {
    Addr pc;
    Maybe#(Addr) pred_next_pc;
    Maybe#(Exception) cause;
    Addr tval;                 // in case of exception
    Bool access_mmio; // inst fetch from MMIO
    Bool decode_epoch;
    Epoch main_epoch;
} Fetch2ToFetch3 deriving(Bits, Eq, FShow);

typedef struct {
    Addr pred_next_pc;
    Bool mispred_first_half;
    Maybe#(Exception) cause;
    Addr tval;                 // in case of exception
    Bool decode_epoch;
    Epoch main_epoch;
} Fetch3ToDecode deriving(Bits, Eq, FShow);

// Used purely internally in doDecode.
typedef struct {
  Addr pc;
  Addr ppc;
  Bool decode_epoch;
  Epoch main_epoch;
  Instruction inst;
  Maybe#(Exception) cause;
} InstrFromFetch3 deriving(Bits, Eq, FShow);

typedef struct {
  Addr pc;
  Addr ppc;
  Epoch main_epoch;
  DirPredTrainInfo dpTrain;
  Instruction inst;
  DecodedInst dInst;
  Bit #(32) orig_inst;    // original 16b or 32b instruction ([1:0] will distinguish 16b or 32b)
  ArchRegs regs;
  Maybe#(Exception) cause;
  Addr              tval;    // in case of exception
} FromFetchStage deriving (Bits, Eq, FShow);

// train next addr pred (BTB)
typedef struct {
    Addr pc;
    Addr nextPc;
} TrainNAP deriving(Bits, Eq, FShow);

// ================================================================
// Functions for 'C' instruction set

function MISA misa;
   MISA x = unpack (0);
   x.mxl = misa_mxl_64;
   x.u = 1;
   x.s = 1;
   x.m = 1;
   x.i = 1;
   x.f = 1;
   x.d = 1;
   x.c = 1;
   x.a = 1;
   return x;
endfunction

function Bool is_16b_inst (Bit #(n) inst);
   return (inst [1:0] != 2'b11);
endfunction

function Bool is_32b_inst (Bit #(n) inst);
   return (inst [1:0] == 2'b11);
endfunction

// Fetching instructions from mem returns up to superscalar-size 32b parcels, = twice that many 16b parcels

typedef Bit #(TLog #(TAdd #(SupSizeX2, 1))) SupCntX2;

// Merging up to SupSize-1 pending instructions with up to SupSizeX2 decoded
// instructions produces up to 3*SupSize-1 instructions; SupSize can be issued
// if present, leaving up to 2*SupSize-1 pending.
typedef TSub #(TMul #(SupSize, 2), 1) SupSizeX2S1;
typedef Bit #(TLog #(TAdd #(SupSizeX2S1, 1))) SupCntX2S1;
typedef TSub #(TMul #(SupSize, 3), 1) SupSizeX3S1;
typedef Bit #(TLog #(TAdd #(SupSizeX3S1, 1))) SupCntX3S1;

// Appending the pending and decoded vectors produces an intermediate
// 4*SupSize-1 vector (with only up to 3*SupSize-1 elements non-empty).
typedef TSub #(TMul #(SupSize, 4), 1) SupSizeX4S1;
typedef Bit #(TLog #(TAdd #(SupSizeX4S1, 1))) SupCntX4S1;

// Parsing a sequence of 16-bit parcels returns a sequence of the
// following kinds or items

typedef enum {Inst_None,       // When we run off the end of the sequence
	      Inst_16b,        // A 16b instruction
	      Inst_32b         // A 32b instruction
   } Inst_Kind
deriving (Bits, Eq, FShow);

// Each instr item is accompanied by its actual PC, since PC is no
// longer a simple multiple of 4 away from the start-pc of the sequence.

typedef struct {
   Addr        pc;
   Inst_Kind   inst_kind;
   Bit #(32)   orig_inst;    // inst_kind => 0, 16b or 32b relevant
   Bit #(32)   inst;         // Original 32b instruction, or expansion of 16b instruction
   } Inst_Item
deriving (Bits, Eq, FShow);

instance DefaultValue #(Inst_Item);
   function Inst_Item defaultValue = Inst_Item {
      pc: 0, inst_kind: Inst_None, orig_inst: 0, inst: 0
   };
endinstance

typedef Tuple3 #(Addr, Bit #(16), Bool) Straddle;
function Straddle fv_straddle (Addr pc, Bit #(16) lsbs, Bool mispred);
    return tuple3 (pc, lsbs, mispred);
endfunction
typedef Maybe #(Straddle) MStraddle;

// Parse 16b parcels (v_x16) into a sequence of 16b or 32b instructions.
// This is a pure function; ActionValue is used only to allow $displays for debugging.

function ActionValue #(Tuple4 #(SupCntX2,
				Vector #(SupSizeX2, Inst_Item),
				Addr,
				Maybe #(Tuple3 #(Addr, Bit #(16), Bool))))
         fav_parse_insts (Bool  verbose,
			  Addr  pc_start,
			  Maybe #(Addr) pred_next_pc,
			  Maybe #(Tuple3 #(Addr, Bit #(16), Bool)) pending_straddle,
			  SupCntX2 n_x16s,
			  Vector #(SupSizeX2, Bit #(16)) v_x16);
   actionvalue
      // Parse up to SupSizeX2 instructions (v_items) from fetched v_x16 parcels (v_x16).
      Vector #(SupSizeX2, Inst_Item) v_items = replicate (Inst_Item {pc: pc_start,
                                                                     inst_kind: Inst_None,
                                                                     orig_inst: 0,
                                                                     inst: 0});
      MStraddle next_straddle = tagged Invalid;
      SupCntX2 j  = 0;
      Addr     pc = pc_start;
      Integer n_items = 0;
      for (Integer i = 0; i < valueOf (SupSizeX2); i = i + 1) begin
         Inst_Kind inst_kind = Inst_None;
         Bit #(32) orig_inst = 0;
         Bit #(32) inst      = 0;
         Addr      next_pc   = pc;
         if (j < n_x16s) begin
            if (i == 0 &&& pending_straddle matches tagged Valid {.s_pc,
                                                                  .s_lsbs, .s_mispred}) begin
               if (!s_mispred) begin
                   if (pc != getAddr(s_pc) + 2) begin
                      $display ("FetchStage.fav_parse_insts: straddle: pc mismatch: pc = 0x%0h but s_pc = 0x%0h", pc, s_pc);
                      dynamicAssert (False, "FetchStage.fav_parse_insts: straddle: pc mismatch");
                   end
                   pc        = s_pc;
                   inst_kind = Inst_32b;
                   orig_inst = { v_x16[j], s_lsbs };
                   inst      = orig_inst;
                   j         = j + 1;
                   next_pc   = s_pc + 4;
                   n_items   = 1;
               end else begin // This is a mispredicted half of a 32-bit instruction.  We will flush, but just pretend it's a 16-bit instruction to keep things in sync.
                   pc        = getAddr(s_pc);
                   inst_kind = Inst_16b;
                   orig_inst = zeroExtend (s_lsbs);
                   inst      = orig_inst;    // Expand 16b inst to 32b inst
                   next_pc   = pc_start;
                   n_items   = 1;
                   if (verbose)
                      $display ("FetchStage.fav_parse_insts: mispredicted 1/2 inst 0x%0h -> inst 0x%0h", orig_inst, inst);
               end
            end
            else if (is_16b_inst (v_x16 [j])) begin
               inst_kind = Inst_16b;
               orig_inst = zeroExtend (v_x16 [j]);
               inst      = fv_decode_C (misa, misa_mxl_64, v_x16 [j]);    // Expand 16b inst to 32b inst
               j         = j + 1;
               next_pc   = pc + 2;
               n_items   = i + 1;
               if (verbose)
                  $display ("FetchStage.fav_parse_insts: C inst 0x%0h -> inst 0x%0h", orig_inst, inst);
            end
            else if (is_32b_inst (v_x16 [j])) begin
               if ((j + 1) < n_x16s) begin
                  inst_kind = Inst_32b;
                  orig_inst = { v_x16 [j+1], v_x16 [j] };
                  inst      = orig_inst;
                  j = j + 2;
                  next_pc = pc + 4;
                  n_items   = i + 1;
               end
               else begin
                  Bool mispred_first_half = isValid(pred_next_pc);
                  next_straddle = tagged Valid fv_straddle(pc,
                                                           v_x16[j], mispred_first_half);
                  j = j + 1;
                  // Leave next_pc unchanged and clear pred_next_pc so we
                  // return the right predicted pc for the vector, which
                  // excludes the pending straddle.
                  pred_next_pc = tagged Invalid;
               end
            end
            else begin
               $display ("FetchStage.fav_parse_insts: instuction is not 16b or 32b?");
               $display ("    pc_start = 0x%0h, i = %0d, j = %0d, pc = 0x%0h", pc_start, i, j, pc);
               $display ("    v_x16:   ", fshow (v_x16));
               $display ("    v_items: ", fshow (v_items));
               dynamicAssert (False, "FetchStage.fav_parse_insts: instuction is not 16b or 32b?");
            end
         end
         v_items [i] = Inst_Item {pc: pc,
                                  inst_kind: inst_kind, orig_inst: orig_inst, inst: inst};
         pc = next_pc;
      end

      if (verbose) begin
	 $display ("FetchStage.fav_parse_insts:");
	 $display ("    v_x16:   ", fshow (v_x16));
	 $display ("    n_items: %0d", n_items);
	 $display ("    v_items: ", fshow (v_items));
	 $display ("    next_straddle: ", fshow (next_straddle));
      end

      return tuple4(fromInteger(n_items), v_items, fromMaybe(pc, pred_next_pc), next_straddle);
   endactionvalue
endfunction

// ================================================================

(* synthesize *)
module mkFetchStage(FetchStage);
    // rule ordering: Fetch1 (BTB+TLB) < Fetch3 (decode & dir pred) < redirect method
    // Fetch1 < Fetch3 to avoid bypassing path on PC and epochs

    Bool verbose = False;
    Integer verbosity = 0;

    // Basic State Elements
    Reg#(Bool) started <- mkReg(False);

    // Stall fetch when trap happens or system inst is renamed
    // All inst younger than the trap/system inst will be killed
    // Since CSR may be modified, sending wrong path request to TLB may cause problem
    // So we stall until the next redirection happens
    // The next redirect is either by the trap/system inst or an older one
    Reg#(Bool) waitForRedirect <- mkReg(False);
    // We don't want setWaitForRedirect method and redirect method to happen together
    // make them conflict
    RWire#(void) setWaitRedirect_redirect_conflict <- mkRWire;

    // Stall fetch during the flush triggered by the procesing trap/system inst in commit stage
    // We stall until the flush is done
    Reg#(Bool) waitForFlush <- mkReg(False);

    Ehr#(4, Addr) pc_reg <- mkEhr(0);
    Integer pc_fetch1_port = 0;
    Integer pc_decode_port = 1;
    Integer pc_fetch3_port = 2;
    Integer pc_redirect_port = 3;

    // Epochs
    Ehr#(2, Bool) decode_epoch <- mkEhr(False);
    Reg#(Epoch) f_main_epoch <- mkReg(0); // fetch estimate of main epoch

   // Reg to hold the first half of an instruction that straddles a cache line boundary
   Ehr #(2, Maybe #(Tuple3 #(Addr, Bit #(16), Bool))) ehr_pending_straddle <- mkEhr(tagged Invalid);
   // Reg to hold extra instructions from Fetch3 to send to decode the next cycle
   Reg #(Vector #(SupSizeX2S1, Inst_Item)) rg_pending_decode <- mkReg(replicate(defaultValue));
   Reg #(SupCntX2S1) rg_pending_n_items <- mkReg(0);
   Reg #(Fetch3ToDecode) rg_pending_f32d <- mkRegU;

    // Pipeline Stage FIFOs
    Fifo#(2, Tuple2#(Bit#(TLog#(SupSizeX2)),Fetch1ToFetch2)) f12f2 <- mkCFFifo;
    Fifo#(4, Tuple2#(Bit#(TLog#(SupSizeX2)),Fetch2ToFetch3)) f22f3 <- mkCFFifo; // FIFO should match I$ latency
    Fifo#(2, Tuple2#(Bit#(TLog#(SupSize)),Fetch3ToDecode)) f32d <- mkCFFifo;

    // Fifo#(2, Vector#(SupSize,Maybe#(Instruction))) instdata <- mkPipelineFifo();    // OLD
    // FIFO from rule doFetch3 to rule doDecode
    Fifo #(2, Vector #(SupSize, Inst_Item)) instdata <- mkPipelineFifo();

    SupFifo#(SupSize, 2, FromFetchStage) out_fifo <- mkSupFifo;
       // Can the fifo size be smaller?

    // Branch Predictors
    NextAddrPred    nextAddrPred <- mkBtb;
    let             dirPred      <- mkDirPredictor;
    ReturnAddrStack ras          <- mkRas;
    // Wire to train next addr pred (NAP)
    RWire#(TrainNAP) napTrainByExe <- mkRWire;
    RWire#(TrainNAP) napTrainByDec <- mkRWire;
    Fifo#(1, TrainNAP) napTrainByDecQ <- mkPipelineFifo; // cut off critical path

    // TLB and Cache connections
    ITlb iTlb <- mkITlb;
    ICoCache iMem <- mkICoCache;
    MMIOInst mmio <- mkMMIOInst;
    Server#(Addr, TlbResp) tlb_server = iTlb.to_proc;
    Server#(Addr, Vector#(SupSizeX2, Maybe#(Instruction16))) mem_server = iMem.to_proc;

    // performance counters
    Fifo#(1, DecStagePerfType) perfReqQ <- mkCFFifo; // perf req FIFO
`ifdef PERF_COUNT
    Reg#(Bool) doStats <- mkConfigReg(False);
    // decode stage redirect
    Count#(Data) decRedirectBrCnt <- mkCount(0);
    Count#(Data) decRedirectJmpCnt <- mkCount(0);
    Count#(Data) decRedirectJrCnt <- mkCount(0);
    Count#(Data) decRedirectOtherCnt <- mkCount(0);
    // perf resp FIFO
    Fifo#(1, PerfResp#(DecStagePerfType)) perfRespQ <- mkCFFifo;

    rule doPerfReq;
        let t <- toGet(perfReqQ).get;
        Data d = (case(t)
            DecRedirectBr: decRedirectBrCnt;
            DecRedirectJmp: decRedirectJmpCnt;
            DecRedirectJr: decRedirectJrCnt;
            DecRedirectOther: decRedirectOtherCnt;
            default: 0;
        endcase);
        perfRespQ.enq(PerfResp {
            pType: t,
            data: d
        });
    endrule
`endif

    // We don't send req to TLB when waiting for redirect or TLB flush. Since
    // there is no FIFO between doFetch1 and TLB, when OOO commit stage wait
    // TLB idle to change VM CSR / signal flush TLB, there is no wrong path
    // request afterwards to race with the system code that manage paget table.
    rule doFetch1(started && !waitForRedirect && !waitForFlush);
        let pc = pc_reg[pc_fetch1_port];

        // Chain of prediction for the next instructions
        // We need a BTB with a register file with enough ports!
        // Instead of cascading predictions, we can always feed pc+4*i into
        // predictor, because we will break superscaler fetch if nextpc != pc+4
        Vector#(SupSizeX2, Maybe#(Addr)) pred_future_pc;
        for(Integer i = 0; i < valueof(SupSizeX2); i = i+1) begin
            pred_future_pc[i] = nextAddrPred.predPc(pc + fromInteger(2 * i));
        end

        // Next pc is the first nextPc that breaks the chain of pc+4 or
        // that is at the end of a cacheline.
        Vector#(SupSizeX2,Integer) indexes = genVector;
        function Bool findNextPc(Addr p, Integer i);
            Bool notLastInst = getLineInstOffset(p + fromInteger(2*i)) != maxBound;
            Bool noJump = !isValid(pred_future_pc[i]);
            return (!(notLastInst && noJump));
        endfunction
        Bit#(TLog#(SupSizeX2)) posLastSupX2 = fromInteger(fromMaybe(valueof(SupSizeX2) - 1, find(findNextPc(pc), indexes)));
        Maybe#(Addr) pred_next_pc = pred_future_pc[posLastSupX2];

        let next_fetch_pc = fromMaybe(pc + 2 * (zeroExtend(posLastSupX2) + 1), pred_next_pc);
        pc_reg[pc_fetch1_port] <= next_fetch_pc;

        // Send TLB request.

        tlb_server.request.put (pc);

        let out = Fetch1ToFetch2 {
            pc: pc,
            pred_next_pc: pred_next_pc,
            decode_epoch: decode_epoch[0],
            main_epoch: f_main_epoch};

        f12f2.enq(tuple2(posLastSupX2,out));
        if (verbose) $display("Fetch1: ", fshow(out), " posLastSupX2: %d", posLastSupX2);
    endrule

    rule doFetch2;
        let {nbSupX2,in} = f12f2.first;
        f12f2.deq;

        // Get TLB response
        match {.phys_pc, .cause} <- tlb_server.response.get;
        Addr tval =  0;

        // Access main mem or boot rom if no TLB exception
        Bool access_mmio = False;
        if (!isValid(cause)) begin
            case(mmio.getFetchTarget(phys_pc))
                MainMem: begin
                    // Send ICache request
                    mem_server.request.put(phys_pc);
                end
                IODevice: begin
                    // Send MMIO req. Luckily boot rom is also aligned with
                    // cache line size, so all nbSup+1 insts can be fetched
                    // from boot rom. It won't happen that insts fetched from
                    // boot rom is less than requested.
                    mmio.bootRomReq(phys_pc, nbSupX2);
                    access_mmio = True;
                end
                default: begin
                    // Access fault
                    cause = Valid (InstAccessFault);
		    // Without 'C' extension:
		    //     Addr align32b_mask = 'h3;
		    //     tval = (in.pc & (~ align32b_mask));
		    Addr align16b_mask = 'h1;
		    tval = (in.pc & (~ align16b_mask));
                end
            endcase
        end
        else begin
	   // TLB exception: record the request address
           // Without 'C' extension:
           //     Addr align32b_mask = 'h3;
           //     tval = (in.pc & (~ align32b_mask));
           Addr align16b_mask = 'h1;
           tval = (in.pc & (~ align16b_mask));
	end

        let out = Fetch2ToFetch3 {
            pc: in.pc,
            pred_next_pc: in.pred_next_pc,
            cause: cause,
	    tval: tval,
            access_mmio: access_mmio,
            decode_epoch: in.decode_epoch,
            main_epoch: in.main_epoch };
        f22f3.enq(tuple2(nbSupX2,out));

       if (verbosity >= 2) begin
	  $display ("----------------");
	  $display ("Fetch2: TLB response pyhs_pc 0x%0h  cause ", phys_pc, fshow (cause));
	  $display ("Fetch2: f2_tof3.enq: nbSupX2 %0d out ", nbSupX2, fshow (out));
       end
    endrule

// Break out of i$
    rule doFetch3;
        let {nbSupX2In, fetch3In} = f22f3.first;
        if (verbosity >= 2) begin
            if (f22f3.notEmpty)
                $display("Fetch3: nbSupX2In: %0d fetch3In: ", nbSupX2In, fshow (fetch3In));
            else
                $display("Fetch3: Nothing else from Fetch2");
        end

        SupCntX2S1 pending_n_items = rg_pending_n_items;
        let out = rg_pending_f32d;
        Maybe #(Tuple3 #(Addr, Bit #(16), Bool)) pending_straddle = ehr_pending_straddle[0];

        if (pending_n_items > 0) begin
            if (rg_pending_f32d.main_epoch != f_main_epoch || rg_pending_f32d.decode_epoch != decode_epoch[1]) begin
                // Just drop it. Also drop any pending straddle, as that is
                // associated with the same epoch.
                pending_n_items = 0;
                pending_straddle = tagged Invalid;
                if (verbosity >= 2) begin
                    $display ("----------------");
                    $display ("Fetch3: Drop pending: main_epoch: %d decode epoch: %d", f_main_epoch, decode_epoch[1]);
                    $display ("Fetch3: rg_pending_n_items:  ", fshow (rg_pending_n_items));
                    $display ("Fetch3: rg_pending_f32d:   ", fshow (rg_pending_f32d));
                    $display ("Fetch3: rg_pending_decode: ", fshow (rg_pending_decode));
                end
            end
        end

        SupCntX2 parsed_n_items = 0;
        Inst_Item inst_item_none = Inst_Item {pc: fetch3In.pc, inst_kind: Inst_None, orig_inst: 0, inst: 0};
        Vector #(SupSizeX2, Inst_Item) parsed_v_items = replicate (inst_item_none);

        let mispred_first_half = pending_straddle matches tagged Valid {.s_pc, .s_lsbs, .s_mispred} &&& s_mispred ? True : False;
        let can_merge =    pending_n_items > 0
                        && pending_n_items < fromInteger(valueOf(SupSize))
                        && f22f3.notEmpty
                        && !isValid(fetch3In.cause)
                        && fetch3In.main_epoch == rg_pending_f32d.main_epoch
                        && fetch3In.decode_epoch == rg_pending_f32d.decode_epoch
                        && !mispred_first_half;

        let drop_f22f3 =    f22f3.notEmpty
                         && (   fetch3In.main_epoch != f_main_epoch
                             || fetch3In.decode_epoch != decode_epoch[1]);

        let parse_f22f3 = !drop_f22f3 && (pending_n_items == 0 || can_merge);

        // Get ICache/MMIO response if no exception
        // In case of exception, we still need to process at least inst_data[0]
        // (it will be turned to an exception later), so inst_data[0] must be
        // valid.
        Vector#(SupSizeX2,Maybe#(Instruction16)) inst_d = replicate(tagged Valid (0));
        if (drop_f22f3 || parse_f22f3) begin
            f22f3.deq();
            if (!isValid(fetch3In.cause)) begin
                if(fetch3In.access_mmio) begin
                    inst_d <- mmio.bootRomResp;
                    if(verbose) $display("get answer from MMIO 0x%0x", fetch3In.pc, " ", fshow(inst_d));
                end
                else begin
                    if(verbose) $display("get answer from memory %d", fetch3In.pc);
                    inst_d <- mem_server.response.get;
                end
            end
        end

        if (drop_f22f3) begin
            // Drop any pending straddle if this is for a different main or
            // decode epoch since that invalidates our Fetch3 redirect, but
            // otherwise keep it to flush the pipeline until we get the next
            // half of the straddle.
            if (fetch3In.main_epoch != f_main_epoch || fetch3In.decode_epoch != decode_epoch[1]) begin
                pending_straddle = tagged Invalid;
            end
            if (verbosity >= 2) begin
                $display ("----------------");
                $display ("Fetch3: Drop: main_epoch: %d decode epoch: %d", f_main_epoch, decode_epoch[1]);
                $display ("Fetch3: f22f3.first: ", fshow (f22f3.first));
                $display ("Fetch3: inst_d:      ", fshow (inst_d));
            end
        end
        else if (parse_f22f3) begin
            Vector#(SupSizeX2, Instruction16) v_x16 = map(fromMaybe(?), inst_d);
            SupCntX2 n_x16s = zeroExtend(nbSupX2In) + 1;

            // Parse v_x16 into 32-bit and 16-bit instructions
            Addr pred_next_pc;
            {parsed_n_items, parsed_v_items, pred_next_pc, pending_straddle} <-
                fav_parse_insts (verbose, fetch3In.pc, fetch3In.pred_next_pc, pending_straddle, n_x16s, v_x16);

            if (pending_n_items == 0) begin
                out = Fetch3ToDecode {
                    pred_next_pc: pred_next_pc,
                    mispred_first_half: mispred_first_half,
                    cause: fetch3In.cause,
                    tval: fetch3In.tval,
                    decode_epoch: fetch3In.decode_epoch,
                    main_epoch: fetch3In.main_epoch
                };
            end
            else begin
                out.pred_next_pc = pred_next_pc;
            end

        end

        SupCntX2S1 next_pending_n_items = 0;

        if (pending_n_items > 0 || parse_f22f3) begin
            SupCntX3S1 n_items = extend(pending_n_items) + extend(parsed_n_items);
            Bit #(TLog #(SupSize)) nbSupOut = truncate(n_items - 1);

            let pending_spaces = fromInteger(valueOf(SupSizeX2S1)) - pending_n_items;
            Vector #(SupSizeX2S1, Inst_Item) pending_items_ralign =
                shiftOutFromN(inst_item_none, rg_pending_decode, pending_spaces);
            // Appease bluespec compiler with seemingly-unnecessary extension;
            // otherwise elaboration fails with:
            //   Error: "Vector.bs", line 791, column 33: (T0051)
            //     Literal 7 is not a valid Bit#(2).
            //     During elaboration of the body of rule `doFetch3' at
            //     ...
            SupCntX4S1 pending_spaces_ext = extend(pending_spaces);
            Vector #(SupSizeX3S1, Inst_Item) v_items =
                take(shiftOutFrom0(inst_item_none, append(pending_items_ralign, parsed_v_items), pending_spaces_ext));

            // Handle decoding more instructions than we can issue this cycle
            if (n_items > fromInteger(valueOf(SupSize))) begin
                nbSupOut = fromInteger(valueOf(SupSize) - 1);

                if (!isValid(out.cause)) begin
                    next_pending_n_items = truncate(n_items - fromInteger(valueOf(SupSize)));
                    rg_pending_decode <= drop(v_items);
                    rg_pending_f32d <= Fetch3ToDecode {
                        pred_next_pc: out.pred_next_pc,
                        mispred_first_half: False,
                        cause: tagged Invalid,
                        tval: 0,
                        decode_epoch: out.decode_epoch,
                        main_epoch: out.main_epoch
                    };
                end

                out.pred_next_pc = v_items[valueOf(SupSize)].pc;
            end

            if (n_items > 0) begin
                instdata.enq(take(v_items));
                f32d.enq(tuple2(nbSupOut, out));
                if (verbosity >= 2) begin
                    $display ("----------------");
                    $display ("Fetch3: epoch inst: %d, epoch main : %d", out.main_epoch, f_main_epoch);
                    $display ("Fetch3: inst_d:   ", fshow (inst_d));
                    $display ("Fetch3: v_items:  ", fshow (v_items));
                    $display ("Fetch3: f32d.enq: nbSup %0d out ", nbSupOut, fshow (out));
                end
            end
            else begin
                // This means we started fetching from a line straddling
                // instruction; need another cycle to have something to
                // issue.

                dynamicAssert(isValid(pending_straddle), "Decoded no instructions and no straddle!");
            end
        end
        rg_pending_n_items <= next_pending_n_items;
        ehr_pending_straddle[0] <= pending_straddle;
    endrule: doFetch3

   rule doDecode;
      let {nbSup, decodeIn} = f32d.first;
      f32d.deq();
      let inst_data = instdata.first();
      instdata.deq();
      // The main_epoch check is required to make sure this stage doesn't
      // redirect the PC if a later stage already redirected the PC.
      if (decodeIn.main_epoch == f_main_epoch) begin
         Bool decode_epoch_local = decode_epoch[0]; // next value for decode epoch
         Maybe#(Addr) redirectPc = Invalid; // next pc redirect by branch predictor
         Maybe#(TrainNAP) trainNAP = Invalid; // training data sent to next addr pred
`ifdef PERF_COUNT
         // performance counter: inst being redirect by decode stage
         // Note that only 1 redirection may happen in a cycle
         Maybe#(IType) redirectInst = Invalid;
`endif

         for (Integer i = 0; i < valueof(SupSize); i=i+1) begin
            if (inst_data[i].inst_kind != Inst_None && (fromInteger(i) <= nbSup)) begin
               // Inst_16b or Inst_32b
               // get the input to decode
               let inst_data_shifted = shiftInAtN (inst_data, ?);    // for predicted PCs
               let in = InstrFromFetch3 {
                  pc: inst_data[i].pc,
                  // last inst, next pc may not be pc+2/pc+4
                  ppc: ((fromInteger(i) == nbSup)
                        ? decodeIn.pred_next_pc
                        : inst_data_shifted[i].pc),
                  decode_epoch: decodeIn.decode_epoch,
                  main_epoch: decodeIn.main_epoch,
                  inst: inst_data [i].inst,        // original 32b inst, or expanded version of 16b inst
                  cause: decodeIn.cause
                  };
               let cause = in.cause;
               if (verbose)
                  $display("Decode: %0d in = ", i, fshow (in));

               // do decode and branch prediction
               // Drop here if does not match the decode_epoch.

               // We predicted a taken branch for PC, but this is an
               // uncompressed instruction, so we redirect to this PC and
               // train it to fetch the other half in future.
               if (in.decode_epoch == decode_epoch_local && i==0 && decodeIn.mispred_first_half) begin
                  if (verbose) $display("mispredicted first half in decode: pc :  %h", in.pc);
                  decode_epoch_local = !decode_epoch_local;
                  redirectPc = Valid (in.pc); // record redirect to the first PC in this bundle.
                  trainNAP = Valid (TrainNAP {pc: in.pc, nextPc: addPc(in.pc, 2)});
               end else if (in.decode_epoch == decode_epoch_local) begin
                  doAssert(in.main_epoch == f_main_epoch, "main epoch must match");

                  let decode_result = decode(in.inst, getFlags(inst_data[i].pc)==1);    // Decode 32b inst, or 32b expansion of 16b inst

                  // update cause if decode exception and no earlier (TLB) exception
                  if (!isValid(cause)) begin
                     cause = decode_result.illegalInst ? tagged Valid excIllegalInst : tagged Invalid;
                  end

                  let dInst = decode_result.dInst;
		  let regs = decode_result.regs;
		  DirPredTrainInfo dp_train = ?; // dir pred training bookkeeping

		  // update predicted next pc
		  if (!isValid(cause)) begin
		     // direction predict
		     Bool pred_taken = False;
		     if(dInst.iType == Br) begin
			let pred_res <- dirPred.pred[i].pred(in.pc);
			pred_taken = pred_res.taken;
			dp_train = pred_res.train;
		     end
                     Maybe#(Addr) nextPc = decodeBrPred(in.pc, dInst, pred_taken, (inst_data[i].inst_kind == Inst_32b));

		     // return address stack link reg is x1 or x5
		     function Bool linkedR(Maybe#(ArchRIndx) register);
			Bool res = False;
			if (register matches tagged Valid .r &&& (r == tagged Gpr 1 || r == tagged Gpr 5)) begin
			   res = True;
			end
			return res;
		     endfunction
                     Bool dst_link = linkedR(regs.dst);
		     Bool src1_link = linkedR(regs.src1);
		     Addr push_addr = in.pc + ((inst_data[i].inst_kind == Inst_32b) ? 4 : 2);

		     Addr pop_addr = ras.ras[i].first;
		     if (dInst.iType == J && dst_link) begin
			// rs1 is invalid, i.e., not link: push
			ras.ras[i].popPush(False, Valid (push_addr));
		     end
                     else if (dInst.iType == Jr) begin // jalr
			if (!dst_link && src1_link) begin
			   // rd is link while rs1 is not: pop
			   nextPc = Valid (pop_addr);
			   ras.ras[i].popPush(True, Invalid);
			end
                        else if (!src1_link && dst_link) begin
			   // rs1 is not link while rd is link: push
			   ras.ras[i].popPush(False, Valid (push_addr));
			end
                        else if (dst_link && src1_link) begin
			   // both rd and rs1 are links
			   if (regs.src1 != regs.dst) begin
			      // not same reg: first pop, then push
			      nextPc = Valid (pop_addr);
			      ras.ras[i].popPush(True, Valid (push_addr));
			   end
                           else begin
			      // same reg: push
			      ras.ras[i].popPush(False, Valid (push_addr));
			   end
                        end
			end

                     if(verbose) begin
			$display("Branch prediction: ", fshow(dInst.iType), " ; ", fshow(in.pc), " ; ",
				 fshow(in.ppc), " ; ", fshow(pred_taken), " ; ", fshow(nextPc));
		     end

                     // check previous mispred
                     if (nextPc matches tagged Valid .decode_pred_next_pc &&& (decode_pred_next_pc != in.ppc)) begin
                        if (verbose) $display("ppc and decodeppc :  %h %h", in.ppc, decode_pred_next_pc);
                        decode_epoch_local = !decode_epoch_local;
                        redirectPc = Valid (decode_pred_next_pc); // record redirect next pc
                        in.ppc = decode_pred_next_pc;
                        // train next addr pred when mispredict
                        let last_x16_pc = addPc(in.pc, ((inst_data[i].inst_kind == Inst_32b) ? 2 : 0));
                        trainNAP = Valid (TrainNAP {pc: last_x16_pc, nextPc: decode_pred_next_pc});
`ifdef PERF_COUNT
			// performance stats: record decode redirect
			doAssert(redirectInst == Invalid, "at most 1 decode redirect per cycle");
			redirectInst = Valid (dInst.iType);
`endif
		     end
		  end // if (!isValid(cause))
                  let out = FromFetchStage{pc: in.pc,
					   ppc: in.ppc,
					   main_epoch: in.main_epoch,
					   dpTrain: dp_train,
					   inst: in.inst,
					   dInst: dInst,
					   orig_inst: inst_data[i].orig_inst,
					   regs: decode_result.regs,
					   cause: cause,
					   tval:  tval};
		  out_fifo.enqS[i].enq(out);
                  if (verbosity >= 1) begin
		     $write ("%0d: %m.rule doDecode: out_fifo.enqS[%0d].enq", cur_cycle, i);
		     $display (" pc %0h  inst %08h", out.pc, out.orig_inst);
		  end
                  if (verbosity >= 2) begin
		     $display ("    ", fshow(out));
		  end
	       end // if (in.decode_epoch == decode_epoch_local)
               else begin
		  if (verbose) $display("Drop decoded within a superscalar");
		  // just drop wrong path instructions
	       end
	    end
            else if (inst_data[i].inst_kind == Inst_None && fromInteger(i) <= nbSup) begin
	       // inst num is less than expected; this should not happen
	       // because both I$ and boot rom are aligned to cache line
	       // size.
               doAssert(False, "Fetched insts not enough");
	    end // if (inst_data[i].inst_kind!= Inst_None && (fromInteger(i) <= nbSup))
         end // for (Integer i = 0; i < valueof(SupSize); i=i+1)

         // update PC and epoch
         if(redirectPc matches tagged Valid .nextPc) begin
	    pc_reg[pc_decode_port] <= nextPc;
         end
         decode_epoch[0] <= decode_epoch_local;
         // send training data for next addr pred
         if (trainNAP matches tagged Valid .x) begin
	    napTrainByDecQ.enq(x);
         end
`ifdef PERF_COUNT
         // performance counter: check whether redirect happens
         if(redirectInst matches tagged Valid .iType &&& doStats) begin
	    case(iType)
	       Br: decRedirectBrCnt.incr(1);
	       J : decRedirectJmpCnt.incr(1);
	       Jr: decRedirectJrCnt.incr(1);
	       default: decRedirectOtherCnt.incr(1);
	    endcase
         end
`endif
      end // if (decodeIn.main_epoch == f_main_epoch)
      else begin
         if (verbose) $display("drop in fetch3decode");
      end
   endrule

    // train next addr pred: we use a wire to catch outputs of napTrainByDecQ.
    // This prevents napTrainByDecQ from clogging doDecode rule when
    // superscalar size is large
    (* fire_when_enabled *)
    rule setTrainNAPByDec;
        napTrainByDecQ.deq;
        napTrainByDec.wset(napTrainByDecQ.first);
    endrule

    (* fire_when_enabled, no_implicit_conditions *)
    rule doTrainNAP(isValid(napTrainByDec.wget) || isValid(napTrainByExe.wget));
        // Give priority to train from exe. This is because exe has train data
        // only when misprediction happens, i.e., train by dec is already at
        // wrong path.
        TrainNAP train = fromMaybe(validValue(napTrainByDec.wget), napTrainByExe.wget);
        nextAddrPred.update(train.pc, train.nextPc, train.nextPc != train.pc + 2);
    endrule

    // Security: we can flush when front end is empty, i.e.
    // (1) Fetch1 is stalled for waiting flush
    // (2) all internal FIFOs are empty (the output sup fifo needs not to be
    // empty, but why leave this security hole)
    Bool empty_for_flush = waitForFlush &&
                           !f12f2.notEmpty && !f22f3.notEmpty &&
                           !f32d.notEmpty && out_fifo.internalEmpty;

    interface Vector pipelines = out_fifo.deqS;
    interface iTlbIfc = iTlb;
    interface iMemIfc = iMem;
    interface mmioIfc = mmio.toCore;

    method Action start(Addr start_pc);
        pc_reg[0] <= start_pc;
        started <= True;
        waitForRedirect <= False;
        waitForFlush <= False;
    endmethod
    method Action stop();
        started <= False;
    endmethod

    method Action setWaitRedirect;
        waitForRedirect <= True;
        setWaitRedirect_redirect_conflict.wset(?); // conflict with redirect
    endmethod
    method Action redirect(Addr new_pc);
        if (verbose) $display("Redirect: newpc %h, old f_main_epoch %d, new f_main_epoch %d",new_pc,f_main_epoch,f_main_epoch+1);
        pc_reg[pc_redirect_port] <= new_pc;
        f_main_epoch <= (f_main_epoch == fromInteger(valueOf(NumEpochs)-1)) ? 0 : f_main_epoch + 1;
        ehr_pending_straddle[1] <= tagged Invalid;
        // redirect comes, stop stalling for redirect
        waitForRedirect <= False;
        setWaitRedirect_redirect_conflict.wset(?); // conflict with setWaitForRedirect
        // this redirect may be caused by a trap/system inst in commit stage
        // we conservatively set wait for flush TODO make this an input parameter
        waitForFlush <= True;
    endmethod

`ifdef INCLUDE_GDB_CONTROL
   method Action setWaitFlush;
      waitForFlush <= True;
      // $display ("%0d.%m.setWaitFlush", cur_cycle);
   endmethod
`endif

    method Action done_flushing() if (waitForFlush);
        // signal that the pipeline can resume fetching
        waitForFlush <= False;
        // XXX The guard prevents the readyToFetch rule in Core.bsv from firing every cycle
        // The guard also makes this method sequence before (restricted) redirect method
        // So the effect of setting waitForFlush in redirect method will not be overwritten
        // Then we don't need to make two methods conflict
        // It's fine for the effect of this method to be overwritten, because it fires very often
    endmethod

    method Action train_predictors(
        Addr pc, Addr next_pc, IType iType, Bool taken,
        DirPredTrainInfo dpTrain, Bool mispred, Bool isCompressed
    );
        //if (iType == J || (iType == Br && next_pc < pc)) begin
        //    // Only train the next address predictor for jumps and backward branches
        //    // next_pc != pc + 4 is a substitute for taken
        //    nextAddrPred.update(pc, next_pc, taken);
        //end
        if (iType == Br) begin
            // Train the direction predictor for all branches
            dirPred.update(pc, taken, dpTrain, mispred);
        end
        // train next addr pred when mispred
        if(mispred) begin
            let last_x16_pc = pc + (isCompressed ? 0 : 2);
            napTrainByExe.wset(TrainNAP {pc: last_x16_pc, nextPc: next_pc});
        end
    endmethod

    // security
    method Bool emptyForFlush;
        return empty_for_flush;
    endmethod

    method Action flush_predictors;
        nextAddrPred.flush;
        dirPred.flush;
        ras.flush;
    endmethod

    method Bool flush_predictors_done;
        return nextAddrPred.flush_done && dirPred.flush_done && ras.flush_done;
    endmethod

    method FetchDebugState getFetchState;
        return FetchDebugState {
            pc: pc_reg[0],
            waitForRedirect: waitForRedirect,
            waitForFlush: waitForFlush,
            mainEp: f_main_epoch
        };
    endmethod

    interface Perf perf;
        method Action setStatus(Bool stats);
`ifdef PERF_COUNT
            doStats <= stats;
`else
            noAction;
`endif
        endmethod

        method Action req(DecStagePerfType r);
            perfReqQ.enq(r);
        endmethod

        method ActionValue#(PerfResp#(DecStagePerfType)) resp;
`ifdef PERF_COUNT
            perfRespQ.deq;
            return perfRespQ.first;
`else
            perfReqQ.deq;
            return PerfResp {
                pType: perfReqQ.first,
                data: 0
            };
`endif
        endmethod

`ifdef PERF_COUNT
        method Bool respValid = perfRespQ.notEmpty;
`else
        method Bool respValid = perfReqQ.notEmpty;
`endif
    endinterface
endmodule
