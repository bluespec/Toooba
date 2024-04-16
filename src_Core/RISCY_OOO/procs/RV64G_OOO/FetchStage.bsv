
// Copyright (c) 2017 Massachusetts Institute of Technology
// Portions Copyright (c) 2019-2020 Bluespec, Inc.
//
//-
// RVFI_DII + CHERI modifications:
//     Copyright (c) 2020 Jessica Clarke
//     Copyright (c) 2020 Alexandre Joannou
//     Copyright (c) 2020 Peter Rugg
//     Copyright (c) 2020 Jonathan Woodruff
//     Copyright (c) 2024 Franz Fuchs
//     All rights reserved.
//
//     This software was developed by SRI International and the University of
//     Cambridge Computer Laboratory (Department of Computer Science and
//     Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
//     DARPA SSITH research programme.
//
//     This work was supported by NCSC programme grant 4212611/RFA 15971 ("SafeBet").
//     This software was developed by the University of  Cambridge
//     Department of Computer Science and Technology under the
//     SIPP (Secure IoT Processor Platform with Remote Attestation)
//     project funded by EPSRC: EP/S030868/1
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

`include "ProcConfig.bsv"

import BrPred::*;
import DirPredictor::*;
import Btb::*;
import ClientServer::*;
import Connectable::*;
import Decode::*;
import Ehr::*;
import Fifos::*;
import FIFOF::*;
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
import HasSpecBits::*;
import SpecFifo::*;
import TlbTypes::*;
import ITlb::*;
import CCTypes::*;
import L1CoCache::*;
import MMIOInst::*;
import CHERICap::*;
import CHERICC_Fat::*;
`ifdef RVFI_DII
import RVFI_DII_Types::*;
import Types::*;
`endif
import IndexedMultiset::*;
import Cur_Cycle :: *;
import DReg :: *;

// ================================================================
// For fv_decode_C function and related types and definitions

import ISA_Decls        :: *;
import CPU_Decode_C     :: *;

// ================================================================

`ifdef RVFI_DII
interface RvfiDiiServer;
    interface Client#(Dii_Parcel_Id, Dii_Parcels) toCore;
    interface Server#(Dii_Parcel_Id, Vector#(SupSizeX2, Maybe#(Instruction16))) fromDii;
endinterface

module mkRvfiDiiServer(RvfiDiiServer);
    Fifo#(2, Dii_Parcel_Id) reqs <- mkCFFifo;
    Fifo#(2, Vector#(SupSizeX2, Maybe#(Instruction16))) resps <- mkCFFifo;

    interface Client toCore;
        interface Get request = toGet(reqs);
        interface Put response;
            method Action put(Dii_Parcels parcels);
                function Maybe#(a) toMaybe(a x) = tagged Valid x;
                resps.enq(map(toMaybe, unpack(pack(parcels))));
            endmethod
        endinterface
    endinterface

    interface fromDii = toGPServer(reqs, resps);
endmodule
`endif

interface FetchStage;
    // pipeline
    interface Vector#(SupSize, SupFifoDeq#(FromFetchStage)) pipelines;

    // tlb and mem connections
    interface ITlb iTlbIfc;
    interface ICoCache iMemIfc;
    interface MMIOInstToCore mmioIfc;
`ifdef RVFI_DII
    interface Client#(Dii_Parcel_Id, Dii_Parcels) diiIfc;
`endif

    // starting and stopping
    method Action start(CapMem pc
`ifdef RVFI_DII
        , Dii_Parcel_Id parcel_id
`endif
    );
    method Action stop();

    // redirection methods
    method Action setWaitRedirect;
    method Action redirect(
        CapMem pc,
        SpecBits specBits
`ifdef RVFI_DII
        , Dii_Parcel_Id parcel_id
`endif
    );
`ifdef INCLUDE_GDB_CONTROL
   method Action setWaitFlush;
`endif
    method Action done_flushing();
    method Action train_predictors(
        CapMem pc, CapMem next_pc, IType iType, Bool taken, Bool link,
        PredTrainInfo trainInfo, Bool mispred, Bool isCompressed
    );
    interface SpeculationUpdate specUpdate;

    // security
    method Bool emptyForFlush;
    method Action flush_predictors;
    method Bool flush_predictors_done;

    // debug
    method FetchDebugState getFetchState;

    // performance
    interface Perf#(DecStagePerfType) perf;
`ifdef PERFORMANCE_MONITORING
    method Bool redirect_evt;
`endif
endinterface

// PC "compression" types to facilitate storing common upper PC bits in a
// shared structure
// Must be at least a page offset.
typedef 12 PcLsbSz; // Defines PC block size for PCs that will share an index for upper bits.
typedef TLog#(TMul#(SupSize,4)) PcIdxSz; // Number of distinct PC blocks allowed in-flight in the Fetch pipeline.
typedef Bit#(PcLsbSz) PcLSB;
typedef Bit#(TSub#(SizeOf#(CapMem),PcLsbSz)) PcMSB;
typedef Bit#(PcIdxSz) PcIdx;
typedef struct {
    PcLSB lsb;
    PcIdx idx;
} PcCompressed deriving(Bits,Eq,FShow);
function PcCompressed compressPc(PcIdx i, CapMem a) =
    PcCompressed{idx: i, lsb: truncate(a)};

typedef struct {
    Addr pc;
    Epoch mainEp;
    Bool waitForRedirect;
    Bool waitForFlush;
} FetchDebugState deriving(Bits, Eq, FShow);

typedef struct {
    PcCompressed pc;
`ifdef RVFI_DII
    Dii_Parcel_Id dii_pid;
`endif
    Bit#(TLog#(SupSizeX2)) inst_frags_fetched;
    Maybe#(PcCompressed) pred_next_pc;
    Maybe#(Exception) cause;
    Bool access_mmio; // inst fetch from MMIO
    Bool decode_epoch;
    Epoch main_epoch;
`ifdef KONATA 
    Bit#(64) u_id;
`endif
} Fetch1ToFetch2 deriving(Bits, Eq, FShow);

typedef struct {
    PcCompressed pc;
    Maybe#(PcCompressed) ppc;
    Maybe#(Exception) cause;
    Bit#(16) inst_frag;
    Bool decode_epoch;
    Epoch main_epoch;
`ifdef RVFI_DII
    Dii_Parcel_Id dii_pid;
`endif
`ifdef KONATA 
    Bit#(64) u_id;
`endif
} Fetch2ToDecode deriving(Bits, Eq, FShow);

// Used purely internally in doDecode.
typedef struct {
  PcCompressed pc;
`ifdef RVFI_DII
  Dii_Parcel_Id dii_pid;
`endif
  PcCompressed ppc;
  Bool pred_jump;
  Bool decode_epoch;
  Epoch main_epoch;
  Instruction inst;
  Bit#(32) orig_inst;
  Inst_Kind inst_kind;
  Maybe#(Exception) cause;
  Bool cause_second_half;
  Bool mispred_first_half;
`ifdef KONATA 
  Bit#(64) u_id;
`endif
} InstrFromFetch2 deriving(Bits, Eq, FShow);

function Bool popInst(DecodeResult dr);
    let dInst = dr.dInst;
    Bool doPop = False;
    Bool dst_link = linkedR(dr.regs.dst);
    Bool src1_link = linkedR(dr.regs.src1);
    if (dr.dInst.iType == Jr || dr.dInst.iType == CJALR) begin // jalr TODO CCALL could be push
                                                              //           pop or nop (if to trampoline)
                                                              //           Add hint to architecture?
       if (!dst_link && src1_link) begin
          // rd is link while rs1 is not: pop
          doPop = True;
       end
       else if (!src1_link && dst_link) begin
          doPop = doPop; // Nothing
       end
       else if (dst_link && src1_link) begin
          // both rd and rs1 are links
          if (dr.regs.src1 != dr.regs.dst) begin
             doPop = True;
          end
       end
    end
    return doPop;
endfunction

function InstrFromFetch2 fetch2_2_instC(Fetch2ToDecode in, Instruction inst, Bit#(32) orig_inst) =
   InstrFromFetch2 {
      pc: in.pc,
`ifdef RVFI_DII
      dii_pid: in.dii_pid,
`endif
      // This assumes we will call this function on the last fragment of any instruction.
      ppc: fromMaybe(PcCompressed{lsb: in.pc.lsb + 2,
                                  idx: in.pc.idx + ((in.pc.lsb == -2) ? 1:0)}, // If we move to a new page, we will move to the next index in the compressed PC table.
                     in.ppc),
      pred_jump: isValid(in.ppc),
      decode_epoch: in.decode_epoch,
      main_epoch: in.main_epoch,
      inst: inst,
      orig_inst: orig_inst,
      inst_kind: Inst_16b,
      cause: in.cause,
      cause_second_half: False,
      mispred_first_half: False
`ifdef KONATA 
      , u_id: in.u_id
`endif
   };

function InstrFromFetch2 fetch2s_2_inst(Fetch2ToDecode inHi, Fetch2ToDecode inLo);
   Instruction inst = {inHi.inst_frag, inLo.inst_frag};
   InstrFromFetch2 ret = fetch2_2_instC(inHi, inst, inst);
   if (isValid(inLo.cause)) ret.cause = inLo.cause;
   else if (isValid(inHi.cause)) ret.cause_second_half = True;
   ret.inst_kind = Inst_32b;
   ret.pc = inLo.pc; // The PC comes from the 1st fragment.
`ifdef RVFI_DII
   ret.dii_pid = inLo.dii_pid; // The dii_pid comes from the 1st fragment.
`endif
   ret.mispred_first_half = isValid(inLo.ppc); // If we predicted a jump on the first half of the 32-bit instruction, we have erred.
   return ret;
endfunction

typedef struct {
  CapMem pc;
`ifdef RVFI_DII
  Dii_Parcel_Id dii_pid;
`endif
  CapMem ppc;
  Epoch main_epoch;
  PredTrainInfo trainInfo;
  Instruction inst;
  DecodedInst dInst;
  Bit #(32) orig_inst;    // original 16b or 32b instruction ([1:0] will distinguish 16b or 32b)
  ArchRegs regs;
  Maybe#(Exception) cause;
  Addr              tval;    // in case of exception
`ifdef KONATA 
  Bit#(64) u_id;
`endif
} FromFetchStage deriving (Bits, Eq, FShow);

// train next addr pred (BTB)
typedef struct {
    CapMem pc;
    CapMem nextPc;
} TrainNAP deriving(Bits, Eq, FShow);

// "micro-TLB" size (buffer of past few translations)
typedef 2 PageBuffSize;

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

// Parsing a sequence of 16-bit parcels returns a sequence of the
// following kinds or items

typedef enum {Inst_16b,        // A 16b instruction
              Inst_32b         // A 32b instruction
   } Inst_Kind
deriving (Bits, Eq, FShow);

`ifdef KONATA
typedef struct {
    Bit#(64) puid;
    Bit#(64) cuid;
    PcCompressed pc;
} KMergedFrag deriving(Bits, Eq, FShow);
typedef struct {
    Bit#(64) cuid;
} KSingleFrag deriving(Bits, Eq, FShow);
typedef union tagged {
    KMergedFrag MergedFrag;
    KSingleFrag SingleFrag;
} KInfo deriving (Bits, Eq, FShow);

`endif

// ================================================================

(* synthesize *)
module mkFetchStage(FetchStage);
    // rule ordering: Fetch1 (BTB+TLB) < Fetch2 (decode & dir pred) < redirect method
    // Fetch1 < Fetch2 to avoid bypassing path on PC and epochs

    Bool verbose = False;
    Integer verbosity = 0;

    // Basic State Elements
    Reg#(Bool) started <- mkConfigReg(False);

    // Stall fetch when trap happens or system inst is renamed
    // All inst younger than the trap/system inst will be killed
    // Since CSR may be modified, sending wrong path request to TLB may cause problem
    // So we stall until the next redirection happens
    // The next redirect is either by the trap/system inst or an older one
    Ehr#(2, Bool) waitForRedirect <- mkEhr(False);
    Reg#(Bit#(32)) execute_redirect_count <- mkReg(0);
    Reg#(Bit#(32)) decode_redirect_count <- mkReg(0);

    // Stall fetch during the flush triggered by the procesing trap/system inst in commit stage
    // We stall until the flush is done
    Ehr#(3, Bool) waitForFlush <- mkEhr(False);

    Ehr#(5, CapMem) pc_reg <- mkEhr(nullCap);
`ifdef RVFI_DII
    Ehr#(4, Dii_Parcel_Id) dii_pid_reg <- mkEhr(0);
`endif
    Integer pc_fetch1_port = 0;
    Integer pc_decode_port = 1;
    Integer pc_fetch2_port = 2;
    Integer pc_redirect_port = 3;
    Integer pc_final_port = 4;
    // To track the next expected PC in Decode for early lookups for prediction.
    Ehr#(TAdd#(SupSize, 2), Addr) decode_pc_reg <- mkEhr(?);
    Integer decode_pc_redirect_port = valueOf(SupSize);
    Integer decode_pc_final_port = valueOf(SupSize) + 1;

    // PC compression structure holding an indexed set of PC blocks so that only indexes need be tracked.
    IndexedMultiset#(PcIdx, PcMSB, SupSizeX2) pcBlocks <- mkIndexedMultisetQueue;
    function CapMem decompressPc(PcCompressed p) = {pcBlocks.lookup(p.idx),p.lsb};
    // Epochs
    Ehr#(2, Bool) decode_epoch <- mkEhr(False);
    // fetch estimate of main epoch
    Reg#(Epoch) f_main_epoch <- mkConfigReg(0);
    SpecFifo#(2, Bit#(0), 1, 1) main_epoch_spec <- mkSpecFifoUG(True);
    function Action set_main_epoch(Epoch e, SpecBits sb) = (action
        f_main_epoch <= e;
        if (main_epoch_spec.notEmpty) main_epoch_spec.deq;
`ifdef NO_SPEC_REDIRECT
        main_epoch_spec.enq(ToSpecFifo{data: ?, spec_bits: sb});
`else
        main_epoch_spec.enq(ToSpecFifo{data: ?, spec_bits: 0});
`endif
    endaction);

    // Pipeline Stage FIFOs
    Fifo#(1, Addr) translateAddress <- mkCFFifo;
    Fifo#(2, Fetch1ToFetch2) fetch1toFetch2 <- mkCFFifo; // FIFO should match I$ latency
    // These two fifos needs a capacity of 3 for full throughput if we fire only when we can enq on all channels.
    SupFifo#(SupSizeX2, 3, Fetch2ToDecode) f2d <- mkUGSupFifo; // Unguarded to prevent the static analyser from exploding.
    SupFifo#(SupSize, 3, FromFetchStage) out_fifo <- mkSupFifo;
       // Can the fifo size be smaller?

    // Branch Predictors
    let             nextAddrPred <- mkBtb;
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
`ifdef RVFI_DII
    RvfiDiiServer dii <- mkRvfiDiiServer;
`endif
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
`ifdef PERFORMANCE_MONITORING
    Reg#(Bool) redirect_evt_reg <- mkDReg(False);
`endif
`ifdef KONATA 
    Reg#(Bit#(64)) uid <- mkReg(0);
    Reg#(Bool) k_reset <- mkReg(True);

    //rule header(k_reset);
    //    k_reset <= ! k_reset;
    //    $display("KONATAKanata\t0004");
    //    $display("KONATAC=\t0");
    //    $fflush;
    //endrule

    //rule displayCycle(!k_reset);
    //    $display("KONATAC\t1");
    //          $fflush;
    //endrule
`endif

    rule updatePcInBtb;
        nextAddrPred.put_pc(pc_reg[pc_final_port]);
    endrule

    Reg#(Vector#(PageBuffSize,Maybe#(Vpn))) buffered_translation_virt_pc <- mkReg(replicate(Invalid));
    Reg#(Vector#(PageBuffSize,TlbResp)) buffered_translation_tlb_resp <- mkRegU;
    Reg#(Bit#(TLog#(PageBuffSize))) buffered_translation_count <- mkRegU;

    rule invalidate_buffered_translation(!iTlb.flush_done);
        buffered_translation_virt_pc <= replicate(Invalid);
    endrule

    // getTlbResp catches a iTLB translation and writes it into translation
    // buffer. If there is an active iTlb flush, clear the buffer.
    rule getTlbResp;
        // Get TLB response
        TlbResp tr <- tlb_server.response.get;
        translateAddress.deq;
        if (iTlb.flush_done) begin
            // Check if, because of pipelining, we already have this vpn.
            Bool found = elem(Valid(getVpn(translateAddress.first)), buffered_translation_virt_pc);
            if (!found) begin
                buffered_translation_virt_pc[buffered_translation_count] <= Valid(getVpn(translateAddress.first));
                buffered_translation_tlb_resp[buffered_translation_count] <= tr;
                buffered_translation_count <= buffered_translation_count + 1;
            end
        end else buffered_translation_virt_pc <= replicate(Invalid);
        if (verbosity >= 2) $display ("%d Fetch Translate: pc: %x, ", cur_cycle, translateAddress.first, fshow (tr));
    endrule

    // doFetch1 pulls a prediction out of the BTB and attempts to translate it
    // from a small buffer (~2) of recent TLB translations.
    // If the necessary translation is not in the buffer, doFetch1 submits a TLB
    // lookup request and then retrys until getTlbResp has populated the buffer
    // and the lookup succeeds.
    rule doFetch1(started && !waitForRedirect[0] && !waitForFlush[0]);
        let pc = pc_reg[pc_fetch1_port];

        // Grab a chain of predictions from the BTB, which predicts targets for the next
        // set of addresses based on the current PC.
        Vector#(SupSizeX2, Maybe#(CapMem)) pred_future_pc = nextAddrPred.pred;

        // Next pc is the first nextPc that breaks the chain of pc+4 or
        // that is at the end of a cacheline.
        Vector#(SupSizeX2,Integer) indexes = genVector;
        function Bool findNextPc(CapMem in_pc, Integer i);
            Bool notLastInst = getLineInstOffset(getAddr(in_pc) + fromInteger(2*i)) != maxBound;
            Bool noJump = !isValid(pred_future_pc[i]);
            return (!(notLastInst && noJump));
        endfunction
        Bit#(TLog#(SupSizeX2)) posLastSupX2 = fromInteger(fromMaybe(valueof(SupSizeX2) - 1, find(findNextPc(pc), indexes)));
        Maybe#(CapMem) pred_next_pc = pred_future_pc[posLastSupX2];

        // Search the last few translations to look for a match.
        Maybe#(UInt#(TLog#(PageBuffSize))) m_buff_match_idx = findElem(Valid(getVpn(getAddr(pc))), buffered_translation_virt_pc);
        if (m_buff_match_idx matches tagged Valid .buff_match_idx) begin
            let next_fetch_pc = fromMaybe(addPc(pc, 2 * (zeroExtend(posLastSupX2) + 1)), pred_next_pc);
            let pc_idxs <- pcBlocks.insertAndReserve(truncateLSB(pc), truncateLSB(next_fetch_pc));
            PcIdx pc_idx = pc_idxs.inserted;
            PcIdx ppc_idx = pc_idxs.reserved;
`ifdef RVFI_DII
            Dii_Parcel_Id dii_pid = dii_pid_reg[pc_fetch1_port];
            dii_pid_reg[pc_fetch1_port] <= dii_pid + (zeroExtend(posLastSupX2) + 1);
`endif
            match {.buffered_phys_pc, .cause, .allow_cap} = buffered_translation_tlb_resp[buff_match_idx];
            Addr phys_pc = unpack({buffered_phys_pc[63:12],getAddr(pc)[11:0]});
            // Access main mem or boot rom if no TLB exception
            Bool access_mmio = False;
`ifdef RVFI_DII
            // We 32-bit align PC (and increment nbSupX2 accordingly) in
            // doFetch1 for the real MMIO and ICache require 32-bit, so make
            // DII look like that by decrementing pid if PC is "odd"; this
            // extra parcel on the front will be discarded by fav_parse_insts.
            dii.fromDii.request.put(dii_pid);
`else
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
                        mmio.bootRomReq(phys_pc, posLastSupX2);
                        access_mmio = True;
                    end
                    default: begin
                        // Access fault
                        cause = Valid (excInstAccessFault);
                    end
                endcase
            end
`endif
            Fetch1ToFetch2 out = Fetch1ToFetch2 {
                pc: compressPc(pc_idx, pc),
`ifdef RVFI_DII
                dii_pid: dii_pid,
`endif
                inst_frags_fetched: posLastSupX2,
                pred_next_pc: isValid(pred_next_pc) ?
                    Valid(compressPc(ppc_idx, validValue(pred_next_pc))) : Invalid,
                cause: cause,
                access_mmio: access_mmio,
                decode_epoch: decode_epoch[0],
                main_epoch: f_main_epoch
`ifdef KONATA
                , u_id: uid
`endif
            };
            fetch1toFetch2.enq(out);

`ifdef KONATA 
            Bit#(TAdd#(TLog#(SupSizeX2),1)) posLastSupX2ex = zeroExtend(posLastSupX2);
            uid <= uid + fromInteger(valueof(SupSizeX2));
            for (Integer i = 0;  fromInteger(i) <= posLastSupX2ex; i = i+1)
            begin
                $display("KONATAI\t%0d\t%0d\t%0d\t0", cur_cycle, uid + fromInteger(i), uid + fromInteger(i));
                $display("KONATAS\t%0d\t%0d\t0\tF1", cur_cycle, uid + fromInteger(i));
                $fflush;
            end 
`endif
            if (verbosity >= 2) begin
                $display ("%d ----------------", cur_cycle);
                $display ("%d Fetch1: translated pyhs_pc 0x%0h  cause ", cur_cycle, phys_pc, fshow (cause));
                $display ("%d Fetch1: fetch1toFetch2.enq: out ", cur_cycle, fshow (out));
            end
            pc_reg[pc_fetch1_port] <= next_fetch_pc;
            if (verbose) $display("%d Fetch1: ", cur_cycle, fshow(out), " posLastSupX2: %d", posLastSupX2);
        end else begin
            // Send TLB request.
            translateAddress.enq(getAddr(pc));
            tlb_server.request.put (getAddr(pc));
            if (verbose) $display("%d Fetch1 lookup: ", cur_cycle, " posLastSupX2: %d", posLastSupX2);
        end
    endrule

// Break out of i$
    Vector#(SupSizeX2,Integer) indexes = genVector;
    function Bool f2d_lane_notFull(Integer i) = f2d.enqS[i].canEnq;
    rule doFetch2(all(f2d_lane_notFull, indexes));
        let fetch2In = fetch1toFetch2.first;
        if (verbosity >= 2) begin
            if (fetch1toFetch2.notEmpty)
                $display("%d Fetch2: fetch2In: ", cur_cycle, fshow (fetch2In));
            else
                $display("%d Fetch2: Nothing else from Fetch1", cur_cycle);
        end

        // Get ICache/MMIO response if no exception
        // In case of exception, we still need to process at least inst_data[0]
        // (it will be turned to an exception later), so inst_data[0] must be
        // valid.
        Vector#(SupSizeX2,Maybe#(Instruction16)) inst_d = replicate(tagged Valid (0));
        fetch1toFetch2.deq();
`ifdef RVFI_DII
        inst_d <- dii.fromDii.response.get;
`else
        if (!isValid(fetch2In.cause)) begin
           if(fetch2In.access_mmio) begin
              inst_d <- mmio.bootRomResp;
              if(verbose) $display("get answer from MMIO 0x%0x", getAddr(decompressPc(fetch2In.pc)), " ", fshow(inst_d));
           end
           else begin
              if(verbose) $display("get answer from memory 0x%0x", getAddr(decompressPc(fetch2In.pc)));
                 inst_d <- mem_server.response.get;
           end
        end
`endif

        for (Integer i = 0; i < valueOf(SupSizeX2) && fromInteger(i) <= fetch2In.inst_frags_fetched; i = i + 1) begin
           PcCompressed pc = fetch2In.pc;
           pc.lsb = pc.lsb + (2 * fromInteger(i));
           f2d.enqS[i].enq (Fetch2ToDecode {
               pc: pc,
`ifdef RVFI_DII
               dii_pid: fetch2In.dii_pid + fromInteger(i),
`endif
               ppc: (fromInteger(i)==fetch2In.inst_frags_fetched) ? fetch2In.pred_next_pc : Invalid,
               inst_frag: validValue(inst_d[i]),
               cause: fetch2In.cause,
               decode_epoch: fetch2In.decode_epoch,
               main_epoch: fetch2In.main_epoch
`ifdef KONATA 
               , u_id: fetch2In.u_id + fromInteger(i)
`endif
           });
`ifdef KONATA 
           $display("KONATAE\t%0d\t%0d\t0\tF1", cur_cycle, fetch2In.u_id + fromInteger(i));
           $display("KONATAS\t%0d\t%0d\t0\tF2", cur_cycle, fetch2In.u_id + fromInteger(i));
           $fflush;
`endif
        end
    endrule: doFetch2

   function Bool isCurrent(Fetch2ToDecode in) = (main_epoch_spec.notEmpty &&
                                                 in.main_epoch == f_main_epoch &&
                                                 in.decode_epoch == decode_epoch[0]);

   rule doDecodeFlush(f2d.deqS[0].canDeq && !isCurrent(f2d.deqS[0].first));
      for (Integer i = 0; i < valueOf(SupSizeX2); i = i + 1)
         if (f2d.deqS[i].canDeq &&& !isCurrent(f2d.deqS[i].first)) begin
            pcBlocks.rPort[i].remove(f2d.deqS[i].first.pc.idx);
            f2d.deqS[i].deq;
`ifdef KONATA 
            $display("KONATAL\t%0d\t%0d\t0\tWrongPathDecode %x", cur_cycle, f2d.deqS[i].first.u_id, f2d.deqS[i].first.pc);
            $display("KONATAE\t%0d\t%0d\t0\tF2", cur_cycle, f2d.deqS[i].first.u_id);
            $display("KONATAR\t%0d\t%0d\t%0d\t1\t//KILLDECODE", cur_cycle, f2d.deqS[i].first.u_id, f2d.deqS[i].first.u_id);
            $fflush;
`endif
         end
   endrule: doDecodeFlush

   Vector#(SupSize, Maybe#(InstrFromFetch2)) decodeIn = replicate(Invalid);
   // Express the incoming fragments as a vector of maybes.
   Vector#(SupSizeX2, Maybe#(Fetch2ToDecode)) frags;
   for (Integer i = 0; i < valueOf(SupSizeX2); i = i + 1)
      frags[i] = (f2d.deqS[i].canDeq) ? Valid (f2d.deqS[i].first) : Invalid;
   // Pick as up to SupSize instructions from the f2d SupFifo.
   // Stop picking when we have SupSize instructions or when we have exhausted the ports on the instruction fragment FIFO.
   Maybe#(Bit#(TLog#(SupSizeX2))) m_used_frag_count = Invalid;
   Bit#(TLog#(SupSize)) pick_count = 0;
   Bool prev_frag_available = False;
`ifdef KONATA
   Vector#(SupSizeX2, Maybe#(KInfo)) kinfos = replicate(Invalid);
`endif
   for (Integer i = 0; i < valueOf(SupSizeX2) && !isValid(decodeIn[valueOf(SupSize) - 1]); i = i + 1) begin
      Maybe#(InstrFromFetch2) new_pick = Invalid;
      if (frags[i] matches tagged Valid .frag) begin
         Fetch2ToDecode prev_frag = (i != 0) ? validValue(frags[i-1]) : ?;
         if (prev_frag_available &&& !is_16b_inst(prev_frag.inst_frag)) begin // 2nd half of 32-bit instruction
`ifdef KONATA
            kinfos[i] = Valid (tagged MergedFrag ( KMergedFrag{ puid: prev_frag.u_id, cuid: fromMaybe(?,frags[i]).u_id, pc: prev_frag.pc}));
`endif
            new_pick = tagged Valid fetch2s_2_inst(frag, prev_frag);

            /*if (!validValue(new_pick).mispred_first_half) begin
               doAssert(getAddr(decompressPc(prev_frag.pc))+2 == getAddr(decompressPc(frag.pc)), "Attached fragments with non-contigious PCs");
   `ifdef RVFI_DII
               doAssert(prev_frag.dii_pid+1 == frag.dii_pid, "Attached fragments with non-contigious DII IDs");
   `endif
            end*/
         end else if (is_16b_inst(frag.inst_frag) || isValid(frag.cause)) begin // 16-bit instruction
`ifdef KONATA
            kinfos[i] = Valid (tagged SingleFrag ( KSingleFrag{ cuid: fromMaybe(?,frags[i]).u_id}));
`endif
            new_pick = tagged Valid fetch2_2_instC(frag,
                                                   fv_decode_C (misa, misa_mxl_64, getFlags(decompressPc(frag.pc))==1, frag.inst_frag),
                                                   zeroExtend(frag.inst_frag));
         end
      end
      decodeIn[pick_count] = new_pick;
      if (isValid(new_pick)) begin
         //if (verbose)
         //    $display("Decode: picked instruction %d, next frag %d :", pick_count, i, fshow(decodeIn[pick_count]));
         pick_count = pick_count + truncate(8'b1);
         m_used_frag_count = tagged Valid fromInteger(i);
         prev_frag_available = False;
      end else prev_frag_available = isValid(frags[i]);
   end
   Bool anyReturns = False;
   Vector#(SupSize, DecodeResult) decodeResults = ?;
   for (Integer i = 0; i < valueOf(SupSize); i = i + 1) begin
      CapMem pc = decompressPc(validValue(decodeIn[i]).pc);
      decodeResults[i] = decode(validValue(decodeIn[i]).inst, getFlags(pc)==1); // Decode 32b inst, or 32b expansion of 16b inst
      if (popInst(decodeResults[i])) anyReturns = True;
   end

   Bool delay_epoch = False;
`ifdef NO_SPEC_REDIRECT
   // Delay decode if the head of this buffer matches the current epoch;
   // this means the source of this epoch is still speculative.
   delay_epoch = (main_epoch_spec.first.spec_bits != 0);
`endif
`ifdef NO_SPEC_RSB_PUSH
   Bool delayForPop = ras.pendingPush && anyReturns;
   rule showDelayForPop(delayForPop);
      $display("RAS delayForPop; ras.pendingPush: %x, anyReturns: %x    %d", ras.pendingPush, anyReturns, cur_cycle);
   endrule
   delay_epoch = delay_epoch || delayForPop;
`endif


`ifdef KONATA
   rule doPrintFragKONATA;
      for (Integer i = 0; i < valueOf(SupSizeX2); i = i + 1) begin
        if(kinfos[i] matches tagged Valid .k) begin
            $display(k);
            $fflush;
            if(k matches tagged MergedFrag .m) begin
                $display("KONATAL\t%0d\t%0d\t0\tBrought Fragment %x", cur_cycle, m.puid, m.pc);
                $display("KONATAE\t%0d\t%0d\t0\tF2", cur_cycle, m.puid);
                $display("KONATAR\t%0d\t%0d\t%0d\t1\t//MERGE FRAGMENT", cur_cycle, m.puid, m.puid);
                $display("KONATAE\t%0d\t%0d\t0\tF2", cur_cycle, m.cuid);
                $display("KONATAS\t%0d\t%0d\t0\tD", cur_cycle, m.cuid);
                $fflush;
            end
            else if(k matches tagged SingleFrag .s) begin
                $display("KONATAE\t%0d\t%0d\t0\tF2", cur_cycle, s.cuid);
                $display("KONATAS\t%0d\t%0d\t0\tD", cur_cycle, s.cuid);
                $fflush;
            end
        end
      end
   endrule
`endif

    rule doDecode(f2d.deqS[0].canDeq && isCurrent(f2d.deqS[0].first) && !delay_epoch);
      if (m_used_frag_count matches tagged Valid .used_frag_count) begin
         for (Integer i = 0; i < valueOf(SupSizeX2) && fromInteger(i) <= used_frag_count; i = i + 1) f2d.deqS[i].deq;
         if (verbose)
            $display("%d Decode: dequed %d instruction fragments", cur_cycle, used_frag_count);
      end

      Maybe#(CapMem) redirectPc = Invalid; // next pc redirect by branch predictor
`ifdef RVFI_DII
      Maybe#(Dii_Parcel_Id) redirectDiiPid = Invalid;
`endif
      Maybe#(TrainNAP) trainNAP = Invalid; // training data sent to next addr pred
      Bool decode_epoch_local = decode_epoch[0]; // next value for decode epoch
`ifdef PERF_COUNT
      // performance counter: inst being redirect by decode stage
      // Note that only 1 redirection may happen in a cycle
      Maybe#(IType) redirectInst = Invalid;
`endif
      Bool likely_epoch_change = False;
      Maybe#(CapMem) m_push_addr = Invalid;
      for (Integer i = 0; i < valueof(SupSize); i=i+1) begin
         CapMem pc = decompressPc(validValue(decodeIn[i]).pc);
         CapMem ppc = decompressPc(validValue(decodeIn[i]).ppc);
         let decode_result = decodeResults[i]; // Decode 32b inst, or 32b expansion of 16b inst
         let dInst = decode_result.dInst;
         let regs = decode_result.regs;
         PredTrainInfo trainInfo = ?; // dir pred training bookkeeping
         DirPredResult#(DirPredTrainInfo) pred_res = DirPredResult{taken: False, train: ?};
         if(dInst.iType == Br && !likely_epoch_change) begin
            pred_res <- dirPred.pred[i].pred;
            trainInfo.dir = pred_res.train;
            likely_epoch_change = (pred_res.taken != validValue(decodeIn[i]).pred_jump);
         end
         Maybe#(CapMem) dir_ppc = decodeBrPred(pc, decode_result.dInst, pred_res.taken, (validValue(decodeIn[i]).inst_kind == Inst_32b));
         if (decodeIn[i] matches tagged Valid .in)  begin
            let cause = in.cause;
            pcBlocks.rPort[i].remove(in.pc.idx);
            if (verbose)
               $display("Decode: %0d in = ", i, fshow (in));

            // do decode and branch prediction
            // Drop here if does not match the decode_epoch.
            if (in.decode_epoch == decode_epoch_local && in.mispred_first_half) begin
               // We predicted a taken branch for PC, but this is an
               // uncompressed instruction, so we redirect to this PC and
               // train it to fetch the other half in future.
`ifdef KONATA 
                $display("KONATAE\t%0d\t%0d\t0\tD", cur_cycle, in.u_id);
                $display("KONATAL\t%0d\t%0d\t0\t%x ", cur_cycle, in.u_id, pc);
                $display("KONATAR\t%0d\t%0d\t%0d\t1\t//depoch wrong", cur_cycle, in.u_id, in.u_id);
                $fflush;
`endif
               if (verbose) $display("mispredicted first half in decode: pc :  %h", pc);
               decode_epoch_local = !decode_epoch_local;
               redirectPc = Valid (pc); // record redirect to the first PC in this bundle.
               trainNAP = Valid (TrainNAP {pc: pc, nextPc: addPc(pc, 2)});
`ifdef RVFI_DII
               redirectDiiPid = Valid (in.dii_pid);
`endif
            end else if (in.decode_epoch == decode_epoch_local) begin
               doAssert(in.main_epoch == f_main_epoch, "main epoch must match");

               // update cause if decode exception and no earlier (TLB) exception
               if (!isValid(cause)) begin
                  cause = decode_result.illegalInst ? tagged Valid excIllegalInst : tagged Invalid;
               end

               // update predicted next pc
               if (!isValid(cause)) begin
                  // direction predict
                  Maybe#(CapMem) nextPc = dir_ppc;
                  // return address stack link reg is x1 or x5
                  Bool dst_link = linkedR(regs.dst);
                  Bool src1_link = linkedR(regs.src1);

                  CapMem pop_addr = ras.ras[i].first;
                  CapMem push_addr = addPc(pc, ((in.inst_kind == Inst_32b) ? 4 : 2));
                  Bool doPop = False;
                  if ((dInst.iType == J || dInst.iType == CJAL) && dst_link) begin
                     // rs1 is invalid, i.e., not link: push
                     m_push_addr = Valid (push_addr);
                  end
                  else if (dInst.iType == Jr || dInst.iType == CJALR) begin // jalr TODO CCALL could be push
                                                                            //           pop or nop (if to trampoline)
                                                                            //           Add hint to architecture?
                     if (!dst_link && src1_link) begin
                        // rd is link while rs1 is not: pop
                        doPop = True;
                        nextPc = Valid (pop_addr);
                     end
                     else if (!src1_link && dst_link) begin
                        // rs1 is not link while rd is link: push
                        m_push_addr = Valid (push_addr);
                     end
                     else if (dst_link && src1_link) begin
                        // both rd and rs1 are links
                        if (regs.src1 != regs.dst) begin
                           // not same reg: first pop, then push
                           nextPc = Valid (pop_addr);
                           doPop = True;
                           m_push_addr = Valid (push_addr);
                        end
                        else begin
                           // same reg: push
                           m_push_addr = Valid (push_addr);
                        end
                     end
                  end
                  trainInfo.ras <- ras.ras[i].pop(doPop);
                  if(verbose) begin
                     $display("Branch prediction: ", fshow(dInst.iType), " ; ", fshow(pc), " ; ",
                              fshow(ppc), " ; ", fshow(nextPc));
                  end

`ifdef NO_SPEC_STRAIGHT_PATH
                  // If we don't have a good guess about where we are going, don't proceed.
                  if ((!isValid(nextPc)) && (!in.pred_jump)) begin
                     // Invalid virtual address to ensure redirection.
                     ppc = setAddrUnsafe(nullCap, {2'b01,?});
                     decode_epoch_local = !decode_epoch_local;
                  // check previous mispred
                  end
`endif
                  if (nextPc matches tagged Valid .decode_pred_next_pc &&& (decode_pred_next_pc != ppc)) begin
                     if (verbose) $display("%x: ppc and decodeppc :  %h %h", pc, ppc, decode_pred_next_pc);
                     decode_epoch_local = !decode_epoch_local;
                     redirectPc = Valid (decode_pred_next_pc); // record redirect next pc
`ifdef RVFI_DII
                     redirectDiiPid = Valid (in.dii_pid + ((in.inst_kind == Inst_32b) ? 2 : 1));
`endif
                     ppc = decode_pred_next_pc;
                     // train next addr pred when mispredict
                     let last_x16_pc = addPc(pc, ((in.inst_kind == Inst_32b) ? 2 : 0));
                     trainNAP = Valid (TrainNAP {pc: last_x16_pc, nextPc: decode_pred_next_pc});
`ifdef PERF_COUNT
                     // performance stats: record decode redirect
                     doAssert(redirectInst == Invalid, "at most 1 decode redirect per cycle");
                     redirectInst = Valid (dInst.iType);
`endif
                  end
               end // if (!isValid(cause))
               if (isValid(m_push_addr)) trainInfo.ras = trainInfo.ras + 1;
               decode_pc_reg[i] <= getAddr(ppc);
`ifdef KONATA 
               //$display("KONATAE\t%0d\t%0d\t0\tF3", cur_cycle, in.u_id);
               $display("KONATAL\t%0d\t%0d\t0\t%x ", cur_cycle, in.u_id, getAddr(pc), fshow(dInst.iType));
               //$display("KONATAS\t%0d\t%0d\t0\tD", cur_cycle, in.u_id);
               $fflush;
`endif
               let out = FromFetchStage{pc: pc,
`ifdef RVFI_DII
                                        dii_pid: in.dii_pid,
`endif
                                        ppc: ppc,
                                        main_epoch: in.main_epoch,
                                        trainInfo: trainInfo,
                                        inst: in.inst,
                                        dInst: dInst,
                                        orig_inst: in.orig_inst,
                                        regs: decode_result.regs,
                                        cause: cause,
                                        tval: getAddr(pc) + ((in.cause_second_half) ? 2:0)
`ifdef KONATA 
                                        , u_id : in.u_id
`endif
                                        };
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
`ifdef KONATA 
               $display("KONATAE\t%0d\t%0d\t0\tD", cur_cycle, in.u_id);
               $display("KONATAL\t%0d\t%0d\t0\t%x ", cur_cycle, in.u_id, pc);
               $display("KONATAR\t%0d\t%0d\t%0d\t1\t//depoch wrong", cur_cycle, in.u_id, in.u_id);
               $fflush;
`endif
               if (verbose) $display("Drop decoded within a superscalar");
               // just drop wrong path instructions
            end
         end // if (decodeIn[i] matches tagged Valid .in)
      end // for (Integer i = 0; i < valueof(SupSize); i=i+1)

      if (m_push_addr matches tagged Valid .pc) ras.push(pc);

      // update PC and epoch
      if(redirectPc matches tagged Valid .rp) begin
         pc_reg[pc_decode_port] <= rp;
         decode_redirect_count <= decode_redirect_count + 1;
      end
`ifdef RVFI_DII
      doAssert(isValid(redirectPc) == isValid(redirectDiiPid), "PC and DII redirections always happen together");
      if(redirectDiiPid matches tagged Valid .nextDiiPid) begin
         dii_pid_reg[pc_decode_port] <= nextDiiPid;
      end
`endif
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
            J, CJAL: decRedirectJmpCnt.incr(1);
            Jr: decRedirectJrCnt.incr(1);
            default: decRedirectOtherCnt.incr(1);
         endcase
      end
`endif
   endrule

   rule reportDecodePc;
       dirPred.nextPc(decode_pc_reg[decode_pc_final_port]);
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
        nextAddrPred.update(train.pc, train.nextPc, train.nextPc != addPc(train.pc, 2));
    endrule

    // Security: we can flush when front end is empty, i.e.
    // (1) Fetch1 is stalled for waiting flush
    // (2) all internal FIFOs are empty (the output sup fifo needs not to be
    // empty, but why leave this security hole)
    Bool empty_for_flush = waitForFlush[0] &&
                           !translateAddress.notEmpty && !fetch1toFetch2.notEmpty &&
                           f2d.internalEmpty && out_fifo.internalEmpty;

    interface Vector pipelines = out_fifo.deqS;
    interface iTlbIfc = iTlb;
    interface iMemIfc = iMem;
    interface mmioIfc = mmio.toCore;
`ifdef RVFI_DII
    interface diiIfc = dii.toCore;
`endif

    method Action start(
        CapMem start_pc
`ifdef RVFI_DII
        , Dii_Parcel_Id dii_pid
`endif
    );
        pc_reg[0] <= start_pc;
`ifdef RVFI_DII
        dii_pid_reg[0] <= dii_pid;
`endif
        started <= True;
        waitForRedirect[0] <= False;
        waitForFlush[0] <= False;
        set_main_epoch(0,0);
    endmethod
    method Action stop();
        started <= False;
    endmethod

    method Action setWaitRedirect;
        waitForRedirect[0] <= True;
    endmethod
    method Action redirect(
        CapMem new_pc,
        SpecBits specBits
`ifdef RVFI_DII
        , Dii_Parcel_Id dii_pid
`endif
    );
        if (verbose)
        $display("Redirect: newpc %h, old f_main_epoch %d, new f_main_epoch %d, specBits %x",new_pc,f_main_epoch,f_main_epoch+1, specBits);
        pc_reg[pc_redirect_port] <= new_pc;
`ifdef RVFI_DII
        dii_pid_reg[pc_redirect_port] <= dii_pid;
        if (verbose) $display("%t Redirect: dii_pid_reg %d", $time(), dii_pid);
`endif
        decode_pc_reg[decode_pc_redirect_port] <= getAddr(new_pc);
        set_main_epoch((f_main_epoch == fromInteger(valueOf(NumEpochs)-1)) ? 0 : f_main_epoch + 1, specBits);
        // redirect comes, stop stalling for redirect
        waitForRedirect[1] <= False;
        // this redirect may be caused by a trap/system inst in commit stage
        // we conservatively set wait for flush TODO make this an input parameter
        waitForFlush[2] <= True;
`ifdef PERFORMANCE_MONITORING
        redirect_evt_reg <= True;
`endif
        execute_redirect_count <= execute_redirect_count + 1;
    endmethod

`ifdef INCLUDE_GDB_CONTROL
   method Action setWaitFlush;
      waitForFlush[1] <= True;
      // $display ("%0d.%m.setWaitFlush", cur_cycle);
   endmethod
`endif

    method Action done_flushing() if (waitForFlush[0]);
        // signal that the pipeline can resume fetching
        waitForFlush[0] <= False;
        if (verbose) $display("%t : Done Flushing",$time());

        // XXX The guard prevents the readyToFetch rule in Core.bsv from firing every cycle
        // The guard also makes this method sequence before (restricted) redirect method
        // So the effect of setting waitForFlush in redirect method will not be overwritten
        // Then we don't need to make two methods conflict
        // It's fine for the effect of this method to be overwritten, because it fires very often
    endmethod

    method Action train_predictors(
        CapMem pc, CapMem next_pc, IType iType, Bool taken, Bool link,
        PredTrainInfo trainInfo, Bool mispred, Bool isCompressed
    );
        //if (iType == J || iType == CJAL || (iType == Br && next_pc < pc)) begin
        //    // Only train the next address predictor for jumps and backward branches
        //    // next_pc != pc + 4 is a substitute for taken
        //    nextAddrPred.update(pc, next_pc, taken);
        //end
`ifdef NO_SPEC_RSB_PUSH
        if (link) ras.write(addPc(pc, isCompressed ? 2 : 4), trainInfo.ras);
`endif
        if (iType == Br) begin
            // Train the direction predictor for all branches
            dirPred.update(taken, trainInfo.dir, mispred);
            $display("Branch train PC: %x, taken: %x, mispred: %x", getAddr(pc), taken, mispred);
        end
        // train next addr pred when mispred
        if(mispred) begin
            let last_x16_pc = addPc(pc, (isCompressed ? 0 : 2));
            napTrainByExe.wset(TrainNAP {pc: last_x16_pc, nextPc: next_pc});
`ifdef SPEC_RSB_FIXUP
            ras.setHead(trainInfo.ras);
`endif
        end
    endmethod

    interface SpeculationUpdate specUpdate = main_epoch_spec.specUpdate;

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
            pc: getAddr(pc_reg[0]),
            waitForRedirect: waitForRedirect[0],
            waitForFlush: waitForFlush[0],
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

`ifdef PERFORMANCE_MONITORING
    method Bool redirect_evt = redirect_evt_reg._read;
`endif
endmodule
