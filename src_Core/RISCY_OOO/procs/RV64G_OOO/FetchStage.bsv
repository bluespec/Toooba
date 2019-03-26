
// Copyright (c) 2017 Massachusetts Institute of Technology
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
import Fifo::*;
import GetPut::*;
import MemoryTypes::*;
import Types::*;
import ProcTypes::*;
import CCTypes::*;
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
    method Action done_flushing();
    method Action train_predictors(
        Addr pc, Addr next_pc, IType iType, Bool taken,
        DirPredTrainInfo dpTrain, Bool mispred
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
    Addr pred_next_pc;
    Bool decode_epoch;
    Epoch main_epoch;
} Fetch1ToFetch2 deriving(Bits, Eq, FShow);

typedef struct {
    Addr pc;
    Addr phys_pc;
    Addr pred_next_pc;
    Maybe#(Exception) cause;
    Bool access_mmio; // inst fetch from MMIO
    Bool decode_epoch;
    Epoch main_epoch;
} Fetch2ToFetch3 deriving(Bits, Eq, FShow);

typedef struct {
  Addr pc;
  Addr ppc;
  Bool decode_epoch;
  Epoch main_epoch;
  Instruction inst;
  Maybe#(Exception) cause;
} Fetch3ToDecode deriving(Bits, Eq, FShow);

typedef struct {
  Addr pc;
  Addr ppc;
  Epoch main_epoch;
  DirPredTrainInfo dpTrain;
  Instruction inst;
  DecodedInst dInst;
  ArchRegs regs;
  Maybe#(Exception) cause;
} FromFetchStage deriving (Bits, Eq, FShow);

// train next addr pred (BTB)
typedef struct {
    Addr pc;
    Addr nextPc;
} TrainNAP deriving(Bits, Eq, FShow);

(* synthesize *)
module mkFetchStage(FetchStage);
    // rule ordering: Fetch1 (BTB+TLB) < Fetch3 (decode & dir pred) < redirect method
    // Fetch1 < Fetch3 to avoid bypassing path on PC and epochs

    let verbose = True;

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

    Ehr#(3, Addr) pc_reg <- mkEhr(0);
    Integer pc_fetch1_port = 0;
    Integer pc_decode_port = 1;
    Integer pc_redirect_port = 2;

    // Epochs
    Reg#(Bool) decode_epoch <- mkReg(False);
    Reg#(Epoch) f_main_epoch <- mkReg(0); // fetch estimate of main epoch

    // Pipeline Stage FIFOs
    Fifo#(2, Tuple2#(Bit#(TLog#(SupSize)),Fetch1ToFetch2)) f12f2 <- mkCFFifo;
    Fifo#(4, Tuple2#(Bit#(TLog#(SupSize)),Fetch2ToFetch3)) f22f3 <- mkCFFifo; // FIFO should match I$ latency
    Fifo#(2, Tuple2#(Bit#(TLog#(SupSize)),Fetch2ToFetch3)) f32d <- mkCFFifo;
    Fifo#(2, Vector#(SupSize,Maybe#(Instruction))) instdata <- mkPipelineFifo();
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
    Server#(Addr, Vector#(SupSize, Maybe#(Instruction))) mem_server = iMem.to_proc;

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
        Vector#(SupSize, Addr) pred_future_pc;
        for(Integer i = 0; i < valueof(SupSize); i = i+1) begin
            pred_future_pc[i] = nextAddrPred.predPc(pc + fromInteger(4 * i));
        end

        // Next pc is the first nextPc that breaks the chain of pc+4 or
        // that is at the end of a cacheline.
        Vector#(SupSize,Integer) indexes = genVector;
        function Bool findNextPc(Addr pc, Integer i);
            Bool notLastInst = getLineInstOffset(pc + fromInteger(4*i)) != maxBound;
            Bool noJump = pred_future_pc[i] == pc + fromInteger(4*(i+1));
            return (!(notLastInst && noJump));
        endfunction
        Integer posLastSup = fromMaybe(valueof(SupSize) - 1, find(findNextPc(pc), indexes));
        Addr pred_next_pc = pred_future_pc[posLastSup];
        pc_reg[pc_fetch1_port] <= pred_next_pc;

        // Send TLB request
        tlb_server.request.put(pc);

        let out = Fetch1ToFetch2 {
            pc: pc,
            pred_next_pc: pred_next_pc,
            decode_epoch: decode_epoch,
            main_epoch: f_main_epoch};
        f12f2.enq(tuple2(fromInteger(posLastSup),out));
        if (verbose) $display("Fetch1: ", fshow(out));
    endrule

    rule doFetch2;
        let {nbSup,in} = f12f2.first;
        f12f2.deq;

        // Get TLB response
        match {.phys_pc, .cause} <- tlb_server.response.get;

        // Access main mem or boot rom
        Bool access_mmio = False;
        if (!isValid(cause)) begin
            case(mmio.getFetchTarget(phys_pc))
                MainMem: begin
                    // Send ICache request
                    mem_server.request.put(phys_pc);
                end
                BootRom: begin
                    // Send MMIO req. Luckily boot rom is also aligned with
                    // cache line size, so all nbSup+1 insts can be fetched
                    // from boot rom. It won't happen that insts fetched from
                    // boot rom is less than requested.
                    mmio.bootRomReq(phys_pc, nbSup);
                    access_mmio = True;
                end
                default: begin
                    // Access fault
                    cause = Valid (InstAccessFault);
                end
            endcase
        end

        let out = Fetch2ToFetch3 {
            pc: in.pc,
            phys_pc: phys_pc,
            pred_next_pc: in.pred_next_pc,
            cause: cause,
            access_mmio: access_mmio,
            decode_epoch: in.decode_epoch,
            main_epoch: in.main_epoch };
        f22f3.enq(tuple2(nbSup,out));
        if (verbose) $display("Fetch2: ", fshow(out));
    endrule
 
// Break out of i$
    rule doFetch3;
        let {nbSup, fetch3In} = f22f3.first;
        f22f3.deq();
        if (verbose) $display("Fetch3 %d",fetch3In.pc);

        // Get ICache/MMIO response if no exception
        // In case of exception, we still need to process at least inst_data[0]
        // (it will be turned to an exception later), so inst_data[0] must be
        // valid.
        Vector#(SupSize,Maybe#(Instruction)) inst_d = replicate(tagged Valid (0));
        if(!isValid(fetch3In.cause)) begin
            if(fetch3In.access_mmio) begin
                if(verbose) $display("get answer from MMIO %d", fetch3In.pc);
                inst_d <- mmio.bootRomResp;
            end
            else begin
                if(verbose) $display("get answer from memory %d", fetch3In.pc);
                inst_d <- mem_server.response.get;
            end
        end
        if(verbose) $display("epoch instr: %d, epoch main : %d", fetch3In.main_epoch, f_main_epoch);
        instdata.enq(inst_d);
        f32d.enq(f22f3.first);
    endrule

    rule doDecode;
        let {nbSup, fetch3In} = f32d.first;
        f32d.deq();
        let inst_data = instdata.first();
        instdata.deq();
        // The main_epoch check is required to make sure this stage doesn't
        // redirect the PC if a later stage already redirected the PC.
        if (fetch3In.main_epoch == f_main_epoch) begin
            Bool decode_epoch_local = decode_epoch; // next value for decode epoch
            Maybe#(Addr) redirectPc = Invalid; // next pc redirect by branch predictor
            Maybe#(TrainNAP) trainNAP = Invalid; // training data sent to next addr pred
`ifdef PERF_COUNT
            // performance counter: inst being redirect by decode stage
            // Note that only 1 redirection may happen in a cycle
            Maybe#(IType) redirectInst = Invalid;
`endif

            for (Integer i = 0; i < valueof(SupSize); i=i+1) begin
                if (inst_data[i] != tagged Invalid && fromInteger(i) <= nbSup) begin
                    // get the input to decode
                    let in = Fetch3ToDecode {
                        pc: fetch3In.pc+fromInteger(4*i),
                        // last inst, next pc may not be pc+4
                        ppc: fromInteger(i) == nbSup ? fetch3In.pred_next_pc :
                                                       fetch3In.pc+fromInteger(4*(i+1)),
                        decode_epoch: fetch3In.decode_epoch,
                        main_epoch: fetch3In.main_epoch,
                        inst: fromMaybe(?,inst_data[i]),
                        cause: fetch3In.cause
                    };
                    let cause = in.cause;
                    if (verbose) $display("Decode %d\n",i);

                    // do decode and branch prediction
                    // Drop here if does not match the decode_epoch.
                    if (in.decode_epoch == decode_epoch_local) begin
                        doAssert(in.main_epoch == f_main_epoch, "main epoch must match");

                        let decode_result = decode(in.inst);

                        // update cause if there was not an early detected exception
                        if (!isValid(cause)) begin
                            cause = decode_result.illegalInst ? tagged Valid IllegalInst : tagged Invalid;
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
                            Maybe#(Addr) nextPc = decodeBrPred(in.pc, dInst, pred_taken);

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
                            Addr push_addr = in.pc + 4;
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
                            if (nextPc matches tagged Valid .decode_pred_next_pc &&& decode_pred_next_pc != in.ppc) begin
                                if (verbose) $display("ppc and decodeppc :  %h %h", in.ppc, decode_pred_next_pc);
                                decode_epoch_local = !decode_epoch_local;
                                redirectPc = Valid (decode_pred_next_pc); // record redirect next pc
                                in.ppc = decode_pred_next_pc;
                                // train next addr pred when mispredict
                                trainNAP = Valid (TrainNAP {pc: in.pc, nextPc: decode_pred_next_pc});
`ifdef PERF_COUNT
                                // performance stats: record decode redirect
                                doAssert(redirectInst == Invalid, "at most 1 decode redirect per cycle");
                                redirectInst = Valid (dInst.iType);
`endif
                            end
                        end
                        let out = FromFetchStage{pc: in.pc,
                                                 ppc: in.ppc,
                                                 main_epoch: in.main_epoch,
                                                 dpTrain: dp_train,
                                                 inst: in.inst,
                                                 dInst: dInst,
                                                 regs: decode_result.regs,
                                                 cause: cause };
                        out_fifo.enqS[i].enq(out);
                        if (verbose) $display("Decode: ", fshow(out));
                    end
                    else begin
                        if (verbose) $display("Drop decoded within a superscalar");
                        // just drop wrong path instructions
                    end
                end
                else if (inst_data[i] == tagged Invalid && fromInteger(i) <= nbSup) begin
                    // inst num is less than expected; this should not happen
                    // because both I$ and boot rom are aligned to cache line
                    // size.
                    doAssert(False, "Fetched insts not enough");
                end
            end

            // update PC and epoch
            if(redirectPc matches tagged Valid .nextPc) begin
                pc_reg[pc_decode_port] <= nextPc;
            end
            decode_epoch <= decode_epoch_local;
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
        end
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
        nextAddrPred.update(train.pc, train.nextPc, train.nextPc != train.pc + 4);
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
        // redirect comes, stop stalling for redirect
        waitForRedirect <= False;
        setWaitRedirect_redirect_conflict.wset(?); // conflict with setWaitForRedirect
        // this redirect may be caused by a trap/system inst in commit stage
        // we conservatively set wait for flush TODO make this an input parameter
        waitForFlush <= True;
    endmethod
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
        DirPredTrainInfo dpTrain, Bool mispred
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
            napTrainByExe.wset(TrainNAP {pc: pc, nextPc: next_pc});
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
 
