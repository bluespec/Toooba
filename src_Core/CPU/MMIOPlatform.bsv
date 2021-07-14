
// Copyright (c) 2018 Massachusetts Institute of Technology
// Portions (c) 2019-2020 Bluespec, Inc.
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

// This file is adapted from: MIT-riscy/riscy-OOO/procs/lib/MMIOPlatform.bsv
// Modifications to fit into Bluespec's RISC-V execution environments.

// ================================================================
// BSV lib imports

import Vector::*;
import GetPut::*;
import ClientServer::*;
import Connectable::*;
import FIFOF     :: *;

// ----------------
// BSV additional libs

import GetPut_Aux :: *;

// ================================================================
// Project imports

// ----------------
// From MIT RISCY-OOO

import Fifos::*;
import Types::*;
import ProcTypes::*;
import CCTypes::*;
import MMIOAddrs::*;
import MMIOCore::*;
import CacheUtils::*;
import Amo::*;

// ----------------
// From McStriiv

import MMIO_AXI4_Adapter  :: *;

// ================================================================
// Extract bytes from raw word read from near-mem.
// The bytes of interest are offset according to LSBs of addr.
// Arguments:
//  - a RISC-V LD/ST size (encoding B, H, W, or D)
//  - a byte-address
//  - a load-word (loaded from cache/mem)
// result:
//  - word with correct byte(s) shifted into LSBs and properly extended

Bit #(2) sz_B  = 2'b00;
Bit #(2) sz_H  = 2'b01;
Bit #(2) sz_W  = 2'b10;
Bit #(2) sz_D  = 2'b11;

function Bit #(64) fn_extract_and_extend_bytes (Bit #(2) sz, Bit #(64) byte_addr, Bit #(64) word64);
   Bit #(64) result    = 0;
   Bit #(3)  addr_lsbs = byte_addr [2:0];

   case (sz)
      sz_B: case (addr_lsbs)
	       'h0: result = zeroExtend (word64 [ 7: 0]);
	       'h1: result = zeroExtend (word64 [15: 8]);
	       'h2: result = zeroExtend (word64 [23:16]);
	       'h3: result = zeroExtend (word64 [31:24]);
	       'h4: result = zeroExtend (word64 [39:32]);
	       'h5: result = zeroExtend (word64 [47:40]);
	       'h6: result = zeroExtend (word64 [55:48]);
	       'h7: result = zeroExtend (word64 [63:56]);
	    endcase

      sz_H: case (addr_lsbs)
	       'h0: result = zeroExtend (word64 [15: 0]);
	       'h2: result = zeroExtend (word64 [31:16]);
	       'h4: result = zeroExtend (word64 [47:32]);
	       'h6: result = zeroExtend (word64 [63:48]);
	    endcase

      sz_W: case (addr_lsbs)
	       'h0: result = zeroExtend (word64 [31: 0]);
	       'h4: result = zeroExtend (word64 [63:32]);
	    endcase

      sz_D: case (addr_lsbs)    // D
	       'h0: result = word64;
	    endcase
   endcase
   return result;
endfunction

// ================================================================
// Update relevant bytes of store-value.
// The bytes of interest are offset according to LSBs of addr.
// Arguments:
//  - a RISC-V LD/ST size (encoding B, H, W, or D)
//  - a byte-address
//  - an amo result (relevant bytes are in lower-order bits)
//  - original store-val
// result:
//  - store-val with relevant byte(s) updated

function Bit #(64) fn_update_bytes (Bit #(2) sz, Bit #(64) byte_addr, Bit #(64) value, Bit #(64) st_val);
   Bit #(64) result    = 0;
   Bit #(3)  addr_lsbs = byte_addr [2:0];

   case (sz)
      sz_B: case (addr_lsbs)
	       'h0: result = { st_val [63:8],  value [7:0] };
	       'h1: result = { st_val [63:16], value [7:0], st_val [7:0] };
	       'h2: result = { st_val [63:24], value [7:0], st_val [15:0] };
	       'h3: result = { st_val [63:32], value [7:0], st_val [23:0] };
	       'h4: result = { st_val [63:40], value [7:0], st_val [31:0] };
	       'h5: result = { st_val [63:48], value [7:0], st_val [39:0] };
	       'h6: result = { st_val [63:56], value [7:0], st_val [47:0] };
	       'h7: result = {                 value [7:0], st_val [55:0] };
	    endcase

      sz_H: case (addr_lsbs)
	       'h0: result = { st_val [63:16], value [15:0] };
	       'h2: result = { st_val [63:32], value [15:0], st_val [15:0] };
	       'h4: result = { st_val [63:48], value [15:0], st_val [31:0] };
	       'h6: result = {                 value [15:0], st_val [47:0] };
	    endcase

      sz_W: case (addr_lsbs)
	       'h0: result = { st_val [63:32], value [31:0] };
	       'h4: result = {                 value [31:0], st_val [31:0] };
	    endcase

      sz_D: case (addr_lsbs)    // D
	       'h0: result = st_val;
	    endcase
   endcase
   return result;
endfunction

// ================================================================
// AMO op function
// Extracts the relevant bytes from ld_val and st_val,
// Performs the AMO op,
// Updates the relevant bytes of st_val.

function Bit #(64) fn_amo_op (Bit #(2)   sz,        // encodes data size (.W or .D)
			      AmoFunc    amofunc,   // encodes the AMO op
			      Bit #(64)  addr,      // lsbs indicate which 32b W in 64b D (.W)
			      Bit #(64)  ld_val,    // 64b value loaded from mem
			      Bit #(64)  st_val);   // 64b value from CPU reg Rs2
   // Extract relevant bytes of ld_val and st_val
   Bit #(64) w1     = fn_extract_and_extend_bytes (sz, addr, ld_val);
   Bit #(64) w2     = fn_extract_and_extend_bytes (sz, addr, st_val);


   // Do AMO op
   Int #(64) i1     = unpack (w1);    // Signed, for signed ops
   Int #(64) i2     = unpack (w2);    // Signed, for signed ops
   if (sz == 2'b10) begin
      // 32-bit word
      w1 = zeroExtend (w1 [31:0]);
      w2 = zeroExtend (w2 [31:0]);
      i1 = unpack (signExtend (w1 [31:0]));
      i2 = unpack (signExtend (w2 [31:0]));
   end
   Bit #(64) op_result = ?;
   case (amofunc)
      Swap: op_result = w2;
      Add:  op_result = pack (i1 + i2);
      Xor:  op_result = w1 ^ w2;
      And:  op_result = w1 & w2;
      Or:   op_result = w1 | w2;
      Minu: op_result = ((w1 < w2) ? w1 : w2);
      Maxu: op_result = ((w1 > w2) ? w1 : w2);
      Min:  op_result = ((i1 < i2) ? w1 : w2);
      Max:  op_result = ((i1 > i2) ? w1 : w2);
   endcase

   // Update relevant bytes of st_val
   return fn_update_bytes (sz, addr, op_result, st_val);
endfunction: fn_amo_op

// ================================================================
// MMIO logic at platform (MMIOPlatform)
// XXX Currently all MMIO requests and posts of timer interrupts are handled
// one by one in a blocking manner. This is extremely conservative. Hopefully
// this may help avoid some kernel-level problems.

interface MMIOPlatform;
   method Action start(Addr toHost, Addr fromHost);
   method ActionValue#(Data) to_host;
   method Action from_host(Data x);
endinterface

typedef enum {
   Init,
   SelectReq,
   ProcessReq,
   WaitResp
} MMIOPlatformState deriving(Bits, Eq, FShow);

// MMIO device/reg targed by the core request together with offset within
// reg/device
typedef union tagged {
   void Invalid; // invalid req target
   void TimerInterrupt; // auto-generated timer interrupt
   MSIPDataAlignedOffset MSIP;
   MTimCmpDataAlignedOffset MTimeCmp;
   void MTime;
   void ToHost;
   void FromHost;
   Addr  MMIO_Fabric_Adapter;
} MMIOPlatformReq deriving(Bits, Eq, FShow);

module mkMMIOPlatform #(Vector#(CoreNum, MMIOCoreToPlatform) cores,
			Server #(MMIOCRq, MMIODataPRs) mmio_fabric_adapter_core_side)
                      (MMIOPlatform)

   provisos (Bits #(Data, 64)); // this module assumes Data is 64-bit wide

   Integer verbosity = 0;

   // mtimecmp
   Vector#(CoreNum, Reg#(Data)) mtimecmp <- replicateM(mkReg(0));
   // mtime
   Reg#(Data) mtime <- mkReg(0);
   // HTIF mem mapped addrs
   Fifo#(1, Data) toHostQ <- mkCFFifo;
   Fifo#(1, Data) fromHostQ <- mkCFFifo;
   Reg#(DataAlignedAddr) toHostAddr <- mkReg(0);
   Reg#(DataAlignedAddr) fromHostAddr <- mkReg(0);

   // state machine
   Reg#(MMIOPlatformState) state <- mkReg(Init);

   // current req (valid when state != Init && state != SelectReq
   Reg #(MMIOPlatformReq) curReq     <- mkRegU;
   Reg #(CoreId)          reqCore    <- mkRegU;
   Reg #(MMIOFunc)        reqFunc    <- mkRegU;
   Reg #(AmoFunc)         reqAmofunc <- mkRegU;
   Reg #(ByteEn)          reqBE      <- mkRegU;
   Reg #(Bit #(2))        reqSz      <- mkRegU;
   Reg #(Data)            reqData    <- mkRegU;

   // For inst fetch, we need more bookkeepings
   // offset of the requested inst within a Data
   Reg#(DataInstOffset) instSel <- mkRegU;
   // the current superscaler way being fetched
   Reg#(SupWayX2Sel) fetchingWay <- mkRegU;
   // the already fetched insts
   Vector#(TSub#(SupSizeX2, 1),
           Reg#(Instruction16)) fetchedInsts <- replicateM(mkRegU);

   // we need to wait for resp from cores when we need to change MTIP
   Reg#(Vector#(CoreNum, Bool)) waitMTIPCRs <- mkRegU;

   // for MSIP access: lower bits and upper bits of requested memory location
   // correspond to two cores. We need to wait resp from these two cores.
   Reg#(Maybe#(CoreId)) waitLowerMSIPCRs <- mkRegU;
   Reg#(Maybe#(CoreId)) waitUpperMSIPCRs <- mkRegU;

   // in case of AMO on mtime and mtimecmp, resp may be sent after waiting for
   // CRs, we record the AMO resp at processing time
   Reg#(Data) amoResp <- mkRegU;

   // For AMOs to the fabric, we end up with read and write responses, and need
   // to discard the latter. This tracks which of the two we're waiting for.
   Reg#(Bool) amoWaitWriteResp <- mkRegU;

   // we increment mtime periodically
   Reg#(Bit#(TLog#(CyclesPerTimeInc))) cycle <- mkReg(0);

   // To avoid posting timer interrupt repeatedly, we keep a copy of MTIP
   // here. Since each core cannot write MTIP by CSRXXX inst, the only way to
   // change MTIP is through here.
   // We initialize to True to avoid an timer interrupt at start of time.
   Vector#(CoreNum, Reg#(Bool)) mtip <- replicateM(mkReg(True));

   // pass mtime to each core
   rule propagateTime(state != Init);
      for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
         cores[i].setTime(mtime);
      end
   endrule

   rule incCycle(
      state != Init &&
      cycle < fromInteger(valueof(CyclesPerTimeInc) - 1)
      );
      cycle <= cycle + 1;
   endrule

   // we don't increment mtime when processing a req
   rule incTime(
      state == SelectReq &&
      cycle >= fromInteger(valueof(CyclesPerTimeInc) - 1)
      );
      cycle <= 0;
      mtime <= mtime + fromInteger(valueof(TicksPerTimeInc));
   endrule

   // since we only process 1 MMIO req or timer interrupt at a time, we can
   // enq/deq all FIFOs in one rule

   (* preempts = "incTime, selectReq" *)
   rule selectReq(state == SelectReq);
      // check for timer interrupt
      Vector#(CoreNum, Bool) needTimerInt = replicate(False);
      for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
         if(!mtip[i] && mtimecmp[i] <= mtime) begin
	    cores[i].pRq.enq(MMIOPRq {
	       target: MTIP,
	       func: St,
	       data: 1
               });
	    mtip[i] <= True;
	    needTimerInt[i] = True;
         end
      end
      if(needTimerInt != replicate(False)) begin
         state <= WaitResp;
         curReq <= TimerInterrupt;
         waitMTIPCRs <= needTimerInt;
         if(verbosity > 0) begin
	    $display("[Platform - SelectReq] timer interrupt",
		     ", mtime %x", mtime,
		     ", mtimcmp ", fshow(readVReg(mtimecmp)),
		     ", old mtip ", fshow(readVReg(mtip)),
		     ", new interrupts ", fshow(needTimerInt));
         end
      end
      else begin
         // now check for MMIO req from core
         function Bool hasReq(Integer i) = cores[i].cRq.notEmpty;
         Vector#(CoreNum, Integer) idxVec = genVector;
         if(find(hasReq, idxVec) matches tagged Valid .i) begin
	    cores[i].cRq.deq;
	    MMIOCRq req = cores[i].cRq.first;
	    // record req
	    reqCore    <= fromInteger(i);
	    reqFunc    <= req.func;
	    reqAmofunc <= case (req.func) matches
			     tagged Amo .f : f;
			     default: None;
			  endcase;
	    reqBE      <= req.byteEn;
	    reqData    <= req.data;
	    reqSz      <= sz_D;    // TODO: may be sz_H, sz_B or sz_W
	    // set up bookkeepings in case of inst fetch (other
	    // bookkeepings are set at processing time)
	    instSel <= truncate(req.addr >> valueof(LgInstSzBytes));
	    fetchingWay <= 0;
	    // find out which MMIO reg/device is being requested
	    DataAlignedAddr addr = getDataAlignedAddr(req.addr);
	    MMIOPlatformReq newReq = Invalid;

	    if(addr >= msipBaseAddr && addr < msipBoundAddr) begin
	       newReq = MSIP (truncate(addr - msipBaseAddr));
	    end
            else if(addr >= mtimecmpBaseAddr &&
		    addr < mtimecmpBoundAddr)
	       begin
		  newReq = MTimeCmp (truncate(addr - mtimecmpBaseAddr));
	       end
            else if(addr == mtimeBaseAddr) begin
	       // assume mtime is of size Data
	       newReq = MTime;
	    end
            else if(addr == toHostAddr) begin
	       // assume tohost is of size Data
	       newReq = ToHost;
	    end
            else if(addr == fromHostAddr) begin
	       // assume fromhost is of size Data
	       newReq = FromHost;
	    end
            else begin    // Send all remaining reqs to the fabric adapter, as is
	       newReq = MMIO_Fabric_Adapter (req.addr);
	    end
            curReq <= newReq;

	    // process valid req
	    state <= ProcessReq;
	    if(verbosity > 0) begin
	       $display("[Platform - SelectReq] core %d, req ", i, fshow(req));
	       $display("    req type ", fshow(newReq));
	    end
         end
      end
   endrule

   // handle new timer interrupt: wait for writes on MTIP to be done
   rule waitTimerInterruptDone(state == WaitResp && curReq == TimerInterrupt);
      for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
         if(waitMTIPCRs[i]) begin
	    cores[i].cRs.deq;
         end
      end
      state <= SelectReq;
      if(verbosity > 0) begin
         $display("[Platform - Done] timer interrupt",
		  ", mtip ", fshow(readVReg(mtip)),
		  ", waitCRs ", fshow(waitMTIPCRs));
      end
   endrule

   // Classify the request
   Bool isInstFetch = (reqFunc matches tagged Inst .x       ? True : False);
   Bool isAmo       = (reqFunc matches tagged Amo  .amofunc ? True : False);
   Bool isLd        = (reqFunc matches tagged Ld            ? True : False);
   Bool isSt        = (reqFunc matches tagged St            ? True : False);

   // handle MSIP access
   rule processMSIP(
      curReq matches tagged MSIP .offset &&& state == ProcessReq
      );
      // core corresponding to lower bits of requested Data
      CoreId lower_core = truncate({offset, 1'b0});
      Bool lower_en = reqBE[0];
      // core corresponding to upper bits of requested Data. Need to check if
      // this core truly exists
      CoreId upper_core = truncate({offset, 1'b1});
      Bool upper_valid = {offset, 1'b1} <= fromInteger(valueof(CoreNum) - 1);
      Bool upper_en = reqBE[4];

      if(isInstFetch) begin
         state <= SelectReq;
         cores[reqCore].pRs.enq(InstFetch (replicate(Invalid)));
         if(verbosity > 0) begin
	    $display("[Platform - process msip] cannot do inst fetch");
         end
      end
      else if(upper_en && !upper_valid) begin
         // access invalid core's MSIP, fault
         state <= SelectReq;
         cores[reqCore].pRs.enq(DataAccess (MMIODataPRs {
	    valid: False, data: ?
	    }));
         if(verbosity > 0) begin
	    $display("[Platform - process msip] access invalid core");
         end
      end
      else if(reqFunc matches tagged Amo .amoFunc) begin
         // AMO req: should only access MSIP of one core. Thus, we always
         // treat the accessed core as the lower core to save the shift (AMO
         // resp is different from load that valid data is already shifted
         // to LSBs). Besides, we only use the lower 32 bits of reqData.
         if(lower_en && upper_en) begin
	    state <= SelectReq;
	    cores[reqCore].pRs.enq(DataAccess (MMIODataPRs {
	       valid: False, data: ?
	       }));
	    if(verbosity > 0) begin
	       $display("[Platform - process msip] ",
			"AMO cannot access 2 cores");
	    end
         end
         else if(lower_en) begin
	    cores[lower_core].pRq.enq(MMIOPRq {
	       target: MSIP,
	       func: reqFunc,
	       data: truncate(reqData)
	       });
	    waitLowerMSIPCRs <= Valid (lower_core);
	    waitUpperMSIPCRs <= Invalid;
	    state <= WaitResp;
         end
         else if(upper_en) begin
	    cores[upper_core].pRq.enq(MMIOPRq {
	       target: MSIP,
	       func: reqFunc,
	       data: truncate(reqData)
	       });
	    waitLowerMSIPCRs <= Valid (upper_core);
	    waitUpperMSIPCRs <= Invalid;
	    state <= WaitResp;
         end
         else begin
	    // AMO access nothing: fault
	    state <= SelectReq;
	    cores[reqCore].pRs.enq(DataAccess (MMIODataPRs {
							    valid: False, data: ?
							    }));
	    if(verbosity > 0) begin
               $display("[Platform - process msip] access nothing");
	    end
         end
      end
      else begin
         // normal load and store
         if(lower_en) begin
	    cores[lower_core].pRq.enq(MMIOPRq {
	       target: MSIP,
	       func: reqFunc,
	       data: zeroExtend(reqData[0])
		});
         end
         if(upper_en) begin
	    cores[upper_core].pRq.enq(MMIOPRq {
	       target: MSIP,
	       func: reqFunc,
	       data: zeroExtend(reqData[32])
	       });
         end
         state <= WaitResp;
         waitLowerMSIPCRs <= lower_en ? Valid (lower_core) : Invalid;
         waitUpperMSIPCRs <= upper_en ? Valid (upper_core) : Invalid;
      end
   endrule

   rule waitMSIPDone(
		     curReq matches tagged MSIP .offset &&& state == WaitResp
		     );
      Bit#(32) lower_data = 0;
      Bit#(32) upper_data = 0;
      for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
         if (waitLowerMSIPCRs matches tagged Valid .c &&&
	     c == fromInteger(i)) begin
				     cores[i].cRs.deq;
				     lower_data = zeroExtend(cores[i].cRs.first.data);
				  end
	 else if(waitUpperMSIPCRs matches tagged Valid .c &&&
		 c == fromInteger(i)) begin
					 cores[i].cRs.deq;
					 upper_data = zeroExtend(cores[i].cRs.first.data);
				      end
      end
      state <= SelectReq;
      cores[reqCore].pRs.enq(DataAccess (MMIODataPRs {
						      valid: True,
	 // for AMO, resp data should be signExtend(lower_data). However,
	 // lower_data is just 1 or 0, and upper_data is always 0, so we
	 // don't need to do signExtend.
	 data: {upper_data, lower_data}
	}));
      if(verbosity > 0) begin
         $display("[Platform - msip done] lower %x, upper %x",
		  lower_data, upper_data);
      end
   endrule

   function Data getWriteData(Data orig);
      if(reqFunc matches tagged Amo .amoFunc) begin
         // amo
         Bool doubleWord = reqBE[4] && reqBE[0];
         Bool upper32 = reqBE[4] && !reqBE[0];
         let amoInst = AmoInst {
	    func: amoFunc,
	    doubleWord: doubleWord,
	    aq: False,
	    rl: False
	    };
         return amoExec(amoInst, orig, reqData, upper32);
      end
      else begin
         // normal store
         Vector#(NumBytes, Bit#(8)) data = unpack(orig);
         Vector#(NumBytes, Bit#(8)) wrVec = unpack(reqData);
         for(Integer i = 0; i < valueof(NumBytes); i = i+1) begin
	    if(reqBE[i]) begin
	       data[i] = wrVec[i];
	    end
         end
         return pack(data);
      end
   endfunction

   function Data getAmoResp(Data orig);
      if(reqBE[4] && reqBE[0]) begin
         // double word
         return orig;
      end
      else if(reqBE[4]) begin
         // upper 32 bit
         return signExtend(orig[63:32]);
      end
      else begin
         // lower 32 bit
         return signExtend(orig[31:0]);
      end
   endfunction

   // handle mtimecmp access
   rule processMTimeCmp(
			curReq matches tagged MTimeCmp .offset &&& state == ProcessReq
      );
      if(isInstFetch) begin
         state <= SelectReq;
         cores[reqCore].pRs.enq(InstFetch (replicate(Invalid)));
         if(verbosity > 0) begin
	    $display("[Platform - process mtimecmp] cannot do inst fetch");
         end
      end
      else if(offset > fromInteger(valueof(CoreNum) - 1)) begin
         // access invalid core's mtimecmp, fault
         cores[reqCore].pRs.enq(DataAccess (MMIODataPRs {
	    valid: False, data: ?
	    }));
         state <= SelectReq;
         if(verbosity > 0) begin
	    $display("[Platform - process mtimecmp] access fault");
         end
      end
      else begin
         let oldMTimeCmp = mtimecmp[offset];
         if(reqFunc == Ld) begin
	    cores[reqCore].pRs.enq(DataAccess (MMIODataPRs {
	       valid: True,
	       data: oldMTimeCmp
		}));
	    state <= SelectReq;
	    if(verbosity > 0) begin
	       $display("[Platform - process mtimecmp] read done, data %x",
			oldMTimeCmp);
	    end
         end
         else begin
	    // do updates for store or AMO
	    let newData = getWriteData(oldMTimeCmp);
	    mtimecmp[offset] <= newData;
	    // get and record amo resp
	    let respData = getAmoResp(oldMTimeCmp);
	    amoResp <= respData;
	    // check changes to MTIP
	    if(newData <= mtime && !mtip[offset]) begin
	       // need to post new timer interrupt
	       mtip[offset] <= True;
	       cores[offset].pRq.enq(MMIOPRq {
		  target: MTIP,
		  func: St,
                  data: 1
		  });
	       state <= WaitResp;
	    end
            else if(newData > mtime && mtip[offset]) begin
	       // need to clear timer interrupt
	       mtip[offset] <= False;
	       cores[offset].pRq.enq(MMIOPRq {
		  target: MTIP,
		  func: St,
                  data: 0
		  });
	       state <= WaitResp;
	    end
            else begin
	       // nothing happens to mtip, just finish this req
	       cores[reqCore].pRs.enq(DataAccess (MMIODataPRs {
		  valid: True,
		  // store doesn't need resp data, just fill in AMO resp
		  data: respData
                  }));
	       state <= SelectReq;
	       if(verbosity > 0) begin
		  $display("[Platform - process mtimecmp] ",
			   "no change to mtip ", fshow(readVReg(mtip)),
			   ", mtime %x", mtime,
                           ", old mtimecmp ", fshow(readVReg(mtimecmp)),
			   ", new mtimecmp[%d] %x", offset, newData);
	       end
            end
         end
      end
   endrule

   rule waitMTimeCmpDone(
      curReq matches tagged MTimeCmp .offset &&& state == WaitResp
      );
      cores[offset].cRs.deq;
      cores[reqCore].pRs.enq(DataAccess (MMIODataPRs {
	 valid: True,
	 // store doesn't need resp data, just fill in AMO resp. We cannot
	 // recompute AMO resp now, because mtimecmp has changed
	 data: amoResp
	}));
      state <= SelectReq;
      if(verbosity > 0) begin
         $display("[Platform - mtimecmp done]",
		  ", mtime %x", mtime,
		  ", mtimecmp ", fshow(readVReg(mtimecmp)),
		  ", mtip ", fshow(readVReg(mtip)));
      end
   endrule

   // handle mtime access
   rule processMTime(state == ProcessReq && curReq == MTime);
        if(isInstFetch) begin
            state <= SelectReq;
            cores[reqCore].pRs.enq(InstFetch (replicate(Invalid)));
            if(verbosity > 0) begin
                $display("[Platform - process mtime] cannot do inst fetch");
            end
        end
        else if(reqFunc == Ld) begin
            cores[reqCore].pRs.enq(DataAccess (MMIODataPRs {
                valid: True, data: mtime
            }));
            state <= SelectReq;
            if(verbosity > 0) begin
                $display("[Platform - process mtime] read done, data %x",
                         mtime);
            end
        end
        else begin
            // do update for store or AMO
            let newData = getWriteData(mtime);
            mtime <= newData;
            // get and record AMO resp
            let respData = getAmoResp(mtime);
            amoResp <= respData;
            // check change in MTIP
            Vector#(CoreNum, Bool) changeMTIP = replicate(False);
            for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
                if(mtimecmp[i] <= newData && !mtip[i]) begin
                    cores[i].pRq.enq(MMIOPRq {
                        target: MTIP,
                        func: St,
                        data: 1
                    });
                    changeMTIP[i] = True;
                end
                else if(mtimecmp[i] > newData && mtip[i]) begin
                    cores[i].pRq.enq(MMIOPRq {
                        target: MTIP,
                        func: St,
                        data: 0
                    });
                    changeMTIP[i] = True;
                end
            end
            if(changeMTIP != replicate(False)) begin
                waitMTIPCRs <= changeMTIP;
                state <= WaitResp;
            end
            else begin
                cores[reqCore].pRs.enq(DataAccess (MMIODataPRs {
                    valid: True,
                    data: respData // AMO resp
                }));
                state <= SelectReq;
                if(verbosity > 0) begin
                    $display("[Platform - process mtime] ",
                             "no change to mtip ", fshow(readVReg(mtip)),
                             ", new mtime %x", newData,
                             ", mtimecmp ", fshow(readVReg(mtimecmp)));
                end
            end
        end
   endrule

    rule waitMTimeDone(state == WaitResp && curReq == MTime);
        for(Integer i = 0; i < valueof(CoreNum); i = i+1) begin
            if(waitMTIPCRs[i]) begin
                cores[i].cRs.deq;
            end
        end
        cores[reqCore].pRs.enq(DataAccess (MMIODataPRs {
            valid: True,
            data: amoResp // recorded amo resp
        }));
        state <= SelectReq;
        if(verbosity > 0) begin
            $display("[Platform - mtime done]",
                     ", mtime %x", mtime,
                     ", mtimecmp ", fshow(readVReg(mtimecmp)),
                     ", mtip ", fshow(readVReg(mtip)));
        end
    endrule

    // handle tohost access
    rule processToHost(state == ProcessReq && curReq == ToHost);
        if(isInstFetch) begin
            state <= SelectReq;
            cores[reqCore].pRs.enq(InstFetch (replicate(Invalid)));
            if(verbosity > 0) begin
                $display("[Platform - process tohost] cannot do inst fetch");
            end
        end
        else begin
            let resp = MMIODataPRs {valid: False, data: ?};
            if(reqFunc == St) begin
                if(toHostQ.notEmpty) begin
                    doAssert(False,
                             "Cannot write tohost when toHostQ not empty");
                    // this will raise access fault
                end
                else begin
                    let data = getWriteData(0);
                    if(data != 0) begin // 0 means nothing for tohost
                        toHostQ.enq(data);
                    end
                    resp.valid = True;
                end
            end
            else if(reqFunc == Ld) begin
                resp.valid = True;
                if(toHostQ.notEmpty) begin
                    resp.data = toHostQ.first;
                end
                else begin
                    resp.data = 0;
                end
            end
            else begin
                // amo: access fault
                doAssert(False, "Cannot do AMO on toHost");
            end
            state <= SelectReq;
            cores[reqCore].pRs.enq(DataAccess (resp));
            if(verbosity > 0) begin
                $display("[Platform - process tohost] resp ", fshow(resp));
            end
        end
    endrule

    // handle fromhost access
    rule processFromHost(state == ProcessReq && curReq == FromHost);
        if(isInstFetch) begin
            state <= SelectReq;
            cores[reqCore].pRs.enq(InstFetch (replicate(Invalid)));
            if(verbosity > 0) begin
                $display("[Platform - process fromhost] cannot do inst fetch");
            end
        end
        else begin
            let resp = MMIODataPRs {valid: False, data: ?};
            if(reqFunc == St) begin
                if(fromHostQ.notEmpty) begin
                    if(getWriteData(fromHostQ.first) == 0) begin
                        fromHostQ.deq;
                        resp.valid = True;
                    end
                    else begin
                        doAssert(False, "Can only write 0 to fromhost");
                    end
                end
                else begin
                    if(getWriteData(0) == 0) begin
                        resp.valid = True;
                    end
                    else begin
                        doAssert(False, "Can only write 0 to fromhost");
                    end
                end
            end
            else if(reqFunc == Ld) begin
                resp.valid = True;
                if(fromHostQ.notEmpty) begin
                    resp.data = fromHostQ.first;
                end
                else begin
                    resp.data = 0;
                end
            end
            else begin
                // amo: access fault
                doAssert(False, "Cannot do AMO on fromHost");
            end
            state <= SelectReq;
            cores[reqCore].pRs.enq(DataAccess (resp));
            if(verbosity > 0) begin
                $display("[Platform - process fromhost] resp ", fshow(resp));
            end
        end
    endrule

   // ================================================================
   // ================================================================
   // ================================================================
   // All remaining IO (not MTIMECMP, MSIP, fromHost, toHost) goes to the fabric
   // Instruction-fetches are treated specially (collect a superscalar set of instrs)

   // ================================================================
   // MMIO to Fabric: Load/Store (not Instruction Fetch)

   // Forward the request as-is to the fabric adapter.
   rule rl_mmio_to_fabric_req (curReq matches tagged MMIO_Fabric_Adapter .addr
			       &&& (state == ProcessReq)
			       &&& (isLd || isSt));
      let req = MMIOCRq {addr:addr, func:reqFunc, byteEn:reqBE, data:reqData};
      mmio_fabric_adapter_core_side.request.put (req);
      state <= WaitResp;

      if (verbosity > 0) begin
	 $display ("MMIOPlatform.rl_mmio_to_fabric_req");
	 $display ("    ", fshow (req));
      end
   endrule

   // Forward the fabric-adapter's response as-is to the core.
   rule rl_mmio_from_fabric_rsp (curReq matches tagged MMIO_Fabric_Adapter .addr
				 &&& (state == WaitResp)
				 &&& (isLd || isSt));
      MMIODataPRs dprs <- mmio_fabric_adapter_core_side.response.get;
      let prs = tagged DataAccess dprs;
      cores[reqCore].pRs.enq (prs);
      state <= SelectReq;

      if (verbosity > 0) begin
	 $display ("MMIOPlatform.rl_mmio_from_fabric_rsp");
	 $display ("    ", fshow (prs));
      end
   endrule

   // ================================================================
   // MMIO to Fabric: AMO (not Instruction Fetch)

   rule rl_mmio_to_fabric_amo_req (curReq matches tagged MMIO_Fabric_Adapter .addr
				   &&& (state == ProcessReq)
				   &&& isAmo);
      // Send a load-request to the fabric adapter.
      // Align addr to 8-byte boundary (FabricData-aligned)
      Addr addr1 = { addr [63:3], 3'b_000 };
      let req = MMIOCRq {addr:addr, func:tagged Ld, byteEn:?, data:?};
      mmio_fabric_adapter_core_side.request.put (req);
      state <= WaitResp;
      amoWaitWriteResp <= False;

      if (verbosity > 0) begin
	 $display ("MMIOPlatform.rl_mmio_to_fabric_amo_req: addr 0x%0h", addr);
	 $display ("    ", fshow (req));
      end
   endrule

   // Get the Load-response; do the AMO op; send final write back to fabric, and respond to core
   rule rl_mmio_from_fabric_amo_rsp (curReq matches tagged MMIO_Fabric_Adapter .addr
				     &&& (state == WaitResp)
				     &&& isAmo);
      MMIODataPRs dprs <- mmio_fabric_adapter_core_side.response.get;

      if (amoWaitWriteResp) begin
	 // Discard the write response; we're now ready for another request
	 state <= SelectReq;
      end
      else if (! dprs.valid) begin
	 // Access fault
	 let prs = tagged DataAccess dprs;
	 cores[reqCore].pRs.enq (prs);
	 state <= SelectReq;
      end
      else begin
	 // Do the AMO op on the loaded value and the store value
	 let ld_val = dprs.data;
	 let new_st_val = fn_amo_op (reqSz, reqAmofunc, addr, ld_val, reqData);

	 // Write back new st_val to fabric
	 let req = MMIOCRq {addr:addr, func:tagged St, byteEn:reqBE, data:new_st_val};
	 mmio_fabric_adapter_core_side.request.put (req);

	 let prs = tagged DataAccess (MMIODataPRs { valid: True, data: ld_val });
	 cores[reqCore].pRs.enq (prs);
	 // Stay in WaitResp but wait to discard the write response
	 amoWaitWriteResp <= True;

	 if (verbosity > 1) begin
	    $display ("MMIO_Platform.rl_mmio_from_fabric_amo_rsp: addr 0x%0h, size %0d, amofunc %0d",
		      addr, reqSz, reqAmofunc);
	    $display ("    ld_val 0x%0h  op  st_val 0x%0h => new_st_val 0x%0h", ld_val, reqData, new_st_val);
	 end
      end
   endrule

   // ================================================================
   // MMIO to Fabric: Instruction Fetch
   // (This code adapted from MMIOPlatform::processBootRomInst and waitBootRomInst)
   // Loops, collecting and returning a super-scalar-wide set of instructions (0..maxWay).
   //     Note: may repeatedly fetch the same Data word as it collects instuctions.
   //     Expected to be used only for initial boot ROM, so speed is not critical.
   //     TODO: Candidate for future optimization.
   // The original request had func = Inst maxWay
   // instSel: initial instruction index in a Data word: truncate(req.addr >> valueof(LgInstSzBytes))
   // fetchingWay: initial 0

   rule rl_mmio_to_fabric_ifetch_req (curReq matches tagged MMIO_Fabric_Adapter .addr
				      &&& (state == ProcessReq)
				      &&& isInstFetch);
      // Note: addr may not be FabricData-aligned; result will be Data that contains addr
      // TODO: currently assumes superscalarity fits in fabric width
      Addr addr1 = { addr [63:3], 3'b_000 };
      let req = MMIOCRq {addr:addr1, func: tagged Ld, byteEn: ?, data: ? };
      mmio_fabric_adapter_core_side.request.put (req);
      state <= WaitResp;

      if (verbosity > 0) begin
	 $display ("MMIOPlatform.rl_mmio_to_fabric_ifetch_req: addr 0x%0h  fetchingWay %0d",
		   addr, fetchingWay);
	 $display ("    ", fshow (req));
      end
   endrule

   rule rl_mmio_from_fabric_ifetch_rsp (curReq matches tagged MMIO_Fabric_Adapter .addr
					&&& (state == WaitResp)
					&&& isInstFetch);
      MMIODataPRs dprs <- mmio_fabric_adapter_core_side.response.get;
      if (! dprs.valid) begin
         // Access fault
         Vector #(SupSizeX2, Maybe #(Instruction16)) resp = replicate (Invalid);
         for(Integer i = 0; i < valueof (SupSizeX2); i = i+1) begin
            if (fromInteger (i) < fetchingWay)
               resp [i] = Valid (fetchedInsts [i]);
            else if (fromInteger (i) == fetchingWay)
	       resp [i] = tagged Invalid;
         end
         cores[reqCore].pRs.enq (tagged InstFetch resp);
         state <= SelectReq;

	 if (verbosity > 0) begin
	    $display ("MMIOPlatform.rl_mmio_from_fabric_ifetch_rsp: access fault; final resp to core:");
	    $display ("    ", fshow (resp));
	 end
      end

      else begin
	 // No access fault
	 let data = dprs.data;

         SupWayX2Sel maxWay = 0;
         if(reqFunc matches tagged Inst .w) begin
            maxWay = w;
         end

         // View Data as a vector of instructions
         Vector#(MemDataSzInst, Instruction16) instVec = unpack(pack(dprs.data));
         // extract inst from resp data
         Instruction16 inst = instVec[instSel];
         // check whether we are done or not
         if (fetchingWay >= maxWay) begin
            // all 0..maxWay insts are fetched; we can resp now
            Vector#(SupSizeX2, Maybe#(Instruction16)) resp = replicate(Invalid);
            for(Integer i = 0; i < valueof(SupSizeX2); i = i+1) begin
               if(fromInteger(i) < fetchingWay)
                  resp[i] = Valid (fetchedInsts[i]);
               else if(fromInteger(i) == fetchingWay)
                  resp[i] = Valid (inst);
            end
            cores[reqCore].pRs.enq (tagged InstFetch resp);
            state <= SelectReq;

	    if (verbosity > 0) begin
	       $display ("MMIOPlatform.rl_mmio_from_fabric_ifetch_rsp: final resp to core:");
	       $display ("    ", fshow (resp));
	    end
         end
         else begin
            // continue to fetch next inst, save current inst, increment offset
            fetchedInsts[fetchingWay] <= inst;
            fetchingWay <= fetchingWay + 1;
            instSel <= instSel + 1;
            curReq <= MMIO_Fabric_Adapter (addr + 2);
            state <= ProcessReq;

	    if (verbosity > 0) begin
	       $display ("MMIOPlatform.rl_mmio_from_fabric_ifetch_rsp:");
	       $display ("     fetchingWay %0d instSel %0d inst 0x%0h", fetchingWay, instSel, inst);
	    end
         end
      end
   endrule

   // ================================================================
   // ================================================================
   // ================================================================
   // INTERFACE

    method Action start(Addr toHost, Addr fromHost) if(state == Init);
        toHostAddr <= getDataAlignedAddr(toHost);
        fromHostAddr <= getDataAlignedAddr(fromHost);
        state <= SelectReq;
    endmethod

    method ActionValue#(Data) to_host;
        toHostQ.deq;
        return toHostQ.first;
    endmethod

    method Action from_host(Data x);
        fromHostQ.enq(x);
    endmethod
endmodule
