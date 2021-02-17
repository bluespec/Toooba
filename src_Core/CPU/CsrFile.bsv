
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
import Types::*;
import ProcTypes::*;
import DefaultValue::*;
import ConcatReg::*;
import ConfigReg::*;
import Ehr::*;
import Fifos::*;
import Vector::*;
import FIFO::*;
import GetPut::*;
import BuildVector::*;
//import TRNG::*;

// ================================================================
// BSV additional libs

import Cur_Cycle  :: *;

`ifdef PERFORMANCE_MONITORING
import PerformanceMonitor :: *;
`endif

// ================================================================
// Project imports from Toooba

import SoC_Map :: *;

// ================================================================
// Information returned on traps and mret/sret/uret

typedef struct {
   Addr      new_pc;

`ifdef INCLUDE_TANDEM_VERIF
   // The fields below are for tandem verification only
   Bit #(2)  prv;
   Data      status;
   Data      cause;
   Data      epc;
   Data      tval;
`endif
   } Trap_Updates
deriving (Bits, FShow);

typedef struct {
   Addr      new_pc;

`ifdef INCLUDE_TANDEM_VERIF
   // The fields below are for tandem verification only
   Bit #(2)  prv;
   Data      status;
`endif
   } RET_Updates
deriving (Bits, FShow);

// ================================================================

interface CsrFile;
    // Read
    method Data rd(CSR csr);
    // normal write by CSRXXX inst to any CSR
    method Action csrInstWr(CSR csr, Data x);
    // normal write by FPU inst to FPU CSR
    method Bool fpuInstNeedWr(Bit#(5) fflags, Bool fpu_dirty);
    method Action fpuInstWr(Bit#(5) fflags); // FPU must become dirty
`ifdef INCLUDE_TANDEM_VERIF
    // Returns new fcsr and mstatus (pure function)
    method Tuple2 #(Bit #(5), Data) fpuInst_csr_updates (Bit #(5)  fflags,
							 Bool      init_for_way0,
							 Bit #(5)  old_fflags,
							 Data      old_mstatus);
    method Data getMIP;
`endif

    // The WARL transform performed during CSRRx writes to a CSR
    method Data warl_xform (CSR csr, Data x);

    // Methods for handling traps
    method Maybe#(Interrupt) pending_interrupt;
    method ActionValue#(Trap_Updates) trap(Trap t, Addr pc, Addr faultAddr, Bit #(32) orig_inst);
    method ActionValue#(RET_Updates) sret;
    method ActionValue#(RET_Updates) mret;

    // Outputs for CSRs that the rest of the processor needs to know about
    method VMInfo vmI;
    method VMInfo vmD;
    method CsrDecodeInfo decodeInfo;

    // Updating minstret CSR outside of normal CSR write instructions. This
    // increment will see the effect of normal CSR write.
    method Action incInstret(SupCnt x);

    // update copy of mtime
    method Action setTime(Data t);

    // MSIP/MTIP bits for external world (e.g., for MMIO and timer interrupt).
    // XXX These methods should only be called when the processor backend
    // pipeline does not contain any CSRXXX inst or corresponding interrupt
    // inst (the inst which is turned into an interrupt). This ensures that
    // CSRXXX and interrupt are handled atomically.  MSIP/MTIP should not
    // affect other insts (e.g., address translation of loads/stores),
    // synchronous exceptions and other types of interrupts.
    method Bit#(1) getMSIP;
    method Action setMSIP(Bit#(1) v);
    method Action setMTIP(Bit#(1) v);

   // Bluespec: external interrupts targeting machine and supervisor modes
    method Action setMEIP (Bit #(1) v);
    method Action setSEIP (Bit #(1) v);

    // performance stats is collected or not
    method Bool doPerfStats;
    // send/recv updates on stats CSR globally
    method ActionValue#(Bool) sendDoStats;
    method Action recvDoStats(Bool s);

    // terminate
    method ActionValue#(void) terminate;

`ifdef INCLUDE_GDB_CONTROL
   // Read dpc
   method Addr dpc_read ();

   // Update dpc
   method Action dpc_write (Addr pc);

   // Check whether to enter Debug Mode based on dcsr.{ebreakm, ebreaks, ebreaku}
   method Bit #(1) dcsr_break_bit;

   // Read dcsr[2], the step bit
   method Bit #(1) dcsr_step_bit;

   // Update 'cause' in DCSR
   // Is invoked by logic that stops a hart, to enter Debug Mode
   (* always_ready *)
   method Action dcsr_cause_write (Bit #(3)  dcsr_cause);

`endif
`ifdef PERFORMANCE_MONITORING
   (* always_ready, always_enabled *)
   method Action send_performance_events (Vector #(No_Of_Evts, Bit #(Report_Width)) evts);
`endif
endinterface

// Fancy Reg functions
function Reg#(Bit#(n)) truncateReg(Reg#(Bit#(m)) r) provisos (Add#(a__,n,m));
    return (interface Reg;
        method Bit#(n) _read = truncate(r._read);
        method Action _write(Bit#(n) x) = r._write({truncateLSB(r._read), x});
    endinterface);
endfunction

function Reg#(Bit#(n)) truncateRegLSB(Reg#(Bit#(m)) r) provisos (Add#(a__,n,m));
    return (interface Reg;
        method Bit#(n) _read = truncateLSB(r._read);
        method Action _write(Bit#(n) x) = r._write({x, truncate(r._read)});
    endinterface);
endfunction

function Reg#(Bit#(n)) zeroExtendReg(Reg#(Bit#(m)) r) provisos (Add#(a__,m,n));
    return (interface Reg;
        method Bit#(n) _read = zeroExtend(r._read);
        method Action _write(Bit#(n) x) = r._write(truncate(x));
    endinterface);
endfunction

function Reg#(t) readOnlyReg(t r);
    return (interface Reg;
        method t _read = r;
        method Action _write(t x) = noAction;
    endinterface);
endfunction
// module version of readOnlyReg for convenience
module mkReadOnlyReg#(t x)(Reg#(t));
    return readOnlyReg(x);
endmodule

function Reg#(t) regFromReadOnly(ReadOnly#(t) r);
    return (interface Reg;
        method t _read = r._read;
        method Action _write(t x);
            noAction;
        endmethod
    endinterface);
endfunction

function Reg#(t) addWriteSideEffect(Reg#(t) r, Action a);
    return (interface Reg;
        method t _read = r._read;
        method Action _write(t x);
            r._write(x);
            a;
        endmethod
    endinterface);
endfunction

`ifdef PERFORMANCE_MONITORING
interface PerfCountersVec;
    interface Vector#(No_Of_Ctrs, Reg#(Data)) counter_vec;
    interface Vector#(No_Of_Ctrs, Reg#(Data)) event_vec;
    interface Reg#(Data) inhibit;
    method Action send_performance_events (Vector #(No_Of_Evts, Bit#(Report_Width)) evts);
endinterface
(* synthesize *)
module mkPerfCountersToooba (PerfCountersVec);
    PerfCounters_IFC #(No_Of_Ctrs, Counter_Width, Report_Width, No_Of_Evts) perf_counters <- mkPerfCounters;
    Vector#(No_Of_Ctrs, Reg#(Data)) counters = ?;
    for (Bit#(TLog#(No_Of_Ctrs)) i = 0; i < 29; i = i + 1) counters[i] =
        interface Reg;
            method Action _write(Data x) = perf_counters.write_counter(i,x);
            method Data _read = perf_counters.read_counters[i];
        endinterface;
    Vector#(No_Of_Ctrs, Reg#(Data)) events = ?;
    for (Bit#(TLog#(No_Of_Ctrs)) i = 0; i < 29; i = i + 1) events[i] =
        interface Reg;
            method Action _write(Data x) = perf_counters.write_ctr_sel(i,truncate(x));
            method Data _read = zeroExtend(perf_counters.read_ctr_sels[i]);
        endinterface;
    interface counter_vec = counters;
    interface event_vec = events;
    interface inhibit = interface Reg;
        method Action _write(Data x) = perf_counters.write_ctr_inhibit(truncate(x));
        method Data _read = zeroExtend(perf_counters.read_ctr_inhibit);
    endinterface;
    method send_performance_events = perf_counters.send_performance_events;
endmodule
`endif

function Bool has_csr_permission(CSR csr, Bit#(2) prv, Bool write);
    Bit#(12) csr_index = pack(csr);
    return ((prv >= csr_index[9:8]) && (!write || (csr_index[11:10] != 2'b11)));
endfunction

// non-standard terminate CSR
interface Terminate;
    interface Reg#(Data) reg_ifc;
    method ActionValue#(void) terminate;
endinterface

module mkTerminate(Terminate);
    FIFO#(void) terminateQ <- mkFIFO1;

    interface Reg reg_ifc;
        method Action _write(Data x);
            terminateQ.enq(?);
            $display(
                "[Terminate CSR] being written (val = %x), ",
                "send terminate signal to host", x
            );
        endmethod
        method Data _read = 0;
    endinterface

    method terminate = toGet(terminateQ).get;
endmodule

// stats CSR: there is only one copy in the whole multiprocessor, so any write
// to stats CSR will be broadcasted
interface StatsCsr;
    interface Reg#(Data) reg_ifc;
    method Bool doPerfStats;
    // send/recv updates on stats CSR globally
    method ActionValue#(Bool) sendDoStats;
    method Action recvDoStats(Bool s);
endinterface

module mkStatsCsr(StatsCsr);
    Reg#(Bool) doStats <- mkConfigReg(False);

    FIFO#(Bool) writeQ <- mkFIFO1;

    interface Reg reg_ifc;
        method Data _read = zeroExtend(pack(doStats));
        method Action _write(Data x);
            writeQ.enq(unpack(truncate(x)));
        endmethod
    endinterface

    method Bool doPerfStats = doStats;

    method ActionValue#(Bool) sendDoStats;
        writeQ.deq;
        return writeQ.first;
    endmethod

    method Action recvDoStats(Bool s);
        doStats <= s;
    endmethod
endmodule

// same as EHR except that read port 0 is not ordered with other methods. Read
// port 1 will still get bypassing from write port 0.
module mkConfigEhr#(t init)(Ehr#(n, t)) provisos(Bits#(t, w));
    Ehr#(n, t) data <- mkEhr(init);
    Wire#(t) read <- mkBypassWire;

    (* fire_when_enabled, no_implicit_conditions *)
    rule setRead;
        read <= data[0];
    endrule

    Ehr#(n, t) ifc = ?;
    ifc[0] = (interface Reg;
        method _read = read._read;
        method _write = data[0]._write;
    endinterface);
    for(Integer i = 1; i < valueOf(n); i = i+1) begin
        ifc[i] = (interface Reg;
            method _read = data[i]._read;
            method _write = data[i]._write;
        endinterface);
    end
    return ifc;
endmodule

module mkCsrFile #(Data hartid)(CsrFile);
    RiscVISASubset isa = defaultValue;

    // To save from bypassing logic, CSR reads will get stale value
    let mkCsrReg = mkConfigReg;
    let mkCsrEhr = mkConfigEhr;

    // current prv level (this is not a csr...)
    Reg#(Bit#(2)) prv_reg <- mkCsrReg(prvM);
    
    // Machine level CSRs
    // mstatus
    Reg#(Bit#(2)) xs_reg   <- mkReadOnlyReg(0); // XXX no extension
    Reg#(Bit#(2)) fs_reg   <- (isa.f || isa.d) ? mkCsrReg(2'b01) : mkReadOnlyReg(0);
    Reg#(Bit#(1)) sd_reg   =  readOnlyReg(
        ((xs_reg == 2'b11) || (fs_reg == 2'b11)) ? 1 : 0
    );
    function Bit #(1) fn_sd_val (Bit #(2) xs_val, Bit #(2) fs_val);
       return (((xs_val == 2'b11) || (fs_val == 2'b11)) ? 1 : 0);
    endfunction
    Reg#(Bit#(2)) sxl_reg  =  readOnlyReg(getXLBits);
    Reg#(Bit#(2)) uxl_reg  =  readOnlyReg(getXLBits);
    Reg#(Bit#(1)) tsr_reg  <- mkCsrReg(0);
    Reg#(Bit#(1)) tw_reg   <- mkCsrReg(0);
    Reg#(Bit#(1)) tvm_reg  <- mkCsrReg(0);
    Reg#(Bit#(1)) mxr_reg  <- mkCsrReg(0);
    Reg#(Bit#(1)) sum_reg  <- mkCsrReg(0);
    Reg#(Bit#(1)) mprv_reg <- mkCsrReg(0);
    Reg#(Bit#(2)) mpp_reg  <- mkCsrReg(0);
    Reg#(Bit#(1)) spp_reg  <- mkCsrReg(0);
    Vector#(4, Reg#(Bit#(2))) prev_prv_vec = vec(
        // prev_prv_vec[x]: privilege mode before trapping into mode x
        readOnlyReg(prvU), // upp
        concatReg2(readOnlyReg(1'b0), spp_reg), // spp
        readOnlyReg(2'b0), // reserved
        mpp_reg
    );
    Vector#(4, Reg#(Bit#(1))) ie_vec = replicate(
        readOnlyReg(0) // ie_vec[x]: interrupt enable for mode x
    );
    ie_vec[prvU] <- mkCsrReg(0);
    ie_vec[prvS] <- mkCsrReg(0);
    ie_vec[prvM] <- mkCsrReg(0);
    Vector#(4, Reg#(Bit#(1))) prev_ie_vec = replicate(
        readOnlyReg(0) // prev_ie_vec[x]: ie_vec[x] before trapping into mode x
    );
    prev_ie_vec[prvU] <- mkCsrReg(0);
    prev_ie_vec[prvS] <- mkCsrReg(0);
    prev_ie_vec[prvM] <- mkCsrReg(0);
    Reg#(Data) mstatus_csr = concatReg24(
        sd_reg, readOnlyReg(27'b0), sxl_reg, uxl_reg, readOnlyReg(9'b0),
        tsr_reg, tw_reg, tvm_reg, mxr_reg, sum_reg, mprv_reg, xs_reg, fs_reg,
        mpp_reg, readOnlyReg(2'b0), spp_reg,
        prev_ie_vec[prvM], readOnlyReg(1'b0),
        prev_ie_vec[prvS], prev_ie_vec[prvU],
        ie_vec[prvM],      readOnlyReg(1'b0),
        ie_vec[prvS],      ie_vec[prvU]
    );
    function Data fn_mstatus_val (Bit #(2) sxl_val, Bit #(2) uxl_val,
				  Bit #(1) tsr_val, Bit #(1) tw_val,  Bit #(1) tvm_val,
				  Bit #(1) mxr_val, Bit #(1) sum_val, Bit #(1) mprv_val,
				  Bit #(2) xs_val,  Bit #(2) fs_val,
				  Bit #(2) mpp_val, Bit #(1) spp_val,
				  Bit #(1) prev_ie_vec_prvM_val,
				  Bit #(1) prev_ie_vec_prvS_val, Bit #(1) prev_ie_vec_prvU_val,
				  Bit #(1) ie_vec_prvM_val,
				  Bit #(1) ie_vec_prvS_val,      Bit #(1) ie_vec_prvU_val);
       return {fn_sd_val (xs_val, fs_val),
	       27'b0, sxl_val, uxl_val, 9'b0,
	       tsr_val, tw_val, tvm_val, mxr_val, sum_val, mprv_val, xs_val, fs_val,
	       mpp_val, 2'b0, spp_val,
	       prev_ie_vec_prvM_val, 1'b0,
	       prev_ie_vec_prvS_val, prev_ie_vec_prvU_val,
	       ie_vec_prvM_val,      1'b0,
	       ie_vec_prvS_val,      ie_vec_prvU_val};
    endfunction

    // misa
    Reg#(Data) misa_csr = readOnlyReg({getXLBits, 36'b0, getExtensionBits(isa)});
    // medeleg: some exceptions don't exist, fix corresponding bits to 0
    Reg#(Bit#(1)) medeleg_15_reg <- mkCsrReg(0); // cause 15
    Reg#(Bit#(3)) medeleg_13_11_reg <- mkCsrReg(0); // case 13-11
    Reg#(Bit#(10)) medeleg_9_0_reg <- mkCsrReg(0); // cause 9-0
    Reg#(Data) medeleg_csr = concatReg6(
        readOnlyReg(48'b0), medeleg_15_reg,
        readOnlyReg(1'b0), medeleg_13_11_reg,
        readOnlyReg(1'b0), medeleg_9_0_reg
    );
    // mideleg: some interrupts don't exist, fix corresponding bits to 0
    Reg#(Bit#(1)) mideleg_11_reg <- mkCsrReg(0);
    Reg#(Bit#(3)) mideleg_9_7_reg <- mkCsrReg(0);
    Reg#(Bit#(3)) mideleg_5_3_reg <- mkCsrReg(0);
    Reg#(Bit#(2)) mideleg_1_0_reg <- mkCsrReg(0);
    Reg#(Data) mideleg_csr = concatReg8(
        readOnlyReg(52'b0), mideleg_11_reg,
        readOnlyReg(1'b0), mideleg_9_7_reg,
        readOnlyReg(1'b0), mideleg_5_3_reg,
        readOnlyReg(1'b0), mideleg_1_0_reg
    );
    // mie
    Vector#(4, Reg#(Bit#(1))) external_int_en_vec = replicate(readOnlyReg(0));
    external_int_en_vec[prvU] <- mkCsrReg(0);
    external_int_en_vec[prvS] <- mkCsrReg(0);
    external_int_en_vec[prvM] <- mkCsrReg(0);
    Vector#(4, Reg#(Bit#(1))) timer_int_en_vec = replicate(readOnlyReg(0));
    timer_int_en_vec[prvU] <- mkCsrReg(0);
    timer_int_en_vec[prvS] <- mkCsrReg(0);
    timer_int_en_vec[prvM] <- mkCsrReg(0);
    Vector#(4, Reg#(Bit#(1))) software_int_en_vec = replicate(readOnlyReg(0));
    software_int_en_vec[prvU] <- mkCsrReg(0);
    software_int_en_vec[prvS] <- mkCsrReg(0);
    software_int_en_vec[prvM] <- mkCsrReg(0);
    Reg#(Data) mie_csr = concatReg13(
        readOnlyReg(52'b0),
        external_int_en_vec[prvM], readOnlyReg(1'b0),
        external_int_en_vec[prvS], readOnlyReg(1'b0),    // only if misa.N: external_int_en_vec[prvU],
        timer_int_en_vec[prvM],    readOnlyReg(1'b0),
        timer_int_en_vec[prvS],    readOnlyReg(1'b0),    // only if misa.N: timer_int_en_vec[prvU],
        software_int_en_vec[prvM], readOnlyReg(1'b0),
        software_int_en_vec[prvS], readOnlyReg(1'b0)     // only if misa.N: software_int_en_vec[prvU]
    );
    // mtvec
    Reg#(Bit#(62)) mtvec_base_hi_reg <- mkCsrReg(0); // this is BASE[63:2]
    Reg#(Bit#(1)) mtvec_mode_low_reg <- mkCsrReg(0); // this is MODE[0]
    Reg#(Data) mtvec_csr = concatReg3(
        mtvec_base_hi_reg, readOnlyReg(1'b0), mtvec_mode_low_reg
    );
    // mcounteren
    Reg#(Bit#(1)) mcounteren_ir_reg <- mkCsrReg(0);
    Reg#(Bit#(1)) mcounteren_tm_reg <- mkCsrReg(0);
    Reg#(Bit#(1)) mcounteren_cy_reg <- mkCsrReg(0);
    Reg#(Data) mcounteren_csr = concatReg5(
        readOnlyReg(32'b0),
        readOnlyReg(~29'b0), // hpmcounter 3-31 currently always accessible in S mode
        mcounteren_ir_reg, mcounteren_tm_reg, mcounteren_cy_reg
    );
    // mscratch
    Reg#(Data) mscratch_csr <- mkCsrReg(0);
    // mepc: FIXME Since we don't have C extension, mepc should be 4-byte
    // aligned. However, spike is not checking this, so we don't implement it.
    Reg#(Data) mepc_csr <- mkCsrReg(0);
    // mcause
    Reg#(Bit#(1)) mcause_interrupt_reg <- mkCsrReg(0);
    Reg#(Bit#(4)) mcause_code_reg      <- mkCsrReg(0);
    Reg#(Data) mcause_csr = concatReg3(
        mcause_interrupt_reg, readOnlyReg(59'b0), mcause_code_reg
    );
    function Data fn_mcause_val (Bit #(1) mcause_interrupt_val, Bit #(4) mcause_code_val);
       return { mcause_interrupt_val, 59'b0, mcause_code_val };
    endfunction

    // mtval (mbadaddr in spike)
    Reg#(Data) mtval_csr <- mkCsrReg(0);
    // mip
    Vector#(4, Reg#(Bit#(1))) external_int_pend_vec = replicate(readOnlyReg(0));
    external_int_pend_vec[prvU] <- mkCsrReg(0);
    external_int_pend_vec[prvS] <- mkCsrReg(0);
    external_int_pend_vec[prvM] <- mkCsrReg(0);    // TODO: bug (writeable by CSRRx)?
    Vector#(4, Reg#(Bit#(1))) timer_int_pend_vec = replicate(readOnlyReg(0));
    timer_int_pend_vec[prvU] <- mkCsrReg(0);
    timer_int_pend_vec[prvS] <- mkCsrReg(0);
    timer_int_pend_vec[prvM] <- mkCsrReg(0);    // TODO: bug (writeable by CSRRx)?
    Vector#(4, Reg#(Bit#(1))) software_int_pend_vec = replicate(readOnlyReg(0));
    software_int_pend_vec[prvU] <- mkCsrReg(0);
    software_int_pend_vec[prvS] <- mkCsrReg(0);
    software_int_pend_vec[prvM] <- mkCsrReg(0);    // TODO: bug (writeable by CSRRx)?
    Reg#(Data) mip_csr = concatReg13(
        readOnlyReg(52'b0),
        // External interrupts
        readOnlyReg(external_int_pend_vec[prvM]),    // MEIP is read-only to software
        readOnlyReg(1'b0),
        external_int_pend_vec[prvS],
        readOnlyReg(1'b0),    // only if misa.N: external_int_pend_vec[prvU],
        // Timer interrupts
        readOnlyReg(timer_int_pend_vec[prvM]),       // MTIP is read-only to software
        readOnlyReg(1'b0),
        timer_int_pend_vec[prvS],
        readOnlyReg(1'b0),    // only if misa.N: timer_int_pend_vec[prvU],
        // Software interrupts
        readOnlyReg(software_int_pend_vec[prvM]),    // MSIP is read-only to software
        readOnlyReg(1'b0),
        software_int_pend_vec[prvS],
        readOnlyReg(1'b0)     // only if misa.N: software_int_pend_vec[prvU]
    );
    // MIP and MIE fields are WARL (Write Any Read Legal)
    // We support M-privilege and S-privilege bits only;
    // this mask allows only those bits through.
    Data mip_mie_warl_mask = zeroExtend (12'h_222);

    // minstret
    Ehr#(2, Data) minstret_ehr <- mkCsrEhr(0);
    Reg#(Data) minstret_csr = minstret_ehr[0];
    // mcycle
    Ehr#(2, Data) mcycle_ehr <- mkCsrEhr(0);
    Reg#(Data) mcycle_csr = mcycle_ehr[0];
    // mvendorid
    Reg#(Data) mvendorid_csr = readOnlyReg(0);
    // marchid
    Reg#(Data) marchid_csr = readOnlyReg(0);
    // mimpid
    Reg#(Data) mimpid_csr = readOnlyReg(0);
    // mhartid
    Reg#(Data) mhartid_csr = readOnlyReg(hartid);

    // Supervisor level CSRs
    // sstatus: restricted view of mstatus
    Reg#(Data) sstatus_csr = concatReg17(
        sd_reg, readOnlyReg(29'b0), uxl_reg, readOnlyReg(12'b0),
        mxr_reg, sum_reg, readOnlyReg(1'b0), xs_reg, fs_reg,
        readOnlyReg(4'b0), spp_reg,
        readOnlyReg(2'b0), prev_ie_vec[prvS], prev_ie_vec[prvU],
        readOnlyReg(2'b0), ie_vec[prvS], ie_vec[prvU]
    );
    function Data fn_sstatus_val (Bit #(2) uxl_val,
				  Bit #(1) mxr_val, Bit #(1) sum_val,
				  Bit #(2) xs_val,  Bit #(2) fs_val,
				  Bit #(1) spp_val,
				  Bit #(1) prev_ie_vec_prvS_val,
				  Bit #(1) prev_ie_vec_prvU_val,
				  Bit #(1) ie_vec_prvS_val,
				  Bit #(1) ie_vec_prvU_val);
       return {fn_sd_val (xs_val, fs_val),
	       27'b0, 2'b0, uxl_val, 12'b0,
	       mxr_val, sum_val, 1'b0, xs_val, fs_val,
	       4'b0, spp_val,
	       2'b0,
	       prev_ie_vec_prvS_val, prev_ie_vec_prvU_val,
	       2'b0,
	       ie_vec_prvS_val,      ie_vec_prvU_val};
    endfunction

    // sie: restricted view of mie
    Reg#(Data) sie_csr = concatReg9(
        readOnlyReg(54'b0),
        external_int_en_vec[prvS], readOnlyReg(1'b0),    // only if misa.N: external_int_en_vec[prvU],
        readOnlyReg(2'b0),
        timer_int_en_vec[prvS], readOnlyReg(1'b0),    // only if misa.N: timer_int_en_vec[prvU],
        readOnlyReg(2'b0),
        software_int_en_vec[prvS], readOnlyReg(1'b0)    // only if misa.N: software_int_en_vec[prvU]
    );
    // stvec
    Reg#(Bit#(62)) stvec_base_hi_reg <- mkCsrReg(0); // BASE[63:2]
    Reg#(Bit#(1)) stvec_mode_low_reg <- mkCsrReg(0); // MODE[0]
    Reg#(Data) stvec_csr = concatReg3(
        stvec_base_hi_reg, readOnlyReg(1'b0), stvec_mode_low_reg
    );
    // scounteren
    Reg#(Bit#(1)) scounteren_ir_reg <- mkCsrReg(0);
    Reg#(Bit#(1)) scounteren_tm_reg <- mkCsrReg(0);
    Reg#(Bit#(1)) scounteren_cy_reg <- mkCsrReg(0);
    Reg#(Data) scounteren_csr = concatReg5(
        readOnlyReg(32'b0),
        readOnlyReg(~29'b0), // hpmcounter 3-31 currently always accessible in U mode
        scounteren_ir_reg, scounteren_tm_reg, scounteren_cy_reg
    );
    // sscratch
    Reg#(Data) sscratch_csr <- mkCsrReg(0);
    // sepc: FIXME Since we don't have C extension, sepc should be 4-byte
    // aligned. However, spike is not checking this, so we don't implement it.
    Reg#(Data) sepc_csr <- mkCsrReg(0);
    // scause
    Reg#(Bit#(1)) scause_interrupt_reg <- mkCsrReg(0);
    Reg#(Bit#(4)) scause_code_reg      <- mkCsrReg(0);
    Reg#(Data) scause_csr = concatReg3(
        scause_interrupt_reg, readOnlyReg(59'b0), scause_code_reg
    );
    function Data fn_scause_val (Bit #(1) scause_interrupt_val, Bit #(4) scause_code_val);
       return { scause_interrupt_val, 59'b0, scause_code_val };
    endfunction

    // stval (sbadaddr in spike)
    Reg#(Data) stval_csr <- mkCsrReg(0);
    // sip: restricted view of mip
    Reg#(Data) sip_csr = concatReg9(
        readOnlyReg(54'b0),
        external_int_pend_vec[prvS], readOnlyReg(1'b0),    // only if misa.N: external_int_pend_vec[prvU],
        readOnlyReg(2'b0),
        timer_int_pend_vec[prvS], readOnlyReg(1'b0),    // only if misa.N: timer_int_pend_vec[prvU],
        readOnlyReg(2'b0),
        software_int_pend_vec[prvS], readOnlyReg(1'b0)    // only if misa.N: software_int_pend_vec[prvU]
    );

    // SIP and SIE fields are WARL (Write Any Read Legal)
    // We support S-privilege bits only;
    // this mask allows only those bits through.
    Data sip_sie_warl_mask = zeroExtend (12'h_222);

    // satp (sptbr in spike): FIXME we only support Bare and Sv39, so we hack
    // the encoding of mode[3:0] field. Only mode[3] is relevant, other bits
    // are always 0
    Reg#(Bit#(1)) vm_mode_sv39_reg <- mkCsrReg(0);
    Reg#(Bit#(4)) vm_mode_reg = concatReg2(vm_mode_sv39_reg, readOnlyReg(3'b0));
    Reg#(Asid) asid_reg <- mkCsrReg(0);
    Reg#(Bit#(16)) full_asid_reg = zeroExtendReg(asid_reg);
    Reg#(Bit#(44)) ppn_reg <- mkCsrReg(0);
    Reg#(Data) satp_csr = concatReg3(vm_mode_reg, full_asid_reg, ppn_reg);

    // User level CSRs
    // According to spike, any write to fflags/frm/fcsr will set fs_reg as
    // dirty, regardless of whether the write truly changes value or not.
    // Besides, any non-zero FP exception flags will also make fs_reg dirty. 
    // fflags: if we directly change fflags_reg (instead of fflags_csr), then
    // we must set fs_reg manually
    Reg#(Bit#(5)) fflags_reg <- mkCsrReg(0);
    Reg#(Data) fflags_csr = addWriteSideEffect(
        zeroExtendReg(fflags_reg), fs_reg._write(2'b11)
    );
    // frm: if we directly change frm_reg (instead of frm_csr), then we must
    // set fs_reg manually
    Reg#(Bit#(3)) frm_reg <- mkCsrReg(0);
    Reg#(Data) frm_csr = addWriteSideEffect(
        zeroExtendReg(frm_reg), fs_reg._write(2'b11)
    );
    // fcsr
    Reg#(Data) fcsr_csr = addWriteSideEffect(
        zeroExtendReg(concatReg2(frm_reg, fflags_reg)), fs_reg._write(2'b11)
    );
    // cycle
    Reg#(Data) cycle_csr = readOnlyReg(mcycle_csr);
    // time
    Reg#(Data) time_reg <- mkCsrReg(0);
    Reg#(Data) time_csr = readOnlyReg(time_reg);
    // instret
    Reg#(Data) instret_csr = readOnlyReg(minstret_csr);
    // terminate (non-standard)
    Terminate  terminate_module <- mkTerminate;
    Reg#(Data) terminate_csr = terminate_module.reg_ifc;
    // whether performance stats is collected
    StatsCsr stats_module <- mkStatsCsr;
    Reg#(Data) stats_csr = stats_module.reg_ifc;

   Reg #(Data) rg_tselect <- mkConfigReg (0);
   // Note: ISA test rv64mi-p-breakpoint assumes tdata1's reset value == 0
   // Until we implement trigger functionality,
   // force 'tdata1.type' field ([xlen-1:xlen-4]) to zero
   // meaning: 'There is no trigger at this tselect'
   Reg #(Bit #(4))  rg_tdata1_type  <- mkReadOnlyReg (0);
   Reg #(Bit #(1))  rg_tdata1_dmode <- mkCsrReg (0);
   Reg #(Bit #(59)) rg_tdata1_data  <- mkCsrReg (0);
   Reg #(Data) rg_tdata1  = concatReg3 (rg_tdata1_type, rg_tdata1_dmode, rg_tdata1_data);
   Reg #(Data) rg_tdata2  <- mkConfigRegU;
   Reg #(Data) rg_tdata3  <- mkConfigRegU;

`ifdef INCLUDE_GDB_CONTROL
   // DCSR is 32b even in RV64
   Bit #(32) dcsr_reset_value =  {4'h4,    // [31:28]  xdebugver
				  12'h0,   // [27:16]  reserved
				  1'h0,    // [15]     ebreakm
				  1'h0,    // [14]     reserved
				  1'h0,    // [13]     ebreaks
				  1'h0,    // [12]     ebreaku
				  1'h0,    // [11]     stepie
				  1'h0,    // [10]     stopcount
				  1'h0,    // [9]      stoptime
				  3'h0,    // [8:6]    cause    // WARNING: 0 is non-standard
				  1'h0,    // [5]      reserved
				  1'h1,    // [4]      mprven
				  1'h0,    // [3]      nmip    // non-maskable interrupt pending
				  1'h0,    // [2]      step
				  2'h3};   // [1:0]    prv (machine mode)

   // RV64: dcsr's upper 32b zeroExtended/ignored
   Reg #(Data) rg_dcsr      <- mkConfigReg (zeroExtend (dcsr_reset_value));
   Reg #(Data) rg_dpc       <- mkConfigReg (truncate (soc_map_struct.pc_reset_value));
   Reg #(Data) rg_dscratch0 <- mkConfigRegU;
   Reg #(Data) rg_dscratch1 <- mkConfigRegU;
`endif

`ifdef PERFORMANCE_MONITORING
   PerfCountersVec perf_counters <- mkPerfCountersToooba;

   //Reg #(Bit #(2)) rg_ctr_inhib_lsb   <- mkReg (0);
   //Wire #(Bit #(2)) w_ctr_inhib_lsb <- mkWire;
   //Bit #(3) ctr_inhibit_lsb = { rg_ctr_inhib_lsb [1], 0, rg_ctr_inhib_lsb [0] };
   //Word ctr_inhibit = zeroExtend ({ perf_counters.read_ctr_inhibit, ctr_inhibit_lsb });
   //CSR_Addr no_of_ctrs = fromInteger (valueOf (No_Of_Ctrs));
`endif

`ifdef SECURITY
    // sanctum machine CSRs

    // ### Enclave virtual base and mask
    // (per-core) registers
    // ( defines a virtual region for which enclave page tables are used in
    //   place of OS-controlled page tables)
    // (machine-mode non-standard read/write)
    Reg#(Data) mevbase_csr <- mkCsrReg(maxBound); // impossible base & mask,
    Reg#(Data) mevmask_csr <- mkCsrReg(0);  // so no enclave accesses are possible

    // ### Enclave page table base
    // (per core) register
    // ( pointer to a separate page table data structure used to translate enclave
    //   virtual addresses)
    // (machine-mode non-standard read/write)
    Reg#(Bit#(44)) eppn_reg <- mkCsrReg(0);
    Reg#(Data) meatp_csr = zeroExtendReg(eppn_reg);

    // ### DRAM bitmap
    // (per core) registers (OS and Enclave)
    // ( white-lists the DRAM regions the core is allowed to access via OS and
    //   enclave virtual addresses)
    // (machine-mode non-standard read/write)
    Reg#(Data) mmrbm_csr <- mkCsrReg(maxBound);
    Reg#(Data) memrbm_csr <- mkCsrReg(0);

    // ### Protected region base and mask
    // (per core) registers (OS and Enclave)
    // ( these are used to prevent address translation into a specific range of
    //   physical addresses, for example to protect the security monitor from all software)
    // (machine-mode non-standard read/write)
    Reg#(Data) mparbase_csr <- mkCsrReg(maxBound);
    Reg#(Data) mparmask_csr <- mkCsrReg(0);
    Reg#(Data) meparbase_csr <- mkCsrReg(0);
    Reg#(Data) meparmask_csr <- mkCsrReg(0);

    // ### Turn on/off speculation
    Reg#(Bit#(2)) mspec_reg <- mkCsrReg(mSpecAll);
    Reg#(Data) mspec_csr = zeroExtendReg(mspec_reg);

    // sanctum user CSR
    // ### true random number
    // For now, we skip secure boot, keep TRNG = 0
    Reg#(Data) trng_csr <- mkReadOnlyReg(0); //mkTRNG;
`endif

    rule incCycle;
        mcycle_ehr[1] <= mcycle_ehr[1] + 1;
    endrule

    // Function for getting a csr given an index
    function Reg#(Data) get_csr(CSR csr);
        Reg#(Data) ret = readOnlyReg(64'b0);
`ifdef PERFORMANCE_MONITORING
	function Reg#(Data) getCsrFromRange(Bit#(12) first, Bit#(12) last, Vector#(n, Reg#(Data)) vec, Reg#(Data) def);
		Bit#(12) c = pack(csr);
		if ((first <= c) && (c <= last)) return vec[c - first];
		else return def;
	endfunction
	ret = getCsrFromRange(pack(CSRmhpcounter3), pack(CSRmhpcounter31), perf_counters.counter_vec, ret);
	ret = getCsrFromRange(pack(CSRmhpmevent3),  pack(CSRmhpmevent31),  perf_counters.event_vec,   ret);
	ret = getCsrFromRange(pack(CSRhpcounter3),  pack(CSRhpcounter31),  perf_counters.counter_vec, ret);
`endif
        return (case (csr)
            // User CSRs
            CSRfflags:     fflags_csr;
            CSRfrm:        frm_csr;
            CSRfcsr:       fcsr_csr;
            CSRcycle:      cycle_csr;
            CSRtime:       time_csr;
            CSRinstret:    instret_csr;
            CSRterminate:  terminate_csr;
            CSRstats:      stats_csr;
            // Supervisor CSRs
            CSRsstatus:    sstatus_csr;
            CSRsie:        sie_csr;
            CSRstvec:      stvec_csr;
            CSRscounteren: scounteren_csr;
            CSRsscratch:   sscratch_csr;
            CSRsepc:       sepc_csr;
            CSRscause:     scause_csr;
            CSRstval:      stval_csr;
            CSRsip:        sip_csr;
            CSRsatp:       satp_csr;
            // Machine CSRs
            CSRmstatus:    mstatus_csr;
            CSRmisa:       misa_csr;
            CSRmedeleg:    medeleg_csr;
            CSRmideleg:    mideleg_csr;
            CSRmie:        mie_csr;
            CSRmtvec:      mtvec_csr;
            CSRmcounteren: mcounteren_csr;
            CSRmscratch:   mscratch_csr;
            CSRmepc:       mepc_csr;
            CSRmcause:     mcause_csr;
            CSRmtval:      mtval_csr;
            CSRmip:        mip_csr;
            CSRmcycle:     mcycle_csr;
            CSRminstret:   minstret_csr;
            CSRmvendorid:  mvendorid_csr;
            CSRmarchid:    marchid_csr;
            CSRmimpid:     mimpid_csr;
            CSRmhartid:    mhartid_csr;
`ifdef PERFORMANCE_MONITORING
            CSRmcounterinhibit: perf_counters.inhibit;
`endif
`ifdef SECURITY
            CSRmevbase:    mevbase_csr;
            CSRmevmask:    mevmask_csr;
            CSRmeatp:      meatp_csr;
            CSRmmrbm:      mmrbm_csr;
            CSRmemrbm:     memrbm_csr;
            CSRmparbase:   mparbase_csr;
            CSRmparmask:   mparmask_csr;
            CSRmeparbase:  meparbase_csr;
            CSRmeparmask:  meparmask_csr;
            CSRmspec:      mspec_csr;
            CSRtrng:       trng_csr;
`endif

	   CSRtselect:    rg_tselect;
	   CSRtdata1:     rg_tdata1;
	   CSRtdata2:     rg_tdata2;
	   CSRtdata3:     rg_tdata3;

`ifdef INCLUDE_GDB_CONTROL
	   CSRdcsr:       rg_dcsr;    // TODO: take NMI into account (cf. Piccolo/Flute)
	   CSRdpc:        rg_dpc;
	   CSRdscratch0:  rg_dscratch0;
	   CSRdscratch1:  rg_dscratch1;
`endif

            default: ret;
        endcase);
    endfunction

   // ================================================================
   // This function is the WARL (Write Any Read Legal) transform
   // performed during CSR writes.  Currently it duplicates the logic
   // in the _write method of CSRs; ideally this function should be
   // separate from the _write method, which should remain as an
   // ordinary _write.  The WARL'd value is needed for Tandem
   // Verification.

   function Data fv_warl_xform (CSR csr, Data x);
      Asid      x_asid = truncate (x [59:44]);
      Bit #(16) asid   = zeroExtend (x_asid);
      return (
	 case (csr)
	    // Machine CSRs
	    CSRmisa:      {getXLBits, 36'b0, getExtensionBits(isa)};
	    CSRmvendorid: 0;
	    CSRmarchid:   0;
	    CSRmimpid:    0;
	    CSRmhartid:   hartid;
	    CSRmstatus:   fn_mstatus_val (getXLBits,    // sxl
					  getXLBits,    // uxl
					  x [22],       // tsr
					  x [21],       // tw
					  x [20],       // tvm
					  x [19],       // mxr
					  x [18],       // sum
					  x [17],       // mprv
					  2'b0,         // xs
					  ((isa.f || isa.d) ? x [14:13] : 2'b0),    // fs
					  x [12:11],    // mpp
					  x [8],        // spp
					  x [7],        // prev_ie_vec[prvM]
					  x [5],        // prev_ie_vec[prvS]
					  x [4],        // prev_ie_vec[prvU]
					  x [3],        // ie_vec[prvM]
					  x [1],        // ie_vec[prvS]
					  x [0]);       // ie_vec[prvU]
	    CSRmtvec:      { x[63:2], 1'b0, x[0]};
	    CSRmedeleg:    { 48'b0, x[15], 1'b0, x[13:12], x[11], 1'b0, x[9:0]};
	    CSRmideleg:    { 52'b0, x[11], 1'b0, x[9:8], x[7], 1'b0, x[5:4], x[3], 1'b0, x[1:0]};
	    CSRmip:        ((mip_csr & (~ mip_mie_warl_mask)) | (x & mip_mie_warl_mask));
	    CSRmie:        (x & mip_mie_warl_mask);
	    CSRmcounteren: { 61'b0, x[2:0]};
	    CSRmcause:     { x[63], 59'b0, x[3:0] };

	    CSRtdata1:     { 4'b0, x [59:0] };    // Force tdata.type == 0 ("no trigger at this tselect")

	    // Supervisor level CSRs
	    CSRsstatus:   fn_sstatus_val (getXLBits,    // uxl
					  x [19],       // mxr
					  x [18],       // sum
					  2'b0,         // xs
					  ((isa.f || isa.d) ? x [14:13] : 2'b0),    // fs
					  x [8],        // spp
					  x [5],        // prev_ie_vec[prvS]
					  x [4],        // prev_ie_vec[prvU]
					  x [1],        // ie_vec[prvS]
					  x [0]);       // ie_vec[prvU]
	    CSRstvec:      { x[63:2], 1'b0, x[0]};
	    CSRsip:        ((sip_csr & (~ sip_sie_warl_mask)) | (x & sip_sie_warl_mask));
	    CSRsie:        (x & sip_sie_warl_mask);
	    CSRscounteren: { 61'b0, x[2:0]};
	    CSRscause:     { x[63], 59'b0, x[3:0] };
	    CSRsatp:       { x[63], 3'b0, asid,  x [43:0] };

	    // User level CSRs
	    CSRfflags:     { 59'b0, x [4:0] };
	    CSRfrm:        { 61'b0, x [2:0] };
	    CSRfcsr:       { 56'b0, x [7:0] };

`ifdef INCLUDE_GDB_CONTROL
	    // Debug Mode CSRs
	    CSRdcsr:       { 32'b0, x[31:28], 12'b0, x[14], 1'b0, x[13:6], 1'b0, x[4:0] };
`endif

	    default:       x;
         endcase);
   endfunction

   // ================================================================
   // INTERFACE

    method Data rd(CSR csr);
        return get_csr(csr)._read;
    endmethod

    method Action csrInstWr(CSR csr, Data x);
        get_csr(csr)._write(x);
`ifdef INCLUDE_GDB_CONTROL
        if (csr == CSRdcsr) begin
	   let prv = x [1:0];
	   prv_reg <= prv;
	end
`endif
    endmethod

    method Bool fpuInstNeedWr(Bit#(5) fflags, Bool fpu_dirty);
        Bool fflags_change = (fflags & fflags_reg) != fflags;
        // we need to set fs_reg as dirty in two cases
        // 1. FP reg is written (i.e., fpu_dirty)
        // 2. FP exception (i.e., fflags) is non-zero (try to match spike)
        Bool need_set_dirty = fs_reg != 2'b11 && (fpu_dirty || fflags != 0);
        return fflags_change || need_set_dirty;
    endmethod

    method Action fpuInstWr(Bit#(5) fflags);
        fs_reg <= 2'b11; // FPU must be dirty
        fflags_reg <= fflags_reg | fflags;
    endmethod

`ifdef INCLUDE_TANDEM_VERIF
    method Tuple2 #(Bit #(5), Data) fpuInst_csr_updates (Bit #(5)  fflags,
							 Bool      init_for_way0,
							 Bit #(5)  old_fflags,
							 Data      old_mstatus);

       // Note: old_fflags and old_mstatus are accumulated in
       // sequential program order, and so may differ from fflags_reg
       // and mstatus_csr, which only change after superscalar-wide
       // retirement.

       Bit #(5) old_fflags1  = (init_for_way0 ? fflags_reg  : old_fflags);
       Data     old_mstatus1 = (init_for_way0 ? mstatus_csr : old_mstatus);

       Bit #(5) new_fflags  = (old_fflags1 | fflags);
       Data     new_mstatus = { 1'b1, old_mstatus1 [62:15], 2'b11, old_mstatus1 [12:0] };

       return tuple2 (new_fflags, new_mstatus);
    endmethod

    method Data getMIP;
       return mip_csr;
    endmethod
`endif

    method Data warl_xform (CSR csr, Data x);
       return fv_warl_xform (csr, x);
    endmethod

    method Maybe#(Interrupt) pending_interrupt;
        // first get all the pending interrupts
        Bit#(InterruptNum) pend_ints = truncate(mie_csr & mip_csr);
        // now find out all the truly enabled interrupts (that needs handling)
        Bit#(InterruptNum) enabled_ints = 0;
        // check interrupts that needs to be handled at M mode: all interrupts
        // are by default handled at M mode unless it is delegated in
        // mideleg_csr, we just need to ignore those interrupts
        if(prv_reg < prvM || (prv_reg == prvM && ie_vec[prvM] == 1)) begin
            enabled_ints = pend_ints & ~truncate(mideleg_csr);
        end
        // check interrupts that needs to be handled at S mode only if no
        // interrupt needs to be handled at M mode: interrupts handled at S
        // mode must be delegated in mideleg_csr
        if (enabled_ints == 0 &&
            (prv_reg < prvS || (prv_reg == prvS && ie_vec[prvS] == 1))) begin
            enabled_ints = pend_ints & truncate(mideleg_csr);
        end
        // According to spike, return the interrupt bit at LSB
        function Bool isEnabled(Integer i) = (enabled_ints[i] == 1);
        Vector#(InterruptNum, Integer) idxVec = genVector;
        if(find(isEnabled, idxVec) matches tagged Valid .i) begin
            return Valid (unpack(fromInteger(i)));
        end
        else begin
            return Invalid;
        end
    endmethod

    method ActionValue#(Trap_Updates) trap(Trap t, Addr pc, Addr addr, Bit #(32) orig_inst);
        // figure out trap cause & trap val
        Bit#(1) cause_interrupt = 0;
        Bit#(4) cause_code = 0;
        Data trap_val = 0;
        case(t) matches
            tagged Exception .e: begin
                cause_code = pack(e);
                trap_val = (case(e)
		    IllegalInst: zeroExtend (orig_inst);
                    InstAddrMisaligned, Breakpoint: return pc;

		    InstAccessFault, InstPageFault,
                    LoadAddrMisaligned, LoadAccessFault,
                    StoreAddrMisaligned, StoreAccessFault,
                    LoadPageFault, StorePageFault: return addr;

                    default: return 0;
                endcase);
            end
            tagged Interrupt .i: begin
                cause_code = pack(i);
                cause_interrupt = 1;
            end
        endcase
        // function to figure out next PC
        function Addr getNextPc(Bit#(1) mode_low, Bit#(62) base_hi);
            Addr base = {base_hi, 2'b0};
            if(mode_low == 1 && cause_interrupt == 1) begin
                // vector jump: only for interrupt
                return base + zeroExtend({cause_code, 2'b0});
            end
            else begin // direct jump
                return base;
            end
        endfunction
        // check if trap is delegated
        Bool deleg = prv_reg <= prvS && (case(t) matches
            tagged Exception .e: return medeleg_csr[pack(e)] == 1;
            tagged Interrupt .i: return mideleg_csr[pack(i)] == 1;
            default: return False;
        endcase);
        // handle the trap
        if(deleg) begin // handle in S mode
            // ie/prv stack
            prev_prv_vec[prvS] <= prv_reg;
            prv_reg <= prvS;
            prev_ie_vec[prvS] <= ie_vec[prvS];
            ie_vec[prvS] <= 0;
            // record trap info
            sepc_csr <= pc;
            scause_interrupt_reg <= cause_interrupt;
            scause_code_reg <= cause_code;
            stval_csr <= trap_val;
            // return next pc
            // return getNextPc(stvec_mode_low_reg, stvec_base_hi_reg);
	    Data sstatus_val = fn_sstatus_val (uxl_reg,
					       mxr_reg, sum_reg,
					       xs_reg,  fs_reg,
					       /* spp_reg */ prv_reg [0],
					       /* prev_ie_vec_[prvS] */ ie_vec[prvS],
					       prev_ie_vec [prvU],
					       /* ie_vec [prvS] */ 0,
					       ie_vec [prvU]);
	    Data scause_val = fn_scause_val (cause_interrupt, cause_code);
	    return Trap_Updates {new_pc: getNextPc(stvec_mode_low_reg, stvec_base_hi_reg)
`ifdef INCLUDE_TANDEM_VERIF
				 , prv:    prvS,
				 status: sstatus_val,
				 cause:  scause_val,
				 epc:    pc,
				 tval:   trap_val
`endif
				 };
        end
        else begin
            // ie/prv stack
            prev_prv_vec[prvM] <= prv_reg;
            prv_reg <= prvM;
            prev_ie_vec[prvM] <= ie_vec[prvM];
            ie_vec[prvM] <= 0;
            // record trap info
            mepc_csr <= pc;
            mcause_interrupt_reg <= cause_interrupt;
            mcause_code_reg <= cause_code;
            mtval_csr <= trap_val;
            // return next pc
            // return getNextPc(mtvec_mode_low_reg, mtvec_base_hi_reg);
	    Data mstatus_val = fn_mstatus_val (sxl_reg, uxl_reg,
					       tsr_reg, tw_reg,  tvm_reg,
					       mxr_reg, sum_reg, mprv_reg,
					       xs_reg,  fs_reg,
					       /* mpp */ prv_reg, spp_reg,
					       /* prev_ie_vec [prvM] */ ie_vec [prvM],
					       prev_ie_vec [prvS],
					       prev_ie_vec [prvU],
					       /* ie_vec [prvM] */ 0,
					       ie_vec [prvS],
					       ie_vec [prvU]);
	    Data mcause_val = fn_mcause_val (cause_interrupt, cause_code);
	    return Trap_Updates {new_pc: getNextPc(mtvec_mode_low_reg, mtvec_base_hi_reg)
`ifdef INCLUDE_TANDEM_VERIF
				 , prv:    prvM,
				 status: mstatus_val,
				 cause:  mcause_val,
				 epc:    pc,
				 tval:   trap_val
`endif
				 };
        end
        // XXX yield load reservation should be done outside this method
    endmethod

    method ActionValue#(RET_Updates) mret;
        prv_reg <= prev_prv_vec[prvM];
        prev_prv_vec[prvM] <= prvU;
        ie_vec[prvM] <= prev_ie_vec[prvM];
        prev_ie_vec[prvM] <= 1;

        Data mstatus_val = fn_mstatus_val(sxl_reg, uxl_reg,
					  tsr_reg, tw_reg,  tvm_reg,
					  mxr_reg, sum_reg, mprv_reg,
					  xs_reg,  fs_reg,
					  /* mpp */ prvU,
					  spp_reg,
					  /* prev_ie_vec [prvM] */ 1,
					  prev_ie_vec [prvS],
					  prev_ie_vec [prvU],
					  /* ie_vec [prvM] */ prev_ie_vec[prvM],
					  ie_vec [prvS],
					  ie_vec [prvU]);
        return RET_Updates {new_pc: mepc_csr
`ifdef INCLUDE_TANDEM_VERIF
			    , prv:    prev_prv_vec[prvM],
			    status: mstatus_val
`endif
			    };
    endmethod

    method ActionValue#(RET_Updates) sret;
        prv_reg <= prev_prv_vec[prvS];
        prev_prv_vec[prvS] <= prvU;
        ie_vec[prvS] <= prev_ie_vec[prvS];
        prev_ie_vec[prvS] <= 1;

        // For Tandem Verification, we return the full underlying MSTATUS register
        Data mstatus_val = fn_mstatus_val(sxl_reg, uxl_reg,
					  tsr_reg, tw_reg,  tvm_reg,
					  mxr_reg, sum_reg, mprv_reg,
					  xs_reg,  fs_reg,
					  mpp_reg,
					  /* spp_reg */ prvU [0],

					  prev_ie_vec [prvM],
					  /* prev_ie_vec_[prvS] */ 1,
					  prev_ie_vec [prvU],

					  ie_vec [prvM],
					  /* ie_vec [prvS] */ prev_ie_vec[prvS],
					  ie_vec [prvU]);
        return RET_Updates {new_pc: sepc_csr
`ifdef INCLUDE_TANDEM_VERIF
			    , prv:    prev_prv_vec[prvS],
			    status: mstatus_val
`endif
			    };
    endmethod

    method VMInfo vmI;
        // for inst fetch, NO need to consider MPRV
        Bit#(2) prv = prv_reg;
        return VMInfo {
            prv: prv,
            asid: asid_reg,
            sv39: prv < prvM && vm_mode_sv39_reg == 1,
            exeReadable: mxr_reg == 1,
            userAccessibleByS: sum_reg == 1,
            basePPN: ppn_reg
`ifdef SECURITY
            , sanctum_evbase:   mevbase_csr,
            sanctum_evmask:     mevmask_csr,
            sanctum_ebasePPN:   eppn_reg,
            sanctum_mrbm:       mmrbm_csr,
            sanctum_emrbm:      memrbm_csr,
            sanctum_parbase:    mparbase_csr,
            sanctum_parmask:    mparmask_csr,
            sanctum_eparbase:   meparbase_csr,
            sanctum_eparmask:   meparmask_csr,
            // enclave / security monitor should never execute instructions
            // from untrusted shared region
            sanctum_authShared: False
`endif
        };
    endmethod

    method VMInfo vmD;
        // for load/store, need to consider MPRV
        Bit#(2) prv = (mprv_reg == 1) ? prev_prv_vec[prvM] : prv_reg;
        return VMInfo {
            prv: prv,
            asid: asid_reg,
            sv39: prv < prvM && vm_mode_sv39_reg == 1,
            exeReadable: mxr_reg == 1,
            userAccessibleByS: sum_reg == 1,
            basePPN: ppn_reg
`ifdef SECURITY
            , sanctum_evbase:   mevbase_csr,
            sanctum_evmask:     mevmask_csr,
            sanctum_ebasePPN:   eppn_reg,
            sanctum_mrbm:       mmrbm_csr,
            sanctum_emrbm:      memrbm_csr,
            sanctum_parbase:    mparbase_csr,
            sanctum_parmask:    mparmask_csr,
            sanctum_eparbase:   meparbase_csr,
            sanctum_eparmask:   meparmask_csr,
            // enclave / security monitor can read/write untrusted shared
            // region when speculation is off (either by mspec CSR or in M
            // mode)
            // XXX Because of the effects of mprv, we have to use prv_reg here
            // instead of prv. Otherwise, we may be in M mode, but prv=S, and
            // still forbid shared accesses
            sanctum_authShared: mspec_reg != mSpecAll || prv_reg == prvM
`endif
        };
    endmethod

    method CsrDecodeInfo decodeInfo = CsrDecodeInfo {
        frm: frm_reg,
        fEnabled: fs_reg != 0,
        prv: prv_reg,
        trapVM: tvm_reg == 1,
        timeoutWait: tw_reg == 1,
        trapSret: tsr_reg == 1,
        cycleReadableByS: mcounteren_cy_reg == 1,
        cycleReadableByU: mcounteren_cy_reg == 1 && scounteren_cy_reg == 1,
        instretReadableByS: mcounteren_ir_reg == 1,
        instretReadableByU: mcounteren_ir_reg == 1 && scounteren_ir_reg == 1,
        timeReadableByS: mcounteren_tm_reg == 1,
        timeReadableByU: mcounteren_tm_reg == 1 && scounteren_tm_reg == 1
    };

    method Action incInstret(SupCnt x);
        minstret_ehr[1] <= minstret_ehr[1] + zeroExtend(x);
    endmethod

    method Action setTime(Data t);
        time_reg <= t;
    endmethod

    method getMSIP = software_int_pend_vec[prvM]._read;
    method setMSIP = software_int_pend_vec[prvM]._write;
    method setMTIP = timer_int_pend_vec[prvM]._write;

   // Bluespec: external interrupts targeting machine and supervisor modes
    method Action setMEIP (Bit #(1) v);
       external_int_pend_vec[prvM] <= v;
    endmethod

    method Action setSEIP (Bit #(1) v);
       external_int_pend_vec[prvS] <= v;
    endmethod

    method terminate = terminate_module.terminate;

    // performance stats
    method doPerfStats = stats_module.doPerfStats;
    method sendDoStats = stats_module.sendDoStats;
    method recvDoStats = stats_module.recvDoStats;

   // ----------------
   // Bluespec:
   // Methods when Debug Module is present

`ifdef INCLUDE_GDB_CONTROL
   // Read dpc
   method Addr dpc_read ();
      return rg_dpc;
   endmethod

   // Update dpc
   method Action dpc_write (Addr pc);
      rg_dpc <= pc;
   endmethod

   // Check whether to enter Debug Mode based on dcsr.{ebreakm, ebreaks, ebreaku}
   method Bit #(1) dcsr_break_bit;
      return case (prv_reg)
		prvM: rg_dcsr [15];
		prvS: rg_dcsr [13];
		prvU: rg_dcsr [12];
	     endcase;
   endmethod

   // Check whether to enter Debug Mode based on dcsr.step
   method Bit #(1) dcsr_step_bit;
      return rg_dcsr [2];
   endmethod

   // Update 'cause' in DCSR
   // Is invoked by logic that stops a hart, to enter Debug Mode
   method Action dcsr_cause_write (Bit #(3) dcsr_cause);
      rg_dcsr <= { 32'b0, rg_dcsr [31:9], dcsr_cause, rg_dcsr [5:2], prv_reg };

      /*
      $display ("%0d: %m mkCsrFile.method-dcsr_cause_write: cause %0d, prv %0d",
		cur_cycle, dcsr_cause, prv_reg);
      */
   endmethod

`endif

`ifdef PERFORMANCE_MONITORING
    method send_performance_events = perf_counters.send_performance_events;
`endif
endmodule
