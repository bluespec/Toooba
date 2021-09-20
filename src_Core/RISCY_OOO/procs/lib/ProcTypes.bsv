
// Copyright (c) 2017 Massachusetts Institute of Technology
// Portions (c) 2020 Bluespec, Inc.
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
import Vector::*;
import Types::*;
import FShow::*;
import DefaultValue::*;
import MemoryTypes::*;
import CHERICap::*;
import CHERICC_Fat::*;
import ISA_Decls_CHERI::*;
`ifdef RVFI_DII
import GetPut::*;
import RVFI_DII_Types::*;
`endif

typedef `NUM_CORES CoreNum;
typedef Bit#(TLog#(CoreNum)) CoreId;

typedef `sizeSup SupSize;
typedef Bit#(TLog#(SupSize)) SupWaySel;
typedef Bit#(TLog#(TAdd#(SupSize, 1))) SupCnt;
typedef TMul#(SupSize, 2) SupSizeX2;
typedef Bit#(TLog#(SupSizeX2)) SupWayX2Sel;

typedef `NUM_EPOCHS NumEpochs;
typedef Bit#(TLog#(NumEpochs)) Epoch;

typedef `NUM_SPEC_TAGS NumSpecTags;
typedef Bit#(TLog#(NumSpecTags)) SpecTag;
typedef Bit#(NumSpecTags) SpecBits;

typedef `ROB_SIZE NumInstTags;
typedef TDiv#(NumInstTags, SupSize) SingleScalarSize;
typedef Bit#(TLog#(SingleScalarSize)) SingleScalarPtr;
typedef Bit#(TAdd#(1, TLog#(SingleScalarSize))) SingleScalarLen;

// consider ROB as a FIFO of size 2^log(NumInstTags)
// inst time is the index of the inst in the FIFO
// This indicates older/younger inst
typedef Bit#(TLog#(NumInstTags)) InstTime;

typedef struct {
    SupWaySel way; // which way in superscalar
    SingleScalarPtr ptr; // pointer within a way
    InstTime t; // inst time in ROB (for dispatch in reservation station)
`ifdef RVFI_DII
    Dii_Parcel_Id dii_next_pid;
`endif
} InstTag deriving(Bits, Eq, FShow);

`ifdef RVFI_DII
typedef Vector#(SupSize, Maybe#(RVFI_DII_Execution #(64, 64))) Rvfi_Traces;
typedef Vector#(TMul#(SupSize, 2), RVFI_DII_Parcel_Resp) Dii_Parcel_Resps;
typedef Vector#(TMul#(SupSize, 2), Bit#(16)) Dii_Parcels;

interface Toooba_RVFI_DII_Server;
    interface Get#(Dii_Parcel_Id) seqReqFirst;
    interface Put#(Dii_Parcel_Resps) parcelResps;
    interface Get#(Rvfi_Traces) trace_report;
endinterface
`endif

typedef `SB_SIZE SBSize;
typedef Bit#(TLog#(SBSize)) SBIndex;

//typedef `LDSTQ_SIZE LdStQSize;
//typedef Bit#(TLog#(LdStQSize)) LdStQTag;

typedef `LDQ_SIZE LdQSize;
typedef Bit#(TLog#(LdQSize)) LdQTag;

typedef `STQ_SIZE StQSize;
typedef Bit#(TLog#(StQSize)) StQTag;

typedef union tagged {
    LdQTag Ld;
    StQTag St;
} LdStQTag deriving(Bits, Eq, FShow);

typedef enum {Ld, St, Cache} LdKilledBy deriving(Bits, Eq, FShow);

typedef `DRAM_MAX_REQS DramMaxReqs;
typedef `DRAM_MAX_READS DramMaxReads;
typedef `DRAM_MAX_WRITES DramMaxWrites;
typedef `DRAM_LATENCY DramLatency;

typedef Bit#(`LOG_DEADLOCK_CYCLES) DeadlockTimer;

typedef struct {
    // ISA modes
    Bool s;
    Bool u;
    // standard ISA extensions
    Bool m;
    Bool a;
    Bool f;
    Bool d;
    Bool c;
} RiscVISASubset deriving (Bits, Eq, FShow);

instance DefaultValue#(RiscVISASubset);
    function RiscVISASubset defaultValue = RiscVISASubset {
        s: True, u: True,
        m: `m , a: `a , f: `f , d: `d, c: `c
    };
endinstance

function Bit#(2) getXLBits = 2'b10; // MXL/SXL/UXL fix to RV64

function Bit#(26) getExtensionBits(RiscVISASubset isa);
    // include S and I by default
    Bit#(26)   ext =       26'b00000001000000000100000000;
    if (isa.u) ext = ext | 26'b00000100000000000000000000;
    if (isa.m) ext = ext | 26'b00000000000001000000000000;
    if (isa.a) ext = ext | 26'b00000000000000000000000001;
    if (isa.f) ext = ext | 26'b00000000000000000000100000;
    if (isa.d) ext = ext | 26'b00000000000000000000001000;
    if (isa.c) ext = ext | 26'b00000000000000000000000100;
    return ext;
endfunction

typedef Bit#(5) GprRIndx;
typedef Bit#(5) FpuRIndx;
typedef union tagged {
    GprRIndx Gpr;
    FpuRIndx Fpu;
} ArchRIndx deriving (Bits, Eq, FShow, Bounded);

typedef TExp#(SizeOf#(ArchRIndx)) NumArchReg;

typedef TAdd#(NumArchReg, `ROB_SIZE) NumPhyReg;
typedef Bit#(TLog#(NumPhyReg)) PhyRIndx;

typedef struct {
    PhyRIndx indx;
    Bool isFpuReg; // need to keep track of this for fs
} PhyDst deriving (Bits, Eq, FShow);

typedef struct {
    Maybe#(ArchRIndx) src1;
    Maybe#(ArchRIndx) src2;
    Maybe#(FpuRIndx) src3;
    Maybe#(ArchRIndx) dst;
} ArchRegs deriving (Bits, Eq, FShow);

typedef struct {
    Maybe#(PhyRIndx) src1;
    Maybe#(PhyRIndx) src2;
    Maybe#(PhyRIndx) src3;
    Maybe#(PhyDst) dst;
} PhyRegs deriving (Bits, Eq, FShow);

typedef struct {
    Bool src1;
    Bool src2;
    Bool src3;
    Bool dst;
} RegsReady deriving(Bits, Eq, FShow);

function Bool allRegsReady(RegsReady x);
    return x.src1 && x.src2 && x.src3 && x.dst;
endfunction

typedef struct { Bit#(7) opc; } Opcode deriving(Bits, Eq);

`define Opcode(n, v) Opcode opc``n = Opcode { opc: v };
`include "Opcodes.bsvi"
`Opcode(Invalid, 7'b0)
`undef Opcode

instance FShow#(Opcode);
    function Fmt fshow(Opcode scr);
        return (case(scr.opc)
`define Opcode(n, v) v: $format(`"``opc``n```");
`include "Opcodes.bsvi"
`undef Opcode
            default: $format("opcInvalid");
        endcase);
    endfunction
endinstance

function Opcode unpackOpcode(Bit#(7) opc);
    return (case(opc)
`define Opcode(n, v) v: opc``n;
`include "Opcodes.bsvi"
`undef Opcode
        default: opcInvalid;
    endcase);
endfunction

typedef struct { Bit#(12) addr; } CSR deriving(Bits, Eq);

`define CSR(n, v) CSR csrAddr``n = CSR { addr: v };
`include "CSRs.bsvi"
// CSR that catches all the unimplemented CSRs. To avoid exception on this,
// make it a user non-standard read/write CSR.
// Bluespec: in RenameStage.getTrap(), we force this to be a csr_access_trap
`CSR(None, 12'h8ff)
`undef CSR

instance FShow#(CSR);
    function Fmt fshow(CSR csr);
        return (case(csr.addr)
`define CSR(n, v) v: $format(`"``csrAddr``n```");
`include "CSRs.bsvi"
`undef CSR
            default: $format("csrAddrNone");
        endcase);
    endfunction
endinstance

function CSR unpackCSR(Bit#(12) addr);
    return (case(addr)
`define CSR(n, v) v: csrAddr``n;
`include "CSRs.bsvi"
`undef CSR
        default: csrAddrNone;
    endcase);
endfunction

// values for MSPEC CSR
Bit#(2) mSpecAll    = 0; // every inst can speculate
Bit#(2) mSpecNonMem = 1; // only non-memory inst can speculate
Bit#(2) mSpecNone   = 2; // no inst can speculate

typedef enum {
    Unsupported,
    Nop,
    Amo,
    Alu,
    Ld, St, Lr, Sc,
    J, Jr, Br,
    CCall, CJAL, CJALR, Cap,
    Auipc,
    Auipcc,
    Fpu,
    Csr,
    Scr,
    Fence,
    FenceI, SFence,
    Ecall, Ebreak,
    Sret, Mret, // do not support URET
    Interrupt // we may turn an inst to an interrupt in implementation
} IType deriving(Bits, Eq, FShow);

typedef enum {
    Eq, Neq,
    Lt, Ltu, Ge, Geu,
    AT, NT
} BrFunc deriving(Bits, Eq, FShow);

typedef enum {
    Add, Addw, Sub, Subw,
    And, Or, Xor,
    Slt, Sltu, Sll, Sllw, Sra, Sraw, Srl, Srlw,
    Csrw, Csrs, Csrc
} AluFunc deriving(Bits, Eq, FShow);

typedef enum {
    SetOffset, IncOffset
} ModifyOffsetFunc deriving(Bits, Eq, FShow);

typedef enum {
    SetBounds, CRRL, CRAM
} SetBoundsFunc deriving(Bits, Eq, FShow);

typedef enum {
    Write, Set, Clear
} CSRAccessFunc deriving(Bits, Eq, FShow);

typedef union tagged {
    CSRAccessFunc TVEC;
    CSRAccessFunc EPC;
    void TCC;
    void EPCC;
    void Normal;
} SpecialRWFunc deriving(Bits, Eq, FShow);

typedef enum {
    Src1Type, Src1Addr
} AddrSource deriving(Bits, Eq, FShow);

typedef enum {
    Src1, Src2
} SrcSelector deriving(Bits, Eq, FShow);

typedef union tagged {
    ModifyOffsetFunc ModifyOffset;
    SetBoundsFunc SetBounds;
    SpecialRWFunc SpecialRW;
    AddrSource SetAddr;
    void Seal;
    void SealEntry;
    SrcSelector Unseal;
    void AndPerm;
    void SetFlags;
    void BuildCap;
    void Move;
    void ClearTag;
    void FromPtr;
} CapModifyFunc deriving(Bits, Eq, FShow);

typedef union tagged {
    void TestSubset;
    void SetEqualExact;
    void CSub;
    void GetLen;
    void GetBase;
    void GetTag;
    void GetSealed;
    void GetAddr;
    void GetOffset;
    void GetFlags;
    void GetPerm;
    void GetType;
    void ToPtr;
} CapInspectFunc deriving(Bits, Eq, FShow);

typedef enum {Mul, Mulh, Div, Rem} MulDivFunc deriving(Bits, Eq, FShow);

typedef enum {
    Signed, Unsigned, SignedUnsigned
} MulDivSign deriving(Bits, Eq, FShow);

typedef struct {
    MulDivFunc  func;
    Bool        w; // use word, i.e. 32-bit
    MulDivSign  sign;
} MulDivInst deriving(Bits, Eq, FShow);

typedef enum {
    FAdd, FSub, FMul, FDiv, FSqrt,
    FSgnj, FSgnjn, FSgnjx,
    FMin, FMax,
    FCvt_FF,
    FCvt_WF, FCvt_WUF, FCvt_LF, FCvt_LUF,
    FCvt_FW, FCvt_FWU, FCvt_FL, FCvt_FLU,
    FEq, FLt, FLe,
    FClass, FMv_XF, FMv_FX,
    FMAdd, FMSub, FNMSub, FNMAdd
} FpuFunc deriving(Bits, Eq, FShow);

typedef enum {
    Single,
    Double
} FpuPrecision deriving(Bits, Eq, FShow);

typedef struct {
    FpuFunc         func;
    RVRoundMode     rm;
    FpuPrecision    precision;
} FpuInst deriving(Bits, Eq, FShow);

// LdStInst and AmoInst are defined in Types.bsv
typedef union tagged {
    AluFunc     Alu;
    BrFunc      Br;
    MemInst     Mem;
    MulDivInst  MulDiv;
    FpuInst     Fpu;
    void        Other;
} ExecFunc deriving(Bits, Eq, FShow);

typedef union tagged {
    CapInspectFunc CapInspect;
    CapModifyFunc  CapModify;
    void           Other;
} CapFunc deriving(Bits, Eq, FShow);

// Rounding Modes (encoding by risc-v, not general fpu)
typedef struct { Bit#(3) mode; } RVRoundMode deriving(Bits, Eq);

`define RVRoundMode(n, v) RVRoundMode rm``n = RVRoundMode { mode: v };
`include "RVRoundModes.bsvi"
`undef RVRoundMode

instance FShow#(RVRoundMode);
    function Fmt fshow(RVRoundMode rm);
        return (case(rm.mode)
`define RVRoundMode(n, v) v: $format(`"``rm``n```");
`include "RVRoundModes.bsvi"
`undef RVRoundMode
            default: $format("rmUnknown");
        endcase);
    endfunction
endinstance

// bsc doesn't like the name colliding with Trap's Exception
typedef struct { Bit#(5) code; } ExceptionS deriving(Bits, Eq);
typedef ExceptionS Exception;

`define Exception(n, v) Exception exc``n = ExceptionS { code: v };
`include "Exceptions.bsvi"
`undef Exception

instance FShow#(Exception);
    function Fmt fshow(Exception exc);
        return (case(exc.code)
`define Exception(n, v) v: $format(`"``exc``n```");
`include "Exceptions.bsvi"
`undef Exception
            default: $format("excUnknown");
        endcase);
    endfunction
endinstance

// bsc doesn't like the name colliding with Trap's Interrupt and IType's
// Interrupt.
typedef struct { Bit#(4) intr; } InterruptS deriving(Bits, Eq);
typedef InterruptS Interrupt;

`define Interrupt(n, v) Interrupt intr``n = InterruptS { intr: v };
`include "Interrupts.bsvi"
`undef Interrupt

instance FShow#(Interrupt);
    function Fmt fshow(Interrupt intr);
        return (case(intr.intr)
`define Interrupt(n, v) v: $format(`"``intr``n```");
`include "Interrupts.bsvi"
`undef Interrupt
            default: $format("intrUnknown");
        endcase);
    endfunction
endinstance

`ifdef INCLUDE_GDB_CONTROL
typedef 16 InterruptNum;    // With debugger
`else
typedef 12 InterruptNum;    // Without debugger
`endif

// Traps are either an exception or an interrupt
typedef union tagged {
    CapException CapException;
    Exception Exception;
    Interrupt Interrupt;
} Trap deriving(Bits, Eq, FShow);

// privilege modes
Bit#(2) prvU = 0;
Bit#(2) prvS = 1;
Bit#(2) prvM = 3;

// VM modes
Bit#(4) vmBare = 0;
Bit#(4) vmSv39  = 9;

typedef struct {
    // for decoding floating-point instructions
    Bit#(3) frm;
    Bool fEnabled;
    // for decoding privileged instructions
    Bit#(2) prv;
    Bool trapVM; // mstatus.tvm: trap on CSRXXX inst on satp or SFENCE.VMA
                 // executed in S mode
    Bool timeoutWait; // mstatus.tw: trap on WFI after waiting N cycles in S
                      // mode. This is currently ignore since WFI is a NOP.
    Bool trapSret; // mstatus.tsr: trap on SRET executed in S mode
    // for decoding rdcycle/time/instret
    Bool cycleReadableByS; // S mode can do rdcycle
    Bool cycleReadableByU; // U mode can do rdcycle
    Bool instretReadableByS; // S mode can do rdinstret
    Bool instretReadableByU; // U mode can do rdinstret
    Bool timeReadableByS; // S mode can do rdtime
    Bool timeReadableByU; // U mode can do rdtime
} CsrDecodeInfo deriving (Bits, Eq, FShow);

typedef struct {
    Bit#(2) prv; // has taken mstatus.mprv into account
    Asid asid; // currently always 0
    Bool sv39; // VM mode: has taken prv into account, False means Bare
    Bool exeReadable; // mstatus.mxr: can load page with X=1 and R=0
    Bool userAccessibleByS; // mstatus.sum: in S mode (after considering
                            // mstatus.mprv), accessing page with U=1 will NOT
                            // fault
    Bit#(44) basePPN; // ppn of root page table
    Bit#(1) globalCapLoadGenU;
    Bit#(1) globalCapLoadGenS;
`ifdef SECURITY
    // sanctum page walk check
    Bit#(64) sanctum_evbase;
    Bit#(64) sanctum_evmask;
    Bit#(44) sanctum_ebasePPN;
    Bit#(64) sanctum_mrbm;
    Bit#(64) sanctum_emrbm;
    Bit#(64) sanctum_parbase;
    Bit#(64) sanctum_parmask;
    Bit#(64) sanctum_eparbase;
    Bit#(64) sanctum_eparmask;
    // whether an access on shared (i.e., not in my own private protation
    // domain) memory is allowed. This should not be allowed if speculation is
    // turned on.
    Bool sanctum_authShared;
`endif
} VMInfo deriving(Bits, Eq, FShow);

instance DefaultValue#(VMInfo);
    function VMInfo defaultValue = VMInfo {
        prv:  prvM,
        asid: 0,
        sv39: False,
        exeReadable: False,
        userAccessibleByS: False,
        basePPN: 0,
        globalCapLoadGenU: 0,
        globalCapLoadGenS: 0
`ifdef SECURITY
        , sanctum_evbase:   maxBound,
        sanctum_evmask:     0,
        sanctum_ebasePPN:   0,
        sanctum_mrbm:       maxBound,
        sanctum_emrbm:      0,
        sanctum_parbase:    maxBound,
        sanctum_parmask:    0,
        sanctum_eparbase:   0,
        sanctum_eparmask:   0,
        sanctum_authShared: False
`endif
    };
endinstance

typedef struct {
    Addr  pc;
    Addr  nextPc;
    IType iType;
    Bool  taken;
    Bool  mispredict;
} Redirect deriving (Bits, Eq, FShow);

typedef struct {
    CapPipe pc;
    CapPipe nextPc;
    Bool taken;
    Bool newPcc;
    Bool mispredict;
} ControlFlow deriving (Bits, Eq, FShow);

typedef struct {
    DecodedInst dInst;
    ArchRegs    regs;
    Bool        illegalInst;
} DecodeResult deriving(Bits, Eq, FShow);

typedef enum {
    Src1,
    Src2,
    Pcc,
    Ddc
} CheckAuthoritySrc deriving(Bits, Eq, FShow);

typedef enum {
    Src1Addr,
    Src1Base,
    Src1Type,
    Src2Addr,
    Vaddr    // Memory Pipe
} CheckLowSrc deriving(Bits, Eq, FShow);

typedef enum {
    Src1AddrPlus2,
    Src1Top,
    Src1Type,
    Src2Addr,
    ResultTop,
    VaddrPlusSize // Memory Pipe
} CheckHighSrc deriving(Bits, Eq, FShow);

typedef struct {
    Data authority_base;
    CapTop authority_top;
    Bit#(6) authority_idx;
    Data check_low;
    CapTop check_high;
    Bool check_inclusive;
} BoundsCheck deriving(Bits, Eq, FShow);

typedef Bit#(65) CapTop;

typedef Bit#(32) ImmData; // 32-bit decoded immediate data

typedef struct {
`define CAP_CHECK_FIELD(x,s) Bool x;
`include "CapChecks.bsvi"
`undef CAP_CHECK_FIELD

    Bool check_enable;
    CheckAuthoritySrc check_authority_src;
    CheckLowSrc check_low_src;
    CheckHighSrc check_high_src;
    Bool check_inclusive;

    Bit#(6) rn1;
    Bit#(6) rn2;
} CapChecks deriving(Bits, Eq);

instance FShow#(CapChecks);
    function Fmt fshow(CapChecks x);
        let ret = $format("CapChecks {",
            "rn1 ", fshow(x.rn1), ", rn2 ", fshow(x.rn2));

`define CAP_CHECK_FIELD(f,s) if (x.f) ret = ret + $format(", ", s);
`include "CapChecks.bsvi"
`undef CAP_CHECK_FIELD

        if (x.check_enable)
            ret = $format(ret, ", bounds check: ",
                "auth ", fshow(x.check_authority_src), ", ",
                "low ",  fshow(x.check_low_src), ", ",
                "high ", fshow(x.check_high_src), ", ",
                "inclusive ", fshow(x.check_inclusive));

        return $format(ret, "}");
    endfunction
endinstance

typedef CSR_XCapCause CapException;

typedef struct {
    IType           iType;
    ExecFunc        execFunc;
    CapFunc         capFunc;
    CapChecks       capChecks;
    Maybe#(CSR)     csr;
    Maybe#(SCR)     scr; // Special Capability Register.
    Maybe#(ImmData) imm;
} DecodedInst deriving(Bits, Eq, FShow);

function Maybe#(Data) getDInstImm(DecodedInst dInst);
    return dInst.imm matches tagged Valid .d ? Valid (signExtend(d)) : Invalid;
endfunction

typedef struct {
    CapPipe     data;
    CapMem      csrData;
    CapPipe     addr;
    ControlFlow controlFlow;
    Maybe#(CapException) capException;
    Maybe#(BoundsCheck) boundsCheck;
} ExecResult deriving(Bits, FShow);

// MMIO
typedef union tagged {
    // inst fetch: contains the maximum superscaler way to fetch
    SupWayX2Sel Inst;
    // data access
    void Ld;
    void St;
    AmoFunc Amo;
} MMIOFunc deriving(Bits, Eq, FShow);

// req fom core to platform
typedef struct {
    Addr addr; // physical address
    MMIOFunc func; // req type
    // BE, shifted for 64-bit aligned. LOAD, STORE and AMO all need to specify
    // this. We need this for to remove redundant MMIO accesses (for MSIP), and
    // to determine AMO access range (upper 32 bits, lower 32 bits, or full 64
    // bits). INST FETCH will not specify this field.
    MemDataByteEn byteEn;
    // For STORE: this is store data shifted to be 64-bit aligned
    // For AMO: this is UNshifted data (like normal mem req)
    MemTaggedData data;
    Bool loadTags;
    // Whether the request is for tags rather than data
    // For non-LOAD: always False
} MMIOCRq deriving(Bits, Eq, FShow);

// resp from platform to core
typedef struct {
    Bool valid; // if fase, then access fault
    // resp data only for LOAD or AMO req.
    // For LOAD: this is the aligned 64-bit result that contains the load
    // access range (similar to normal cache loads, i.e., the receiver needs to
    // shift the result before writting back to reg).
    // For AMO: this is the result that can be directly written into reg, i.e.,
    // for 32-bit access, the result has been shifted and sign-extended.
    MemTaggedData data;
} MMIODataPRs deriving(Bits, Eq, FShow);

typedef union tagged {
    // Resp for INST fetch. A vector entry can be invalid for two reasons: 1)
    // that entry is not requested, 2) that entry is access fault.
    Vector#(SupSizeX2, Maybe#(Instruction16)) InstFetch;
    // Resp for DATA access, i.e. LOAD, STORE and AMO
    MMIODataPRs DataAccess;
} MMIOPRs deriving(Bits, Eq, FShow);

// req from platform to core, only access MSIP or MTIP bit
typedef enum {MSIP, MTIP} MMIOPRqTarget deriving(Bits, Eq, FShow);
typedef struct {
    MMIOPRqTarget target;
    MMIOFunc func;
    // For STORE: only data[0] matters.
    // For AMO: this is unshifted data which is truncated from the data in the
    // original cRq. AMO should only access MSIP with 32-bit access range
    // because MSIP is viewed as 32-bit wide in MMIO.
    Bit#(32) data;
} MMIOPRq deriving(Bits, Eq, FShow); // req from core to platform,

// resp from core to platform
typedef struct {
    // For LOAD or AMO: this is the original MSIP value
    // For STORE: this is garbage
    Bit#(1) data;
} MMIOCRs deriving(Bits, Eq, FShow);

// Boot rom: each block is 64-bit data
typedef `LOG_BOOT_ROM_BYTES LgBootRomBytes;
typedef TSub#(LgBootRomBytes, TLog#(MemDataBytes)) LgBootRomSzData;
typedef Bit#(LgBootRomSzData) BootRomIndex;

// mtime: we increment mtime by 50 every 5000 cycles, this simulates a
// 10MHz clock for a 1GHz CPU (same as spike)
typedef /*50*/ 1 TicksPerTimeInc;
typedef /*5000*/ 100 CyclesPerTimeInc;

// Op
Bit#(3) fnADD   = 3'b000;
Bit#(3) fnSLL   = 3'b001;
Bit#(3) fnSLT   = 3'b010;
Bit#(3) fnSLTU  = 3'b011;
Bit#(3) fnXOR   = 3'b100;
Bit#(3) fnSR    = 3'b101;
Bit#(3) fnOR    = 3'b110;
Bit#(3) fnAND   = 3'b111;

Bit#(7) opALU1   = 7'b0000000;
Bit#(7) opALU2   = 7'b0100000;
Bit#(7) opMULDIV = 7'b0000001;

Bit#(3) fnMUL    = 3'b000;
Bit#(3) fnMULH   = 3'b001;
Bit#(3) fnMULHSU = 3'b010;
Bit#(3) fnMULHU  = 3'b011;
Bit#(3) fnDIV    = 3'b100;
Bit#(3) fnDIVU   = 3'b101;
Bit#(3) fnREM    = 3'b110;
Bit#(3) fnREMU   = 3'b111;

// Branch
Bit#(3) fnBEQ   = 3'b000;
Bit#(3) fnBNE   = 3'b001;
Bit#(3) fnBLT   = 3'b100;
Bit#(3) fnBGE   = 3'b101;
Bit#(3) fnBLTU  = 3'b110;
Bit#(3) fnBGEU  = 3'b111;

// Load
Bit#(3) fnLB    = 3'b000;
Bit#(3) fnLH    = 3'b001;
Bit#(3) fnLW    = 3'b010;
Bit#(3) fnLD    = 3'b011;
Bit#(3) fnLBU   = 3'b100;
Bit#(3) fnLHU   = 3'b101;
Bit#(3) fnLWU   = 3'b110;

// Store
Bit#(3) fnSB    = 3'b000;
Bit#(3) fnSH    = 3'b001;
Bit#(3) fnSW    = 3'b010;
Bit#(3) fnSD    = 3'b011;

// Amo
Bit#(5) fnLR      = 5'b00010;
Bit#(5) fnSC      = 5'b00011;
Bit#(5) fnAMOSWAP = 5'b00001;
Bit#(5) fnAMOADD  = 5'b00000;
Bit#(5) fnAMOXOR  = 5'b00100;
Bit#(5) fnAMOAND  = 5'b01100;
Bit#(5) fnAMOOR   = 5'b01000;
Bit#(5) fnAMOMIN  = 5'b10000;
Bit#(5) fnAMOMAX  = 5'b10100;
Bit#(5) fnAMOMINU = 5'b11000;
Bit#(5) fnAMOMAXU = 5'b11100;

// FPU
Bit#(2) fmtS      = 2'b00;
Bit#(2) fmtD      = 2'b01;
Bit#(5) opFADD    = 5'b00000;
Bit#(5) opFSUB    = 5'b00001;
Bit#(5) opFMUL    = 5'b00010;
Bit#(5) opFDIV    = 5'b00011;
Bit#(5) opFSQRT   = 5'b01011;
Bit#(5) opFSGNJ   = 5'b00100;
Bit#(5) opFMINMAX = 5'b00101;
Bit#(5) opFCMP    = 5'b10100;
Bit#(5) opFMV_XF  = 5'b11100; // FCLASS also
Bit#(5) opFMV_FX  = 5'b11110;
Bit#(5) opFCVT_FF = 5'b01000;
Bit#(5) opFCVT_WF = 5'b11000;
Bit#(5) opFCVT_FW = 5'b11010;

//MiscMem
Bit#(3) fnFENCE  = 3'b000;
Bit#(3) fnFENCEI = 3'b001;
Bit#(3) fnLC     = 3'b010;

// System
Bit#(3) fnPRIV   = 3'b000;
Bit#(3) fnCSRRW  = 3'b001;
Bit#(3) fnCSRRS  = 3'b010;
Bit#(3) fnCSRRC  = 3'b011;
Bit#(3) fnCSRRWI = 3'b101;
Bit#(3) fnCSRRSI = 3'b110;
Bit#(3) fnCSRRCI = 3'b111;

Bit#(12) privECALL  = 12'h000;
Bit#(12) privEBREAK = 12'h001;
Bit#(12) privURET   = 12'h002;
Bit#(12) privSRET   = 12'h102;
Bit#(12) privMRET   = 12'h302;
Bit#(12) privWFI    = 12'h105;

Bit#(7) privSFENCEVMA  = 7'h9;

function Bool isSystem(IType iType) = (
    iType == Unsupported || iType == Interrupt ||
    iType == Ecall || iType == Ebreak || iType == Csr || iType == Scr ||
    iType == SFence || iType == FenceI ||
    iType == Sret || iType == Mret
);

// instruction requires replaying (i.e. fetch next instruction after current
// instruction commits)
function Bool doReplay(IType iType) = isSystem(iType);

function Bool isFpuInst(IType iType) = (iType == Fpu);

function Bool isMemInst(IType iType) = (
    iType == Ld || iType == St || iType == Lr || iType == Sc || iType == Amo
);

function Fmt showInst(Instruction inst);
  Fmt ret = fshow("");

  Opcode opcode = unpack(inst[  6 :  0 ]);
  let rd     = inst[ 11 :  7 ];
  let funct3 = inst[ 14 : 12 ];
  let rs1    = inst[ 19 : 15 ];
  let rs2    = inst[ 24 : 20 ];
  let funct7 = inst[ 31 : 25 ];

  Bit#(32) immI   = signExtend(inst[31:20]);
  Bit#(32) immS   = signExtend({ inst[31:25], inst[11:7] });
  Bit#(32) immB   = signExtend({ inst[31], inst[7], inst[30:25], inst[11:8], 1'b0});
  Bit#(32) immU   = { inst[31:12], 12'b0 };
  Bit#(32) immJ   = signExtend({ inst[31], inst[19:12], inst[20], inst[30:25], inst[24:21], 1'b0});

  case (opcode)
    opcOpImm:
    begin
      ret = case (funct3)
        fnADD: fshow("addi");
        fnSLT: fshow("slti");
        fnSLTU: fshow("sltiu");
        fnAND: fshow("andi");
        fnOR: fshow("ori");
        fnXOR: fshow("xori");
        fnSLL: fshow("slli");
        fnSR: (immI[10] == 0 ? fshow("srli") : fshow("srai"));
      endcase;
      ret = ret + fshow(" ") + fshow(rd) + fshow(" = ") + fshow(rs1) + fshow(" ");
      ret = ret + (case (funct3)
        fnSLL, fnSR: fshow(immI[5:0]);
        default: fshow(immI);
      endcase);
    end

    opcOpImm32:
    begin
      ret = case (funct3)
        fnADD: fshow("addiw");
        fnSLL: fshow("slliw");
        fnSR: (immI[10] == 0 ? fshow("srliw") : fshow("sraiw"));
      endcase;
      ret = ret + fshow(" ") + fshow(rd) + fshow(" = ") + fshow(rs1) + fshow(" ");
      ret = ret + (case (funct3)
        fnSLL, fnSR: fshow(immI[4:0]);
        default: fshow(immI);
      endcase);
    end

    opcOp:
    begin
      ret = case (funct3)
        fnADD: (immI[10] == 0 ? fshow("add") : fshow("sub"));
        fnSLT: fshow("slt");
        fnSLTU: fshow("sltu");
        fnAND: fshow("and");
        fnOR: fshow("or");
        fnXOR: fshow("xor");
        fnSLL: fshow("sll");
        fnSR: (immI[10] == 0 ? fshow("srl") : fshow("sra"));
      endcase;
      ret = ret + fshow(" ") + fshow(rd) + fshow(" = ") + fshow(rs1) + fshow(" ") + fshow(rs2);
    end

    opcOp32:
    begin
      ret = case (funct3)
        fnADD: (immI[10] == 0 ? fshow("addw") : fshow("subw"));
        fnSLL: fshow("sllw");
        fnSR: (immI[10] == 0 ? fshow("srlw") : fshow("sraw"));
      endcase;
      ret = ret + fshow(" ") + fshow(rd) + fshow(" = ") + fshow(rs1) + fshow(" ") + fshow(rs2);
    end

    opcLui:
      ret = fshow("lui ") + fshow(rd) + fshow(" ") + fshow(immU);

    opcAuipc:
      ret = fshow("auipc ") + fshow(rd) + fshow(" ") + fshow(immU);

    opcJal:
      ret = fshow("jal ") + fshow(rd) + fshow(" ") + fshow(immJ);

    opcJalr:
      ret = fshow("jalr ") + fshow(rd) + fshow(" ") + fshow(rs1) + fshow(" ") + fshow(immI);

    opcBranch:
    begin
      ret = case(funct3)
        fnBEQ: fshow("beq");
        fnBNE: fshow("bne");
        fnBLT: fshow("blt");
        fnBLTU: fshow("bltu");
        fnBGE: fshow("bge");
        fnBGEU: fshow("bgeu");
      endcase;
      ret = ret + fshow(" ") + fshow(rs1) + fshow(" ") + fshow(rs2) + fshow(" ") + fshow(immB);
    end

    opcLoad:
    begin
      ret = case(funct3)
        fnLB: fshow("lb");
        fnLH: fshow("lh");
        fnLW: fshow("lw");
        fnLD: fshow("ld");
        fnLBU: fshow("lbu");
        fnLHU: fshow("lhu");
        fnLWU: fshow("lwu");
      endcase;
      ret = ret + fshow(" ") + fshow(rd) + fshow(" = ") + fshow(rs1) + fshow(" ") + fshow(immI);
    end

    opcStore:
    begin
      ret = case(funct3)
        fnSB: fshow("sb");
        fnSH: fshow("sh");
        fnSW: fshow("sw");
        fnSD: fshow("sd");
      endcase;
      ret = ret + fshow(" ") + fshow(rs1) + fshow(" ") + fshow(rs2) + fshow(" ") + fshow(immS);
    end

    opcMiscMem:
    begin
      ret = case (funct3)
        fnFENCE: fshow("fence");
        fnFENCEI: fshow("fence.i");
      endcase;
    end

    opcSystem:
    begin
      case (funct3)
        fnCSRRW, fnCSRRS, fnCSRRC, fnCSRRWI, fnCSRRSI, fnCSRRCI:
        begin
          ret = case(funct3)
            fnCSRRW: fshow("csrrw");
            fnCSRRC: fshow("csrrc");
            fnCSRRS: fshow("csrrs");
            fnCSRRWI: fshow("csrrwi");
            fnCSRRCI: fshow("csrrci");
            fnCSRRSI: fshow("csrrsi");
          endcase;
          ret = ret + fshow(" ") + fshow(rd) + fshow(" ") + fshow(immI) + fshow(" ") + fshow(rs1);
        end

        fnPRIV:
        begin
          ret = case (truncate(immI))
            privECALL: fshow("ecall");
            privEBREAK: fshow("ebreak");
            privURET: fshow("uret");
            privSRET: fshow("sret");
            privMRET: fshow("mret");
            privWFI: fshow("wfi");
            default: (
              funct7 == privSFENCEVMA ?
              (fshow("sfence.vma ") + fshow(rs1) + fshow(" ") + fshow(rs2)) :
              fshow("SYSTEM not implemented")
            );
          endcase;
        end

        default:
          ret = fshow("SYSTEM not implemented");
      endcase
    end
    default:
      ret = fshow("nop");
  endcase

  return ret;
endfunction

function x addPc(x cap, Bit#(12) inc) provisos (Add#(f, 12, c), CHERICap::CHERICap#(x, a, b, c, d, e)) = setAddrUnsafe(cap, getAddr(cap) + signExtend(inc));

`ifdef PERFORMANCE_MONITORING
typedef   8 Report_Width;
typedef  64 Counter_Width;
typedef  29 No_Of_Ctrs;


function Bit#(outWidth) hash(Bit#(inWidth) in)
    provisos(Add#(a__, inWidth, TMul#(TDiv#(inWidth, outWidth), outWidth)),
             Add#(1, b__, TDiv#(inWidth, outWidth)));
    Vector#(TDiv#(inWidth,outWidth), Bit#(outWidth)) vec = unpack(zeroExtend(in));
    return fold( \^ , vec);
endfunction
