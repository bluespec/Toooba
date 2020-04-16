
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

import Types::*;
import ProcTypes::*;
import Vector::*;
import CHERICap::*;
import CHERICC_Fat::*;
import ISA_Decls_CHERI::*;

(* noinline *)
function Maybe#(CapException) capChecks(CapPipe a, CapPipe b, CapChecks toCheck);
    // TODO plumb register indices here
    Maybe#(CapException) result = Invalid;
    if      (toCheck.src1_tag                 && !isValidCap(a))
        result =  Valid (CapException {cheri_exc_code: TagViolation});
    else if (toCheck.src2_tag                 && !isValidCap(b))
        result =  Valid (CapException {cheri_exc_code: TagViolation});
    else if (toCheck.src1_sealed_with_type    && getKind(a) != SEALED_WITH_TYPE)
        result =  Valid (CapException {cheri_exc_code: SealViolation});
    else if (toCheck.src1_unsealed            && isValidCap(a) && isSealed(a))
        result =  Valid (CapException {cheri_exc_code: SealViolation});
    else if (toCheck.src2_unsealed            && isValidCap(b) && isSealed(b))
        result =  Valid (CapException {cheri_exc_code: SealViolation});
    else if (toCheck.src1_sealed              && isValidCap(a) && !isSealed(a))
        result =  Valid (CapException {cheri_exc_code: SealViolation});
    else if (toCheck.src2_sealed              && isValidCap(b) && !isSealed(b))
        result =  Valid (CapException {cheri_exc_code: SealViolation});
    else if (toCheck.src1_src2_types_match    && getType(a) != getType(b))
        result =  Valid (CapException {cheri_exc_code: TypeViolation});
    else if (toCheck.src1_permit_ccall        && !getHardPerms(a).permitCCall)
        result =  Valid (CapException {cheri_exc_code: PermitCCallViolation});
    else if (toCheck.src2_permit_ccall        && !getHardPerms(b).permitCCall)
        result =  Valid (CapException {cheri_exc_code: PermitCCallViolation});
    else if (toCheck.src1_permit_x            && !getHardPerms(a).permitExecute)
        result =  Valid (CapException {cheri_exc_code: PermitXViolation});
    else if (toCheck.src2_no_permit_x         && getHardPerms(b).permitExecute)
        result =  Valid (CapException {cheri_exc_code: PermitXViolation});
    else if (toCheck.src2_permit_unseal       && !getHardPerms(b).permitUnseal)
        result =  Valid (CapException {cheri_exc_code: PermitUnsealViolation});
    else if (toCheck.src2_permit_seal         && !getHardPerms(b).permitSeal)
        result =  Valid (CapException {cheri_exc_code: PermitSealViolation});
    else if (toCheck.src2_points_to_src1_type && getAddr(b) != zeroExtend(getType(a)))
        result =  Valid (CapException {cheri_exc_code: TypeViolation});
    else if (toCheck.src2_addr_valid_type     && !validAsType(b, truncate(getAddr(b))))
        result =  Valid (CapException {cheri_exc_code: LengthViolation});
    else if (toCheck.src2_perm_subset_src1    && (getPerms(a) & getPerms(b)) != getPerms(b))
        result =  Valid (CapException {cheri_exc_code: SoftwarePermViolation});
    return result;
endfunction

(* noinline *)
function Data alu(Data a, Data b, AluFunc func);
    Data res = (case(func)
            Add     : (a + b);
            Addw    : signExtend((a + b)[31:0]);
            Sub     : (a - b);
            Subw    : signExtend((a[31:0] - b[31:0])[31:0]);
            And     : (a & b);
            Or      : (a | b);
            Xor     : (a ^ b);
            Slt     : zeroExtend( pack( signedLT(a, b) ) );
            Sltu    : zeroExtend( pack( a < b ) );
            Sll     : (a << b[5:0]);
            Sllw    : signExtend((a[31:0] << b[4:0])[31:0]);
            Srl     : (a >> b[5:0]);
            Sra     : signedShiftRight(a, b[5:0]);
            Srlw    : signExtend((a[31:0] >> b[4:0])[31:0]);
            Sraw    : signExtend(signedShiftRight(a[31:0], b[4:0])[31:0]);
            Csrw    : b;
            Csrs    : (a | b); // same as Or
            Csrc    : (a & ~b);
            default : 0;
        endcase);
    return res;
endfunction

(* noinline *)
function CapPipe capModify(CapPipe a, CapPipe b, CapModifyFunc func);
    CapPipe res = (case(func) matches
            tagged ModifyOffset .offsetOp :
                modifyOffset(a, getAddr(b), offsetOp == IncOffset).value;
            tagged SetBounds .exact       :
                setBounds(a, getAddr(b)).value;
            //tagged SpecialRW              :
            //    error("SpecialRW not yet implemented");
            tagged SetAddr .addrSource    :
                if (addrSource == Src2Type && !isSealed(b)) return nullWithAddr(-1);
                else return setAddr(a, (addrSource == Src2Type) ? zeroExtend(getType(b)) : getAddr(b) ).value;
            tagged Seal                   :
                setType(a, truncate(getAddr(b)));
            tagged Unseal .src            :
                setType(((src == Src1) ? a:b), -1);
            tagged AndPerm                :
                setPerms(a, pack(getPerms(a)) & truncate(getAddr(b)));
            tagged SetFlags               :
                setFlags(a, truncate(getAddr(b)));
            //tagged FromPtr                :
            //     error("FromPtr not yet implemented");
            tagged BuildCap               :
                setValidCap(a, True);
            tagged Move                   :
                a;
            tagged ClearTag               :
                setValidCap(a, False);
            //tagged CJALR                  :
            //    error("CJALR not yet implemented");
            default: ?;
        endcase);
    return res;
endfunction

(* noinline *)
function Data capInspect(CapPipe a, CapPipe b, CapInspectFunc func);
    Data res = (case(func) matches
               //tagged TestSubset             :
               //   error("TestSubset not yet implemented");
               tagged GetLen                 :
                   truncate(getLength(a));
               tagged GetBase                :
                   getBase(a);
               tagged GetTag                 :
                   zeroExtend(pack(isValidCap(a)));
               tagged GetSealed              :
                   zeroExtend(pack(isSealed(a)));
               tagged GetAddr                :
                   getAddr(a);
               tagged GetOffset              :
                   getOffset(a);
               tagged GetFlags               :
                   zeroExtend(getFlags(a));
               tagged GetPerm                :
                   zeroExtend(getPerms(a));
               tagged GetType                :
                   signExtend(getType(a));
               tagged ToPtr                  :
                   (getAddr(a) - getBase(b));
               default: ?;
        endcase);
    return res;
endfunction

(* noinline *)
function Bool aluBr(Data a, Data b, BrFunc brFunc);
    Bool brTaken = (case(brFunc)
            Eq      : (a == b);
            Neq     : (a != b);
            Lt      : signedLT(a, b);
            Ltu     : (a < b);
            Ge      : signedGE(a, b);
            Geu     : (a >= b);
            AT      : True;
            NT      : False;
            default : False;
        endcase);
    return brTaken;
endfunction

(* noinline *)
function Addr brAddrCalc(Addr pc, Data val, IType iType, Data imm, Bool taken, Bit #(32) orig_inst);
    Addr fallthrough_incr = ((orig_inst [1:0] == 2'b11) ? 4 : 2);
    Addr pcPlusN = pc + fallthrough_incr;
    Addr targetAddr = (case (iType)
            J       : (pc + imm);
            Jr      : {(val + imm)[valueOf(AddrSz)-1:1], 1'b0};
            Br      : (taken? pc + imm : pcPlusN);
            default : pcPlusN;
        endcase);
    return targetAddr;
endfunction

(* noinline *)
function ControlFlow getControlFlow(DecodedInst dInst, Data rVal1, Data rVal2, Addr pc, Addr ppc, Bit #(32) orig_inst);
    ControlFlow cf = unpack(0);

    Bool taken = dInst.execFunc matches tagged Br .br_f ? aluBr(rVal1, rVal2, br_f) : False;
    Addr nextPc = brAddrCalc(pc, rVal1, dInst.iType, validValue(getDInstImm(dInst)), taken, orig_inst);
    Bool mispredict = nextPc != ppc;

    cf.pc = pc;
    cf.nextPc = nextPc;
    cf.taken = taken;
    cf.mispredict = mispredict;

    return cf;
endfunction

(* noinline *)
function ExecResult basicExec(DecodedInst dInst, CapPipe rVal1, CapPipe rVal2, CapPipe pcc, Addr ppc, Bit #(32) orig_inst);
    // just data, addr, and control flow
    Addr pc = getAddr(pcc);
    CapPipe data = nullCap;
    Data csr_data = 0;
    CapPipe addr = nullCap;
    Bool cjalr = False;
    Bool ccall = False;
    if (dInst.iType == Jr) begin
        if (dInst.capChecks.src1_src2_types_match) ccall = True;
        else if (dInst.capChecks.src1_tag) cjalr = True;
    end

    ControlFlow cf = ControlFlow{pc: pc, nextPc: 0, taken: False, newPcc: cjalr, mispredict: False};

    CapPipe aluVal2 = rVal2;
    if (getDInstImm(dInst) matches tagged Valid .imm) aluVal2 = nullWithAddr(imm); //isValid(dInst.imm) ? fromMaybe(?, dInst.imm) : rVal2;
    // Get the alu function. By default, it adds. This is used by memory instructions
    AluFunc alu_f = dInst.execFunc matches tagged Alu .alu_f ? alu_f : Add;
    Data alu_result = alu(getAddr(rVal1), getAddr(aluVal2), alu_f);

    Data inspect_result = capInspect(rVal1, aluVal2, dInst.execFunc.CapInspect);
    CapModifyFunc modFunc = ccall ? (Unseal (Src2)):dInst.execFunc.CapModify;
    CapPipe modify_result = capModify(rVal1, aluVal2, modFunc);
    Maybe#(CapException) capException = capChecks(rVal1, aluVal2, dInst.capChecks); // TODO use this to throw exceptions

    CapPipe cap_alu_result = case (dInst.execFunc) matches tagged CapInspect .x: nullWithAddr(inspect_result);
                                                           tagged CapModify .x: modify_result;
                                                           tagged Br .x: modify_result;
                                                           default: nullWithAddr(alu_result);
                             endcase;

    // Default branch function is not taken
    BrFunc br_f = dInst.execFunc matches tagged Br .br_f ? br_f : NT;
    cf.taken = aluBr(getAddr(rVal1), getAddr(rVal2), br_f);
    cf.nextPc = brAddrCalc(pc, getAddr(rVal1), dInst.iType, fromMaybe(0,getDInstImm(dInst)), cf.taken, orig_inst);
    cf.mispredict = cf.nextPc != ppc;

    Addr fallthrough_incr = ((orig_inst [1:0] == 2'b11) ? 4 : 2);
    CapPipe link_pcc = setAddrUnsafe(pcc, getAddr(pcc) + fallthrough_incr);

    data = (case (dInst.iType) matches
            St          : rVal2;
            Sc          : rVal2;
            Amo         : rVal2;
            J           : nullWithAddr(getAddr(link_pcc));
            Jr &&& (ccall): cap_alu_result; // Depending on defaults falling through!
            Jr &&& (cjalr): link_pcc;
            Jr          : nullWithAddr(getAddr(link_pcc));
            Auipc       : nullWithAddr(pc + fromMaybe(?, getDInstImm(dInst))); // could be computed with alu
            Csr         : rVal1;
            default     : cap_alu_result;
        endcase);
    csr_data = alu_result;
    addr = (case (dInst.iType)
            Ld, St, Lr, Sc, Amo : nullWithAddr(alu_result);
            default             : nullWithAddr(cf.nextPc);
        endcase);
    CapPipe scr_data = rVal1;

    return ExecResult{data: data, csrData: csr_data, scrData: scr_data, addr: addr, controlFlow: cf, capException: capException};
endfunction

(* noinline *)
function Maybe#(Exception) checkForException(
    DecodedInst dInst,
    ArchRegs regs,
    CsrDecodeInfo csrState
); // regs needed to check if x0 is a src
    Maybe#(Exception) exception = Invalid;
    let prv = csrState.prv;

    if(dInst.iType == Ecall) begin
        exception = Valid (case(prv)
            prvU: EnvCallU;
            prvS: EnvCallS;
            prvM: EnvCallM;
            default: IllegalInst;
        endcase);
    end
    else if(dInst.iType == Ebreak) begin
        exception = Valid (Breakpoint);
    end
    else if(dInst.iType == Mret) begin
        if(prv < prvM) begin
            exception = Valid (IllegalInst);
        end
    end
    else if(dInst.iType == Sret) begin
        if(prv < prvS) begin
            exception = Valid (IllegalInst);
        end
        else if(prv == prvS && csrState.trapSret) begin
            exception = Valid (IllegalInst);
        end
    end
    else if(dInst.iType == SFence) begin
        if(prv == prvS && csrState.trapVM) begin
            exception = Valid (IllegalInst);
        end
    end
    else if(dInst.iType == Csr) begin
        let csr = pack(fromMaybe(?, dInst.csr));
        Bool csr_has_priv = (prv >= csr[9:8]);
        if(!csr_has_priv) begin
            exception = Valid (IllegalInst);
        end
        else if(prv == prvS && csrState.trapVM &&
                validValue(dInst.csr) == CSRsatp) begin
            exception = Valid (IllegalInst);
        end
        // TODO check permission for accessing cycle/inst/time, and check
        // read-only CSRs being written
    end
    else if(dInst.iType == Fpu) begin
        if(dInst.execFunc matches tagged Fpu .fpu_f) begin
            // Get rounding mode
            let rm = (fpu_f.rm == RDyn) ? unpack(csrState.frm) : fpu_f.rm;
            case(rm)
                RNE, RTZ, RDN, RUP, RMM: exception = exception; // legal modes
                default                : exception = Valid (IllegalInst);
            endcase
        end
        else begin
            // Fpu instruction without FPU execFunc
            exception = Valid (IllegalInst);
        end
    end

    return exception;
endfunction

// check mem access misaligned: byteEn is unshifted (just from Decode)
function Bool memAddrMisaligned(Addr addr, ByteEn byteEn);
    if(byteEn[7]) begin
        return addr[2:0] != 0;
    end
    else if(byteEn[3]) begin
        return addr[1:0] != 0;
    end
    else if(byteEn[1]) begin
        return addr[0] != 0;
    end
    else begin
        return False;
    end
endfunction

function Data gatherLoad(Addr addr, ByteEn byteEn, Bool unsignedLd, Data data);
    function extend = unsignedLd ? zeroExtend : signExtend;
    Bit#(IndxShamt) offset = truncate(addr);

    if(byteEn[7]) begin
        return extend(data);
    end else if(byteEn[3]) begin
        Vector#(2, Bit#(32)) dataVec = unpack(data);
        return extend(dataVec[offset[2]]);
    end else if(byteEn[1]) begin
        Vector#(4, Bit#(16)) dataVec = unpack(data);
        return extend(dataVec[offset[2:1]]);
    end else begin
        Vector#(8, Bit#(8)) dataVec = unpack(data);
        return extend(dataVec[offset]);
    end
endfunction

function Tuple2#(ByteEn, Data) scatterStore(Addr addr, ByteEn byteEn, Data data);
    Bit#(IndxShamt) offset = truncate(addr);
    if(byteEn[7]) begin
        return tuple2(byteEn, data);
    end else if(byteEn[3]) begin
        return tuple2(unpack(pack(byteEn) << (offset)), data << {(offset), 3'b0});
    end else if(byteEn[1]) begin
        return tuple2(unpack(pack(byteEn) << (offset)), data << {(offset), 3'b0});
    end else begin
        return tuple2(unpack(pack(byteEn) << (offset)), data << {(offset), 3'b0});
    end
endfunction
