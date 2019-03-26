import Vector::*;
import FIFOF::*;
import Assert::*;

export XilinxIntMulSign(..);
export XilinxIntMul(..);
export mkXilinxIntMul;

// Xilinx int multiplier IP is a rigorous pipeline with a fixed latency. There
// is no back pressure in the raw IP. We will wrap it with flow control. To do
// this, we need to know the pipeline latency from macro
// XILINX_INT_MUL_LATENCY.

typedef `XILINX_INT_MUL_LATENCY IntMulLatency;

interface IntMulImport;
    method Action req(Bit#(64) a, Bit#(64) b);
    method Bit#(128) product;
endinterface

import "BVI" int_mul_signed =
module mkIntMulSignedImport(IntMulImport);
    default_clock clk(CLK, (*unused*) unused_gate);
    default_reset no_reset;

    method req(A, B) enable((*inhigh*) EN);
    method P product;

    schedule (req) C (req);
    schedule (product) CF (req, product);
endmodule

import "BVI" int_mul_unsigned =
module mkIntMulUnsignedImport(IntMulImport);
    default_clock clk(CLK, (*unused*) unused_gate);
    default_reset no_reset;

    method req(A, B) enable((*inhigh*) EN);
    method P product;

    schedule (req) C (req);
    schedule (product) CF (req, product);
endmodule

import "BVI" int_mul_signed_unsigned =
module mkIntMulSignedUnsignedImport(IntMulImport);
    default_clock clk(CLK, (*unused*) unused_gate);
    default_reset no_reset;

    method req(A, B) enable((*inhigh*) EN);
    method P product;

    schedule (req) C (req);
    schedule (product) CF (req, product);
endmodule

// mul req type
typedef enum {
    Signed,
    Unsigned,
    SignedUnsigned
} XilinxIntMulSign deriving(Bits, Eq, FShow);

// simulation
module mkIntMulSim#(XilinxIntMulSign sign)(IntMulImport);
    RWire#(Bit#(128)) newReq <- mkRWire;
    Vector#(
        IntMulLatency, Reg#(Maybe#(Bit#(128)))
    ) pipe <- replicateM(mkReg(Invalid));

    (* fire_when_enabled, no_implicit_conditions *)
    rule canon;
        for(Integer i = 1; i < valueof(IntMulLatency); i = i+1) begin
            pipe[i] <= pipe[i - 1];
        end
        pipe[0] <= newReq.wget;
    endrule

    method Action req(Bit#(64) a, Bit#(64) b);
        Int#(128) op1 = (case(sign)
            Signed, SignedUnsigned: (unpack(signExtend(a)));
            default: (unpack(zeroExtend(a)));
        endcase);
        Int#(128) op2 = (case(sign)
            Signed: (unpack(signExtend(b)));
            default: (unpack(zeroExtend(b)));
        endcase);
        Int#(128) prod = op1 * op2;
        newReq.wset(pack(prod));
    endmethod

    method Bit#(128) product = fromMaybe(?, pipe[valueof(IntMulLatency) - 1]);
endmodule

// wrap up all mul IPs to have back pressure
interface XilinxIntMul#(type tagT);
    method Action req(Bit#(64) a, Bit#(64) b, XilinxIntMulSign sign, tagT tag);
    method Action deqResp;
    method Bool respValid;
    method Bit#(128) product;
    method tagT respTag;
endinterface

module mkXilinxIntMul(XilinxIntMul#(tagT)) provisos(
    Bits#(tagT, tagSz),
    // credit based flow control types
    NumAlias#(TAdd#(IntMulLatency, 1), maxCredit),
    Alias#(Bit#(TLog#(TAdd#(maxCredit, 1))), creditT)
);
    // different multilpliers: WaitAutoReset is not needed, since mul is a
    // pipeline with fixed latency
`ifdef BSIM
    IntMulImport mulSigned <- mkIntMulSim(Signed);
    IntMulImport mulUnsigned <- mkIntMulSim(Unsigned);
    IntMulImport mulSignedUnsigned <- mkIntMulSim(SignedUnsigned);
`else
    IntMulImport mulSigned <- mkIntMulSignedImport;
    IntMulImport mulUnsigned <- mkIntMulUnsignedImport;
    IntMulImport mulSignedUnsigned <- mkIntMulSignedUnsignedImport;
`endif

    // resp FIFO (unguarded) & flow ctrl credit
    FIFOF#(
        Tuple2#(Bit#(128), tagT)
    ) respQ <- mkUGSizedFIFOF(valueof(maxCredit));
    Reg#(creditT) credit <- mkReg(fromInteger(valueof(maxCredit)));

    // shift regs for sign + tag
    Vector#(
        IntMulLatency,
        Reg#(Maybe#(Tuple2#(XilinxIntMulSign, tagT)))
    ) pipe <- replicateM(mkReg(Invalid));

    // wire to catch input req
    RWire#(Tuple2#(XilinxIntMulSign, tagT)) newReq <- mkRWire;

    // wire to catch deq
    PulseWire deqEn <- mkPulseWire;

    (* fire_when_enabled, no_implicit_conditions *)
    rule canon;
        creditT nextCredit = credit;
        // incr credit if resp FIFO is deq
        if(deqEn) begin
            if(nextCredit >= fromInteger(valueof(maxCredit))) begin
                $fdisplay(stderr, "\n%m: ASSERT FAIL!!");
                dynamicAssert(False, "credit overflow");
            end
            nextCredit = nextCredit + 1;
        end
        // enq resp FIFO if something is outputed from mul
        if (
            pipe[valueof(IntMulLatency) - 1] matches tagged Valid {.sign, .tag}
        ) begin
            Bit#(128) prod = (case(sign)
                Signed: (mulSigned.product);
                Unsigned: (mulUnsigned.product);
                SignedUnsigned: (mulSignedUnsigned.product);
                default: (?);
            endcase);
            respQ.enq(tuple2(prod, tag));
        end
        // shift pipe regs
        for(Integer i = 1; i < valueof(IntMulLatency); i = i+1) begin
            pipe[i] <= pipe[i - 1];
        end
        pipe[0] <= newReq.wget;
        // decr credit if new req is taken
        if(isValid(newReq.wget)) begin
            if(nextCredit == 0) begin
                $fdisplay(stderr, "\n%m: ASSERT FAIL!!");
                dynamicAssert(False, "credit underflow");
            end
            nextCredit = nextCredit - 1;
        end
        // update credit
        credit <= nextCredit;
    endrule

    method Action req(Bit#(64) a, Bit#(64) b,
                      XilinxIntMulSign sign, tagT tag) if(credit > 0);
        case(sign)
            Signed: mulSigned.req(a, b);
            Unsigned: mulUnsigned.req(a, b);
            SignedUnsigned: mulSignedUnsigned.req(a, b);
        endcase
        newReq.wset(tuple2(sign, tag)); // notify new req
    endmethod

    method Action deqResp if(respQ.notEmpty);
        respQ.deq;
        deqEn.send; // notify deq resp
    endmethod

    method respValid = respQ.notEmpty;

    method Bit#(128) product if(respQ.notEmpty);
        return tpl_1(respQ.first);
    endmethod

    method tagT respTag if(respQ.notEmpty);
        return tpl_2(respQ.first);
    endmethod
endmodule

