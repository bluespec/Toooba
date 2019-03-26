import FIFOF::*;
import FIFO::*;

import WaitAutoReset::*;

export XilinxIntDiv(..);
export mkXilinxIntDiv;

// import Xilinx IP core for unsigned division

// axi tuser: user tag + info needed to handle divide by 0 + sign info
typedef struct {
    Bool divByZero;
    Bit#(64) divByZeroRem; // remainder in case of divide by zero
    Bool signedDiv; // signed division (so dividend/divisor has been abs)
    Bit#(1) quotientSign; // sign bit of quotient (in case of signedDiv)
    Bit#(1) remainderSign; // sign bit of remainder (in case of signedDiv)
    Bit#(8) tag;
} IntDivUser deriving(Bits, Eq, FShow);

interface IntDivUnsignedImport;
    method Action enqDividend(Bit#(64) dividend, IntDivUser user);
    method Action enqDivisor(Bit#(64) divisor);
    method Action deqResp;
    method Bool respValid;
    method Bit#(128) quotient_remainder;
    method IntDivUser respUser;
endinterface

import "BVI" int_div_unsigned =
module mkIntDivUnsignedImport(IntDivUnsignedImport);
    default_clock clk(aclk, (*unused*) unused_gate);
    default_reset no_reset;

    method enqDividend(
        s_axis_dividend_tdata, s_axis_dividend_tuser
    ) enable(s_axis_dividend_tvalid) ready(s_axis_dividend_tready);

    method enqDivisor(
        s_axis_divisor_tdata
    ) enable(s_axis_divisor_tvalid) ready(s_axis_divisor_tready);

    method deqResp() enable(m_axis_dout_tready) ready(m_axis_dout_tvalid);
    method m_axis_dout_tvalid respValid;
    method m_axis_dout_tdata quotient_remainder ready(m_axis_dout_tvalid);
    method m_axis_dout_tuser respUser ready(m_axis_dout_tvalid);

    schedule (enqDividend) C (enqDividend);
    schedule (enqDivisor) C (enqDivisor);
    schedule (deqResp) C (deqResp);
    schedule (enqDividend) CF (enqDivisor, deqResp);
    schedule (enqDivisor) CF (deqResp);
    schedule (
        respValid,
        quotient_remainder,
        respUser
    ) CF (
        respValid,
        quotient_remainder,
        respUser,
        enqDividend,
        enqDivisor,
        deqResp
    );
endmodule

// simulation
module mkIntDivUnsignedSim(IntDivUnsignedImport);
    FIFO#(Tuple2#(Bit#(64), IntDivUser)) dividendQ <- mkFIFO;
    FIFO#(Bit#(64)) divisorQ <- mkFIFO;
    FIFOF#(Tuple2#(Bit#(128), IntDivUser)) respQ <- mkSizedFIFOF(2);

    rule compute;
        dividendQ.deq;
        divisorQ.deq;
        let {dividend, user} = dividendQ.first;
        let divisor = divisorQ.first;

        UInt#(64) a = unpack(dividend);
        UInt#(64) b = unpack(divisor);
        Bit#(64) q = pack(a / b);
        Bit#(64) r = pack(a % b);
        respQ.enq(tuple2({q, r}, user));
    endrule

    method Action enqDividend(Bit#(64) dividend, IntDivUser user);
        dividendQ.enq(tuple2(dividend, user));
    endmethod

    method Action enqDivisor(Bit#(64) divisor);
        divisorQ.enq(divisor);
    endmethod

    method Action deqResp;
        respQ.deq;
    endmethod

    method respValid = respQ.notEmpty;

    method quotient_remainder = tpl_1(respQ.first);

    method respUser = tpl_2(respQ.first);
endmodule


// Wrapper for user (add reset guard, check overflow/divided by 0).  We cannot
// unify two dividers to one, because divider latency may not be a constant.
interface XilinxIntDiv#(type tagT);
    method Action req(Bit#(64) dividend, Bit#(64) divisor, Bool signedDiv, tagT tag);
    // response
    method Action deqResp;
    method Bool respValid;
    method Bit#(64) quotient;
    method Bit#(64) remainder;
    method tagT respTag;
endinterface

module mkXilinxIntDiv(XilinxIntDiv#(tagT)) provisos (
    Bits#(tagT, tagSz), Add#(tagSz, a__, 8)
);
`ifdef BSIM
    IntDivUnsignedImport divIfc <- mkIntDivUnsignedSim;
`else
    IntDivUnsignedImport divIfc <- mkIntDivUnsignedImport;
`endif
    WaitAutoReset#(4) init <- mkWaitAutoReset;

    method Action req(
        Bit#(64) dividend, Bit#(64) divisor, Bool signedDiv, tagT tag
    ) if(init.isReady);
        // compute the input ops to div unsigned IP
        Bit#(1) dividend_sign = truncateLSB(dividend);
        Bit#(1) divisor_sign = truncateLSB(divisor);
        Bit#(64) a = dividend;
        Bit#(64) b = divisor;
        if(signedDiv) begin
            if(dividend_sign == 1) begin
                a = 0 - dividend;
            end
            if(divisor_sign == 1) begin
                b = 0 - divisor;
            end
        end
        // get the user struct (sign/divide by 0)
        let user = IntDivUser {
            divByZero: divisor == 0,
            divByZeroRem: dividend,
            signedDiv: signedDiv,
            // quotient negative when dividend and divisor have different signs
            quotientSign: dividend_sign ^ divisor_sign,
            // remainder sign follows that of dividend
            remainderSign: dividend_sign,
            tag: zeroExtend(pack(tag))
        };
        divIfc.enqDividend(a, user);
        divIfc.enqDivisor(b);
    endmethod

    // we also put reset guard on deq port to prevent random signals before
    // reset from dequing or corrupting axi states
    method Action deqResp if(init.isReady);
        divIfc.deqResp;
    endmethod

    method respValid = divIfc.respValid && init.isReady;
    
    method Bit#(64) quotient if(init.isReady);
        let user = divIfc.respUser;
        Bit#(64) q;
        if(user.divByZero) begin
            q = maxBound;
        end
        else begin
            q = truncateLSB(divIfc.quotient_remainder);
            if(user.signedDiv && user.quotientSign == 1) begin
                q = 0 - q;
            end
            // signed overflow is automatically handled
        end
        return q;
    endmethod
    
    method Bit#(64) remainder if(init.isReady);
        let user = divIfc.respUser;
        Bit#(64) r;
        if(user.divByZero) begin
            r = user.divByZeroRem;
        end
        else begin
            r = truncate(divIfc.quotient_remainder);
            if(user.signedDiv && user.remainderSign == 1) begin
                r = 0 - r;
            end
            // signed overflow is automatically handled
        end
        return r;
    endmethod 

    method tagT respTag if(init.isReady);
        return unpack(truncate(divIfc.respUser.tag));
    endmethod
endmodule

