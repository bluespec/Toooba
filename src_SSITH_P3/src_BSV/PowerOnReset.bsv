package PowerOnReset;

import Clocks::*;

UInt#(8) resetCycles = 10;

import "BVI" RegUNInit =
module mkRegUNInit#(parameter a init) (Reg#(a))
   provisos (Bits#(a,sa));

   parameter width = valueOf(sa);
   parameter init  = pack(init);

   default_clock   clk(CLK, (*unused*)CLK_GATE);
   no_reset;

   method Q_OUT _read();
   method _write(D_IN) enable(EN);

   schedule _read  CF  _read;
   schedule _write SBR _write;
   schedule _read  SB  _write;
endmodule



import "BVI" ASSIGN1 =
module mkBoolToReset#(Bool xin) (ResetGenIfc);
   default_clock ();
   no_reset;

   port IN = xin;
   output_reset gen_rst(OUT);
endmodule

(*synthesize*)
module mkPowerOnReset (ResetGenIfc);
   Reg#(UInt#(8)) ctr <- mkRegUNInit(resetCycles);
   Reg#(Bool) isInPowerOnReset <- mkRegUNInit(True);

   rule countDown(isInPowerOnReset);
      let n = ctr - 1;
      ctr <= n;
      if (n==0) isInPowerOnReset <=  False;
   endrule

   let rst_ifc <- mkBoolToReset(!isInPowerOnReset);
   return rst_ifc;
endmodule

endpackage
