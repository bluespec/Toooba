
module fp_div_sim(
    input [63:0] A,
    input [63:0] B,
    output [63:0] RES
);
    assign RES = $realtobits($bitstoreal(A) / $bitstoreal(B));
endmodule
