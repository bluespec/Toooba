
module fp_fma_sim(
    input [63:0] A,
    input [63:0] B,
    input [63:0] C,
    output [63:0] RES
);
    assign RES = $realtobits($bitstoreal(A) * $bitstoreal(B) + $bitstoreal(C));
endmodule
