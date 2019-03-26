
module fp_sqrt_sim(
    input [63:0] A,
    output [63:0] RES
);
    assign RES = $realtobits($sqrt($bitstoreal(A)));
endmodule
