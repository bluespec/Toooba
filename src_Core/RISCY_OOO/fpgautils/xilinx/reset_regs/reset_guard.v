
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

// This module outputs a 1-bit signal
// The signal is initially 0 after programming the FPGA
// XXX: everything should be inited to 0 after programming

// When a reset arrives, the module starts counting
// In N cycles after the reset, the signal becomes 1 
// This signal can be used as a guard for sync fifo operations

`ifdef BSV_POSITIVE_RESET
  `define BSV_RESET_VALUE 1'b1
  `define BSV_RESET_EDGE posedge
`else
  `define BSV_RESET_VALUE 1'b0
  `define BSV_RESET_EDGE negedge
`endif

module reset_guard(
    input CLK,
    input RST,
    output IS_READY
);
    reg ready = 0;
    reg rst_done = 0;

    always@(posedge CLK) begin
        if(RST == `BSV_RESET_VALUE) begin
            ready <= 0;
            rst_done <= 1;
            // synopsys translate_off
            if(!rst_done) begin
                $display("[reset_guard] %t %m reset happen", $time);
            end
            // synopsys translate_on
        end
        else if(rst_done) begin
            ready <= 1;
            // synopsys translate_off
            if(!ready) begin
                $display("[reset_guard] %t %m guard ready", $time);
            end
            // synopsys translate_on
        end
    end

    assign IS_READY = ready;
endmodule
