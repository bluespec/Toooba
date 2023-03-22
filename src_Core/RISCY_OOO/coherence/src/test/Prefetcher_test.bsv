import Prefetcher::*;
import StmtFSM::*;
import Types::*;
import Fifos::*;
module mkTargetTableTest(Empty);
    TargetTable#(64, 16) t <- mkTargetTable;
    mkAutoFSM(
        seq
            // ----- Send misses and stuff to one window -----
            action
                t.set('h8000, 'h800a); // goes in short table
            endaction
            action
                t.set('h8000, 'h80008000); // goes in long table
            endaction
            action
                let x <- t.getAndRemove('h8000); // comes from short table
                doAssert(x == Valid('h800a), "test fail!");
            endaction
            action
                let x <- t.getAndRemove('h8000); // comes from long table
                doAssert(x == Valid('h80008000), "test fail!");
            endaction
            action
                t.set('h80000000, 'h21230000); // goes in long table
            endaction
            action
                let x <- t.getAndRemove('h80000000); // comes from long table
                doAssert(x == Valid('h21230000), "test fail!");
            endaction
            action
                t.set('h7000, 'h6fde); // goes in short table backwards
            endaction
            action
                let x <- t.getAndRemove('h7000); // get from short table
                doAssert(x == Valid('h6fde), "test fail!");
            endaction
            action
                let x <- t.getAndRemove('h7000); // get from short table
                doAssert(x == Invalid, "test fail!"); //entry was removed!
            endaction
        endseq
    );
endmodule

module mkTargetTableBRAMTest(Empty);
    TargetTableBRAM#(64, 16) t <- mkTargetTableBRAM;
    mkAutoFSM(
        seq
            // ----- Send misses and stuff to one window -----
            action
                t.writeReq('h8000, 'h800a); // goes in short table
            endaction
            action
                t.writeReq('h8000, 'h80008000); // goes in long table
            endaction
            action
                t.readReq('h8000); // comes from short table
            endaction
            action
                let x <- t.readResp(True); // comes from short table
                doAssert(x == Valid('h800a), "test fail!");
            endaction
            action
                t.readReq('h8000); // comes from long table
            endaction
            action
                let x <- t.readResp(True); // comes from long table
                doAssert(x == Valid('h80008000), "test fail!");
            endaction
            action
                t.writeReq('h80000000, 'h21230000); // goes in long table
            endaction
            action
                t.readReq('h80000000); // comes from long table
            endaction
            action
                let x <- t.readResp(True); // comes from long table
                doAssert(x == Valid('h21230000), "test fail!");
            endaction
            action
                t.writeReq('h7000, 'h6fde); // goes in short table backwards
            endaction
            action
                t.readReq('h7000); // get from short table
            endaction
            action
                let x <- t.readResp(True); // get from short table
                doAssert(x == Valid('h6fde), "test fail!");
            endaction
            action
                t.readReq('h7000); // get from short table
            endaction
            action
                let x <- t.readResp(True); // get from short table
                doAssert(x == Invalid, "test fail!"); //entry was removed!
            endaction
        endseq
    );
endmodule

module mkBRAMMultiWindowTargetPrefetcherTest(Empty);
    let p <- mkBRAMMultiWindowTargetPrefetcher;
    mkAutoFSM(
        seq
            // ----- Send misses and stuff to one window -----
            action
                p.reportAccess('h80000040, MISS);
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h80000080, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h800000c0, "test fail!");
            endaction
            action
                p.reportAccess('h800000c0, HIT); //Report hit inside window
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h80000100, "test fail!");
            endaction
            action
                p.reportAccess('h80004000, HIT); //Report hit outside window
            endaction
            action
                let x <- p.getNextPrefetchAddr; //Previous window still recommended
                doAssert(x == 'h80000140, "test fail!");
            endaction
            action
                p.reportAccess('h80000140, MISS); //Report miss inside window
            endaction
            action
                let x <- p.getNextPrefetchAddr; //Previous window still recommended
                doAssert(x == 'h80000180, "test fail!");
            endaction

            // -----  Allocate other windows ----- 
            action
                p.reportAccess('h70000000, MISS); //Report miss outside window
            endaction
            action
                let x <- p.getNextPrefetchAddr; //new window recommended
                doAssert(x == 'h70000040, "test fail!");
            endaction
            action
                p.reportAccess('h90000000, MISS); //Report miss outside window
            endaction
            action
                let x <- p.getNextPrefetchAddr; //new window recommended
                doAssert(x == 'h90000040, "test fail!");
            endaction
            action
                p.reportAccess('h60000000, MISS); //Report miss outside window
            endaction
            action
                let x <- p.getNextPrefetchAddr; //new window recommended
                doAssert(x == 'h60000040, "test fail!");
            endaction
            action
                p.reportAccess('h80000180, HIT); //Report hit inside oldest window
            endaction
            action
                let x <- p.getNextPrefetchAddr; //oldest window recommended
                doAssert(x == 'h800001c0, "test fail!");
            endaction

            // ----- Trigger window deletion -----
            action
                p.reportAccess('h50000000, MISS); //Report miss outside window,
                //discard window with 'h70..
            endaction
            action
                let x <- p.getNextPrefetchAddr; //new window recommended
                doAssert(x == 'h50000040, "test fail!");
            endaction
            action
                p.reportAccess('h70000040, HIT); //Report hit inside now deleted window
            endaction
            action
                let x <- p.getNextPrefetchAddr; //most recent window still recommended
                doAssert(x == 'h50000080, "test fail!");
            endaction

            // ----- Reorder some more windows around
            action
                p.reportAccess('h800001c0, MISS); //Report hit inside now deleted window
            endaction
            action
                let x <- p.getNextPrefetchAddr; 
                doAssert(x == 'h80000200, "test fail!");
            endaction

            // ------ Test saving and prefetching target clines
            action
                p.reportAccess('h81000000, MISS); //Report miss somewhere far away
            endaction
            action
                let x <- p.getNextPrefetchAddr; //New window allocated and recommended
                doAssert(x == 'h81000040, "test fail!");
            endaction
            action
                p.reportAccess('h80000180, MISS); //Report miss back home
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h800001c0, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr; //target address recommended
                doAssert(x == 'h50000000, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr; //target address recommended
                doAssert(x == 'h81000000, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr; 
                doAssert(x == 'h80000200, "test fail!"); // window addresss recommended
            endaction
        endseq
    );
endmodule

module mkBRAMSingleWindowTargetPrefetcherTest(Empty);
    let p <- mkBRAMSingleWindowTargetPrefetcher;
    mkAutoFSM(
        seq
            // ----- Send misses and stuff to one window -----
            action
                p.reportAccess('h80000040, MISS);
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h80000080, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h800000c0, "test fail!");
            endaction
            action
                p.reportAccess('h800000c0, HIT); //Report hit inside window
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h80000100, "test fail!");
            endaction
            action
                p.reportAccess('h80004000, HIT); //Report hit outside window
            endaction
            action
                let x <- p.getNextPrefetchAddr; //Previous window still recommended
                doAssert(x == 'h80000140, "test fail!");
            endaction
            action
                p.reportAccess('h80000140, MISS); //Report miss inside window
            endaction
            action
                let x <- p.getNextPrefetchAddr; //Previous window still recommended
                doAssert(x == 'h80000180, "test fail!");
            endaction
            action
                p.reportAccess('h81000000, MISS); //Report miss somewhere far away
            endaction
            action
                let x <- p.getNextPrefetchAddr; //New window allocated and recommended
                doAssert(x == 'h81000040, "test fail!");
            endaction
            action
                p.reportAccess('h80000180, MISS); //Report miss back home
            endaction
            action
                p.reportAccess('h82000000, MISS); //Report miss far away 2
            endaction
            action
                p.reportAccess('h80000100, MISS); //Report miss back home
            endaction
            action
                let x <- p.getNextPrefetchAddr; 
                doAssert(x == 'h80000140, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr; 
                doAssert(x == 'h80000180, "test fail!"); // window addresss recommended
            endaction
            action
                let x <- p.getNextPrefetchAddr; //target address recommended
                doAssert(x == 'h81000000, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr; //target address recommended
                doAssert(x == 'h82000000, "test fail!");
            endaction
        endseq
    );
endmodule

module mkBRAMStridePCPrefetcherTest(Empty);
    let p <- mkBRAMStridePCPrefetcher;
    mkAutoFSM(
        seq
            // ----- Send misses and stuff to one window -----
            action $display("%t", $time); p.reportAccess('h80000040, 'h0069, MISS); endaction
            action p.reportAccess('h80000080, 'h0069, HIT); endaction
            action p.reportAccess('h800000a0, 'h0069, HIT); endaction
            action p.reportAccess('h800000c0, 'h0069, MISS); endaction
            action p.reportAccess('h800000e0, 'h0069, MISS); endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h80000100, "test fail!");
            endaction
            action p.reportAccess('h80000100, 'h0069, HIT); endaction
            action p.reportAccess('h80000120, 'h0069, HIT); endaction
            action p.reportAccess('h80000140, 'h0069, HIT); endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h80000140, "test fail!");
            endaction
            action endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h80000180, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h800001c0, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h80000200, "test fail!");
            endaction
            action p.reportAccess('h900001f0, 'h006a, MISS); endaction
            action p.reportAccess('h900001c0, 'h006a, HIT); endaction
            action p.reportAccess('h90000190, 'h006a, HIT); endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h90000150, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h90000110, "test fail!");
            endaction
            action p.reportAccess('h90000100, 'h006b, MISS); endaction
            action p.reportAccess('h90000200, 'h006b, MISS); endaction
            action p.reportAccess('h90000300, 'h006b, MISS); endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h900000d0, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h90000400, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h90000500, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h90000600, "test fail!");
            endaction
            action p.reportAccess('h90000160, 'h006a, HIT); endaction
            // -- use older entry
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h900000a0, "test fail!");
            endaction
        endseq
    );
endmodule

module mkBRAMStrideAdaptivePCPrefetcherTest(Empty);
    let p <- mkBRAMStrideAdaptivePCPrefetcher;
    mkAutoFSM(
        seq
            // ----- Send misses and stuff to one window -----
            action $display("%t", $time); p.reportAccess('h80000040, 'h0069, MISS); endaction
            action p.reportAccess('h80000080, 'h0069, HIT); endaction
            action p.reportAccess('h800000a0, 'h0069, HIT); endaction
            action p.reportAccess('h800000c0, 'h0069, MISS); endaction
            action p.reportAccess('h800000e0, 'h0069, MISS); endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h80000100, "test fail!");
            endaction
            action p.reportAccess('h80000100, 'h0069, HIT); endaction
            action p.reportAccess('h80000120, 'h0069, HIT); endaction
            action p.reportAccess('h80000140, 'h0069, HIT); endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h80000140, "test fail!");
            endaction
            action endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h80000180, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h800001c0, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h80000200, "test fail!");
            endaction
            action p.reportAccess('h900001f0, 'h006a, MISS); endaction
            action p.reportAccess('h900001c0, 'h006a, HIT); endaction
            action p.reportAccess('h90000190, 'h006a, HIT); endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h80000240, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h80000280, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h90000150, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h90000110, "test fail!");
            endaction
            action p.reportAccess('h90000100, 'h006b, MISS); endaction
            action p.reportAccess('h90000200, 'h006b, MISS); endaction
            action p.reportAccess('h90000300, 'h006b, MISS); endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h90000400, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h90000500, "test fail!");
            endaction
            action p.reportAccess('h90000160, 'h006a, HIT); endaction
            // -- use older entry
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h900000e0, "test fail!");
            endaction
            action p.reportAccess('h80000080, 'h0069, MISS); endaction
            //Tolerate broken stride, request again
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h800000c0, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h80000100, "test fail!");
            endaction
            action p.reportAccess('h80000088, 'h0069, MISS); endaction
            action p.reportAccess('h80000090, 'h0069, MISS); endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h800000d0, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h80000110, "test fail!");
            endaction
            action p.reportAccess('h80000098, 'h0069, MISS); endaction
            action p.reportAccess('h800000a0, 'h0069, MISS); endaction
            action p.reportAccess('h800000a8, 'h0069, MISS); endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h80000168, "test fail!");
            endaction
            action p.reportAccess('h800000b0, 'h0069, MISS); endaction
            action p.reportAccess('h800000b8, 'h0069, MISS); endaction
        endseq
    );
endmodule

module mkBRAMMarkovPrefetcherTest(Empty);
    //let p <- mkMultipleWindowPrefetcher;
    //TODO pass in value of cachelinesinrange
    let p <- mkBRAMMarkovPrefetcher;
    mkAutoFSM(
        seq
            // ----- Send misses and stuff to one window -----
            action
                p.reportAccess('h80000000, MISS);
            endaction
            action
                p.reportAccess('h80000700, MISS); 
            endaction
            action
                p.reportAccess('h90000000, MISS); 
            endaction
            action
                p.reportAccess('ha0000000, MISS); 
            endaction
            action
                p.reportAccess('h80000000, HIT); //back to start
            endaction
            action
                let x <- p.getNextPrefetchAddr; 
                doAssert(x == 'h80000700, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr; 
                doAssert(x == 'h90000000, "test fail!");
            endaction
            action
                p.reportAccess('h80000700, HIT); 
            endaction
            action
                p.reportAccess('ha0000000, HIT); 
            endaction
            action
                let x <- p.getNextPrefetchAddr; 
                doAssert(x == 'h80000000, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr; 
                doAssert(x == 'h80000700, "test fail!");
            endaction
        endseq
    );
endmodule
module mkOverflowPipelineFifoTest(Empty);
    //let p <- mkMultipleWindowPrefetcher;
    Fifo#(4, Bit#(8)) p <- mkOverflowPipelineFifo;
    mkAutoFSM(
        seq
            // ----- Send misses and stuff to one window -----
            action
                p.enq('h01);
            endaction
            action
                p.enq('h02);
            endaction
            action
                p.enq('h03);
            endaction
            action
                p.enq('h04);
            endaction
            action
                let x = p.first; 
                p.deq;
                doAssert(x == 'h01, "test fail!");
            endaction
            action
                p.enq('h05);
                let x = p.first; 
                p.deq;
                doAssert(x == 'h02, "test fail!");
            endaction
            action
                p.enq('h06);
            endaction
            action
                p.enq('h07);
                let x = p.first; 
                p.deq;
                doAssert(x == 'h03, "test fail!");
            endaction
            action
                p.enq('h08);
            endaction
            action
                p.enq('h09);
                let x = p.first; 
                p.deq;
                doAssert(x == 'h05, "test fail!");
            endaction
            action
                let x = p.first; 
                p.deq;
                doAssert(x == 'h06, "test fail!");
            endaction
            action
                let x = p.first; 
                p.deq;
                doAssert(x == 'h07, "test fail!");
            endaction
            action
                let x = p.first; 
                p.deq;
                doAssert(x == 'h08, "test fail!");
            endaction
            action
                let x = p.first; 
                p.deq;
                doAssert(x == 'h09, "test fail!");
            endaction
            action
                doAssert(!p.notEmpty, "test fail!");
            endaction
            action
                $display("test done!");
            endaction
        endseq
    );
endmodule

module mkStride2PCPrefetcherTest(Empty);
    //paremeter - 2 ahead
    let p <- mkStride2PCPrefetcher;
    mkAutoFSM(
        seq
            // ----- Send misses and stuff to one window -----
            action $display("%t", $time); p.reportAccess('h80000040, 'h0069, MISS); endaction
            action p.reportAccess('h80000080, 'h0069, HIT); endaction
            action p.reportAccess('h800000a0, 'h0069, HIT); endaction
            action p.reportAccess('h800000c0, 'h0069, MISS); endaction
            action p.reportAccess('h800000e0, 'h0069, MISS); endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h80000120, "test fail!");
            endaction
            action p.reportAccess('h80000100, 'h0069, HIT); endaction
            action p.reportAccess('h80000120, 'h0069, HIT); endaction
            action p.reportAccess('h80000140, 'h0069, HIT); endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h80000160, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h80000180, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h800001c0, "test fail!");
            endaction
            action p.reportAccess('h900001f0, 'h006a, MISS); endaction
            action p.reportAccess('h900001c0, 'h006a, HIT); endaction
            action p.reportAccess('h90000190, 'h006a, HIT); endaction
            action p.reportAccess('h90000160, 'h006a, HIT); endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h90000120, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h900000e0, "test fail!");
            endaction
            action p.reportAccess('h90000100, 'h006b, MISS); endaction
            action p.reportAccess('h90000200, 'h006b, MISS); endaction
            action p.reportAccess('h90000300, 'h006b, MISS); endaction
            action p.reportAccess('h90000400, 'h006b, MISS); endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h90000500, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h90000600, "test fail!");
            endaction
            action p.reportAccess('ha0000420, 'h006b, MISS); endaction
            action p.reportAccess('ha0000520, 'h006b, MISS); endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'ha0000620, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'ha0000720, "test fail!");
            endaction

            action p.reportAccess('h90000130, 'h006a, HIT); endaction
            // -- use older entry
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h900000b0, "test fail!");
            endaction
        endseq
    );
endmodule