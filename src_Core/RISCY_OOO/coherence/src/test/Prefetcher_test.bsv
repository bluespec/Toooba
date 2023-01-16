import Prefetcher::*;
import StmtFSM::*;
import Types::*;


module mkMultiWindowPrefetcherTest(Empty);
    //let p <- mkMultipleWindowPrefetcher;
    //TODO pass in value of cachelinesinrange
    let p <- mkMultiWindowPrefetcher;
    mkAutoFSM(
        seq
            // ----- Send misses and stuff to one window -----
            action
                p.reportMiss('h80000040);
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
                p.reportHit('h800000c0); //Report hit inside window
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h80000100, "test fail!");
            endaction
            action
                p.reportHit('h80004000); //Report hit outside window
            endaction
            action
                let x <- p.getNextPrefetchAddr; //Previous window still recommended
                doAssert(x == 'h80000140, "test fail!");
            endaction
            action
                p.reportMiss('h80000140); //Report miss inside window
            endaction
            action
                let x <- p.getNextPrefetchAddr; //Previous window still recommended
                doAssert(x == 'h80000180, "test fail!");
            endaction

            // -----  Allocate other windows ----- 
            action
                p.reportMiss('h70000000); //Report miss outside window
            endaction
            action
                let x <- p.getNextPrefetchAddr; //new window recommended
                doAssert(x == 'h70000040, "test fail!");
            endaction
            action
                p.reportMiss('h90000000); //Report miss outside window
            endaction
            action
                let x <- p.getNextPrefetchAddr; //new window recommended
                doAssert(x == 'h90000040, "test fail!");
            endaction
            action
                p.reportMiss('h60000000); //Report miss outside window
            endaction
            action
                let x <- p.getNextPrefetchAddr; //new window recommended
                doAssert(x == 'h60000040, "test fail!");
            endaction
            action
                p.reportHit('h80000180); //Report hit inside oldest window
            endaction
            action
                let x <- p.getNextPrefetchAddr; //oldest window recommended
                doAssert(x == 'h800001c0, "test fail!");
            endaction

            // ----- Trigger window deletion -----
            action
                p.reportMiss('h50000000); //Report miss outside window,
                //discard window with 'h70..
            endaction
            action
                let x <- p.getNextPrefetchAddr; //new window recommended
                doAssert(x == 'h50000040, "test fail!");
            endaction
            action
                p.reportHit('h70000040); //Report hit inside now deleted window
            endaction
            action
                let x <- p.getNextPrefetchAddr; //most recent window still recommended
                doAssert(x == 'h50000080, "test fail!");
            endaction

            // ----- Reorder some more windows around
            action
                p.reportMiss('h800001c0); //Report hit inside now deleted window
            endaction
            action
                let x <- p.getNextPrefetchAddr; //most recent window still recommended
                doAssert(x == 'h80000200, "test fail!");
            endaction

            
        endseq
    );
endmodule

module mkSingleWindowTargetPrefetcherTest(Empty);
    //let p <- mkMultipleWindowPrefetcher;
    //TODO pass in value of cachelinesinrange
    let p <- mkSingleWindowTargetPrefetcher;
    mkAutoFSM(
        seq
            // ----- Send misses and stuff to one window -----
            action
                p.reportMiss('h80000040);
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
                p.reportHit('h800000c0); //Report hit inside window
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h80000100, "test fail!");
            endaction
            action
                p.reportHit('h80004000); //Report hit outside window
            endaction
            action
                let x <- p.getNextPrefetchAddr; //Previous window still recommended
                doAssert(x == 'h80000140, "test fail!");
            endaction
            action
                p.reportMiss('h80000140); //Report miss inside window
            endaction
            action
                let x <- p.getNextPrefetchAddr; //Previous window still recommended
                doAssert(x == 'h80000180, "test fail!");
            endaction
            action
                p.reportMiss('h81000000); //Report miss somewhere far away
            endaction
            action
                let x <- p.getNextPrefetchAddr; //New window allocated and recommended
                doAssert(x == 'h81000040, "test fail!");
            endaction
            action
                p.reportMiss('h80000100); //Report miss back home
            endaction
            action
                let x <- p.getNextPrefetchAddr; 
                doAssert(x == 'h80000140, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr; //target address recommended
                doAssert(x == 'h81000000, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr; 
                doAssert(x == 'h80000180, "test fail!"); // window addresss recommended
            endaction
        endseq
    );
endmodule

module mkStridePCPrefetcher2Test(Empty);
    //let p <- mkMultipleWindowPrefetcher;
    //TODO pass in value of cachelinesinrange
    let p <- mkStridePCPrefetcher2;
    mkAutoFSM(
        seq
            // ----- Send misses and stuff to one window -----
            action p.reportAccess('h80000040, 'h0069, MISS); endaction
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
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h900000a0, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h90000600, "test fail!");
            endaction
        endseq
    );
endmodule


module mkStridePCPrefetcherTest(Empty);
    //let p <- mkMultipleWindowPrefetcher;
    //TODO pass in value of cachelinesinrange
    let p <- mkStridePCPrefetcher;
    mkAutoFSM(
        seq
            // ----- Send misses and stuff to one window -----
            action
                p.reportAccess('h80000040, 'h0069, MISS);
            endaction
            action
                p.reportAccess('h80000080, 'h0069, HIT);
            endaction
            action
                p.reportAccess('h800000a0, 'h0069, HIT);
            endaction
            action
                p.reportAccess('h800000c0, 'h0069, MISS);
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h80000100, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h80000140, "test fail!");
            endaction
            action
                p.reportAccess('h800000e0, 'h0069, MISS);
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h80000180, "test fail!");
            endaction
            action
                p.reportAccess('h80000100, 'h0069, MISS);
            endaction
            action
                p.reportAccess('h80000120, 'h0069, MISS);
            endaction
            action
                p.reportAccess('h80000140, 'h0069, MISS);
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h80000160, "test fail!");
            endaction
            action
                p.reportAccess('h90000040, 'h0000, MISS);
            endaction
            action
                p.reportAccess('h90000140, 'h0000, MISS);
            endaction
            action
                p.reportAccess('h90000240, 'h0000, MISS);
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h90000340, "test fail!");
            endaction
            action
                p.reportAccess('h80000160, 'h0069, MISS);
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h80000180, "test fail!");
            endaction
        endseq
    );
endmodule
