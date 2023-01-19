import Prefetcher::*;
import StmtFSM::*;
import Types::*;

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

module mkMultiWindowPrefetcherTest(Empty);
    //let p <- mkMultipleWindowPrefetcher;
    //TODO pass in value of cachelinesinrange
    let p <- mkMultiWindowPrefetcher;
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
                let x <- p.getNextPrefetchAddr; //most recent window still recommended
                doAssert(x == 'h80000200, "test fail!");
            endaction

            
        endseq
    );
endmodule

module mkMultiWindowTargetPrefetcherTest(Empty);
    //let p <- mkMultipleWindowPrefetcher;
    //TODO pass in value of cachelinesinrange
    let p <- mkMultiWindowTargetPrefetcher;
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
                let x <- p.getNextPrefetchAddr; //new window recommended
                doAssert(x == 'h50000000, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h800001c0, "test fail!");
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

module mkSingleWindowTargetPrefetcherTest(Empty);
    //let p <- mkMultipleWindowPrefetcher;
    //TODO pass in value of cachelinesinrange
    let p <- mkSingleWindowTargetPrefetcher;
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
                p.reportAccess('h80000100, MISS); //Report miss back home
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

module mkStridePCPrefetcherTest(Empty);
    //let p <- mkMultipleWindowPrefetcher;
    //TODO pass in value of cachelinesinrange
    let p <- mkStridePCPrefetcher;
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

module mkMarkovPrefetcherTest(Empty);
    //let p <- mkMultipleWindowPrefetcher;
    //TODO pass in value of cachelinesinrange
    let p <- mkMarkovPrefetcher;
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