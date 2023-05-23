// Copyright (c) 2023 Karlis Susters 
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

import Prefetcher::*;
import RWBramCore::*;
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

module mkTargetTableDoubleTest(Empty);
    TargetTableDouble#(2048, 128) t <- mkTargetTableDouble;
    mkAutoFSM(
        seq
            // ----- Send misses and stuff to one window -----
            action
                t.sendReadWriteReq('h8000, MISS); // goes in short table
            endaction
            action let x <- t.readResp(); endaction
            action
                t.sendReadWriteReq('h800a, MISS); // goes in short table
            endaction
            action let x <- t.readResp(); endaction
            action
                t.sendReadWriteReq('h8080000b, MISS); // goes in long table
            endaction
            action let x <- t.readResp(); endaction
            action
                t.sendReadWriteReq('h8000, MISS); // comes from short table
            endaction
            action
                let x <- t.readResp(); 
                doAssert(x == tuple2(Valid('h800a), Invalid), "test fail!");
            endaction
            action
                t.sendReadWriteReq('h800a, MISS); // comes from long table
            endaction
            action
                let x <- t.readResp(); 
                doAssert(x == tuple2(Valid('h8080000b), Invalid), "test fail!");
            endaction


            action
                t.sendReadWriteReq('h8000, MISS); 
            endaction
            action let x <- t.readResp(); endaction
            action
                t.sendReadWriteReq('h2123000c, MISS); 
            endaction
            action let x <- t.readResp(); endaction
            action
                t.sendReadWriteReq('h8000, HIT); 
            endaction
            action
                let x <- t.readResp(); 
                doAssert(x == tuple2(Valid('h2123000c), Valid('h800a)), "test fail!");
            endaction
            action
                t.sendReadWriteReq('h8000, MISS); 
            endaction
            action let x <- t.readResp(); endaction
            action
                t.sendReadWriteReq('h2123000c, MISS); 
            endaction
            action let x <- t.readResp(); endaction
            action
                t.sendReadWriteReq('h8000, MISS); 
            endaction
            action
                let x <- t.readResp();
                doAssert(x == tuple2(Valid('h2123000c), Valid('h800a)), "test fail!");
            endaction


            /// ---- Add third entry (narrow) 8003
            action
                t.sendReadWriteReq('h8003, MISS); 
            endaction
            action let x <- t.readResp(); endaction
            action
                t.sendReadWriteReq('h8000, MISS); 
            endaction
            action
                //wide entry is moved to LRU
                let x <- t.readResp();
                doAssert(x == tuple2(Valid('h8003), Valid('h2123000c)), "test fail!");
            endaction
            action
                t.sendReadWriteReq('h8003, MISS); 
            endaction
            action let x <- t.readResp(); endaction
            action
                t.sendReadWriteReq('h8000, MISS); 
            endaction
            action
                let x <- t.readResp();
                doAssert(x == tuple2(Valid('h8003), Valid('h2123000c)), "test fail!");
            endaction
            //refresh 21230000
            action
                t.sendReadWriteReq('h2123000c, MISS); 
            endaction
            action let x <- t.readResp(); endaction

            action
                t.sendReadWriteReq('h8000, MISS); 
            endaction
            action
                let x <- t.readResp();
                doAssert(x == tuple2(Valid('h2123000c), Valid('h8003)), "test fail!");
            endaction

            /// ---- Add another entry (wide) 2123000d
            action
                t.sendReadWriteReq('h2123000d, MISS); 
            endaction
            action let x <- t.readResp(); endaction
            action
                t.sendReadWriteReq('h8000, MISS); 
            endaction
            action
                //wide entry is moved to LRU
                let x <- t.readResp();
                doAssert(x == tuple2(Valid('h2123000d), Valid('h2123000c)), "test fail!");
            endaction


            /// ---- Add fifth entry (narrow) 8004
            action
                t.sendReadWriteReq('h8004, MISS); 
            endaction
            action let x <- t.readResp(); endaction
            action
                t.sendReadWriteReq('h8000, MISS); 
            endaction
            action
                //wide entry is moved to LRU
                let x <- t.readResp();
                doAssert(x == tuple2(Valid('h8004), Valid('h2123000d)), "test fail!");
            endaction
            // --- Add a sixth narrow entry 8005
            action
                t.sendReadWriteReq('h8005, MISS); 
            endaction
            action let x <- t.readResp(); endaction
            action
                t.sendReadWriteReq('h8000, MISS); 
            endaction
            action
                //wide entry is moved to LRU
                let x <- t.readResp();
                doAssert(x == tuple2(Valid('h8005), Valid('h8004)), "test fail!");
            endaction


            action 
                t.sendReadWriteReq('h7004, MISS); // get from short table 
            endaction

            action
                let x <- t.readResp(); 
                doAssert(x == tuple2(Invalid, Invalid), "test fail!");
            endaction
            action
                t.sendReadWriteReq('h6fde, MISS); // get from short table
            endaction
            action let x <- t.readResp(); endaction
            action
                t.sendReadWriteReq('h7004, MISS); // get from short table
            endaction
            action
                let x <- t.readResp();
                doAssert(x == tuple2(Valid('h6fde), Invalid), "test fail!");
            endaction
            action
                t.sendReadWriteReq('h200, HIT); // get from short table
            endaction
            action
                let x <- t.readResp();
                doAssert(x == tuple2(Invalid, Invalid), "test fail!"); //entry was removed!
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
                let x <- p.getNextPrefetchAddr; 
                doAssert(x == 'h80000200, "test fail!"); // window addresss recommended
            endaction
        endseq
    );
endmodule

module mkBRAMSingleWindowTargetPrefetcherTest(Empty);
    //2 ahead
    //remember last 2 target table requests.
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
                p.reportAccess('h81000200, MISS); //Report miss somewhere far away
            endaction
            action
                let x <- p.getNextPrefetchAddr; //New window allocated and recommended
                doAssert(x == 'h81000240, "test fail!");
            endaction
            action
                p.reportAccess('h80000180, MISS); //Report miss back home
            endaction
            action endaction
            action
                p.reportAccess('h82000000, MISS); //Report miss far away 2
            endaction
            action
                p.reportAccess('h80000140, MISS); //Report miss back home
            endaction
            action
                let x <- p.getNextPrefetchAddr; //target address recommended
                doAssert(x == 'h81000200, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr; 
                doAssert(x == 'h80000180, "test fail!"); // window addresss recommended
            endaction
            action
                p.reportAccess('h80000140, HIT); //Report miss back home
            endaction
            action
                p.reportAccess('h81000200, HIT); 
            endaction
            action
                p.reportAccess('h80000140, HIT);  //overwrite last target entry
            endaction
            action
                let x <- p.getNextPrefetchAddr; //target addresss recommended
                doAssert(x == 'h81000200, "test fail!");
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
    // config is 2 - 3 - 5
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

module mkMarkovOnHit2PrefetcherTest(Empty);
    //let p <- mkMultipleWindowPrefetcher;
    //TODO pass in value of cachelinesinrange
    let p <- mkMarkovOnHit2Prefetcher;
    mkAutoFSM(
        seq
            // ----- Send misses and stuff to one window -----
            action
                p.reportAccess('h80010000, MISS);
            endaction
            action
                p.reportAccess('h80010700, MISS); 
            endaction
            action
                p.reportAccess('h90010000, MISS); 
            endaction
            action
                p.reportAccess('ha0010300, MISS); 
            endaction
            action
                p.reportAccess('h80010000, MISS); //back to start
            endaction
            action
                let x <- p.getNextPrefetchAddr; 
                doAssert(x == 'h80010700, "test fail!");
            endaction

            //Add second target
            action
                p.reportAccess('h80010900, MISS); 
            endaction
            action
                p.reportAccess('h80010000, MISS); //back to start
            endaction
            action
                let x <- p.getNextPrefetchAddr; 
                doAssert(x == 'h80010900, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr; 
                doAssert(x == 'h80010700, "test fail!");
            endaction

            action
                p.reportAccess('ha0010300, MISS); 
            endaction
            action
                let x <- p.getNextPrefetchAddr; 
                doAssert(x == 'h80010000, "test fail!");
            endaction
            action
                p.reportAccess('ha0010200, MISS); 
            endaction
            action
                p.reportAccess('ha0010300, MISS); 
            endaction
            action
                let x <- p.getNextPrefetchAddr; 
                doAssert(x == 'ha0010200, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr; 
                doAssert(x == 'h80010000, "test fail!");
            endaction
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

module mkOverflowBypassFifoTest(Empty);
    Fifo#(4, Bit#(8)) p <- mkOverflowBypassFifo;
    mkAutoFSM(
        seq
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
                $display("found %x", x);
                doAssert(x == 'h04, "test fail!");
            endaction
            action
                p.enq('h08);
            endaction
            action
                p.enq('h09);
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
                doAssert(x == 'h90000150, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h90000110, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h900000e0, "test fail!");
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
            
            // -- test blocking on page boundaries
            action p.reportAccess('ha0000000, 'h006c, MISS); endaction
            action p.reportAccess('ha0000400, 'h006c, MISS); endaction
            action p.reportAccess('ha0000800, 'h006c, MISS); endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'ha0000c00, "test fail!");
            endaction
            action p.reportAccess('ha0000c00, 'h006c, MISS); endaction
            action p.reportAccess('hb0000000, 'h006c, MISS); endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'hb0000400, "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'hb0000800, "test fail!");
            endaction
        endseq
    );
endmodule

module mkPrefetcherVectorTest(Empty);
    //config - 2 lines
    PrefetcherVector#(3) p <- mkPrefetcherVector(mkNextLineOnMissPrefetcher);
    mkAutoFSM(
        seq
            action p.reportAccess(1, 'h90000000, MISS); endaction
            action p.reportAccess(0, 'h80000000, MISS); endaction
            action
                let x <- p.getNextPrefetchAddr;
                $display("%t Got %x", $time, x);
                doAssert(x == tuple2('h90000040, 1), "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                $display("%t Got %x", $time, x);
                doAssert(x == tuple2('h90000080, 1), "test fail!");
            endaction
            action p.reportAccess(0, 'h40000000, MISS); endaction
            action p.reportAccess(1, 'h50000000, MISS); endaction
            action
                let x <- p.getNextPrefetchAddr;
                $display("%t Got %x", $time, x);
                doAssert(x == tuple2('h80000040, 0), "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                $display("%t Got %x", $time, x);
                doAssert(x == tuple2('h40000040, 0), "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                $display("%t Got %x", $time, x);
                doAssert(x == tuple2('h50000040, 1), "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                $display("%t Got %x", $time, x);
                doAssert(x == tuple2('h40000080, 0), "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                $display("%t Got %x", $time, x);
                doAssert(x == tuple2('h50000080, 1), "test fail!");
            endaction
            action p.reportAccess(2, 'ha0000000, MISS); endaction
            action
                let x <- p.getNextPrefetchAddr;
                $display("%t Got %x", $time, x);
                doAssert(x == tuple2('ha0000040, 2), "test fail!");
            endaction
            action
                let x <- p.getNextPrefetchAddr;
                $display("%t Got %x", $time, x);
                doAssert(x == tuple2('ha0000080, 2), "test fail!");
            endaction
        endseq
    );
endmodule

module mkSimpleStridePCPrefetcherTest(Empty);
    //paremeter - 2 ahead
    let p <- mkSimpleStridePCPrefetcher;
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
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h80000160, "test fail!");
            endaction
            action p.reportAccess('h80000400, 'h0069, MISS); endaction
            action p.reportAccess('h80000300, 'h0069, MISS); endaction
            action p.reportAccess('h80000200, 'h0069, MISS); endaction
            action p.reportAccess('h80000100, 'h0069, MISS); endaction
            action
                let x <- p.getNextPrefetchAddr;
                doAssert(x == 'h80000000, "test fail!");
            endaction
        endseq
    );
endmodule