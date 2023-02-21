import ISA_Decls   :: *;
import RWBramCore::*;
import FIFO::*;
import FIFOF::*;
import SpecialFIFOs :: *;
import Ehr::*;
import CacheUtils::*;
import CCTypes::*;
import Types::*;
import Vector::*;
import ProcTypes::*;

typedef enum {
    HIT = 1'b0, MISS = 1'b1
} HitOrMiss deriving (Bits, Eq, FShow);

interface Prefetcher;
    (* always_ready *)
    method Action reportAccess(Addr addr, HitOrMiss hitMiss);
    method ActionValue#(Addr) getNextPrefetchAddr();
    //method Action flush;
    //method Bool flush_done;
endinterface

module mkDoNothingPrefetcher(Prefetcher);
    method Action reportAccess(Addr addr, HitOrMiss hitMiss);
    endmethod
    method ActionValue#(Addr) getNextPrefetchAddr if (False);
        return 64'h0;
    endmethod
endmodule

module mkAlwaysRequestPrefetcher(Prefetcher);
    method Action reportAccess(Addr addr, HitOrMiss hitMiss);
    endmethod
    method ActionValue#(Addr) getNextPrefetchAddr;
        return 64'h80000040;
    endmethod
endmodule

module mkPrintPrefetcher(Prefetcher);
    method Action reportAccess(Addr addr, HitOrMiss hitMiss);
        if (hitMiss == HIT) begin
            $display("%t PrintPrefetcher report HIT %h", $time, addr);
        end
        else begin
            $display("%t PrintPrefetcher report MISS %h", $time, addr);
        end
    endmethod
    method ActionValue#(Addr) getNextPrefetchAddr if (False);
        return 64'h0;
    endmethod
endmodule

module mkNextLineOnMissPrefetcher(Prefetcher)
    provisos (
        NumAlias#(nextLinesOnMiss, 2),
        Alias#(rqCntT, Bit#(TLog#(TAdd#(nextLinesOnMiss, 1))))
    );
    Reg#(Addr) lastMissAddr <- mkReg(0);
    Reg#(rqCntT) sentRequestCounter <- mkReg(fromInteger(valueOf(nextLinesOnMiss)));

    method Action reportAccess(Addr addr, HitOrMiss hitMiss);
        if (hitMiss == HIT) begin
            $display("%t Prefetcher report HIT %h", $time, addr);
        end
        else begin
            $display("%t Prefetcher report MISS %h", $time, addr);
            lastMissAddr <= addr;
            sentRequestCounter <= 0;
        end
    endmethod
    method ActionValue#(Addr) getNextPrefetchAddr if 
        (sentRequestCounter < fromInteger(valueOf(nextLinesOnMiss)));

        sentRequestCounter <= sentRequestCounter + 1;
        let addrToRequest = lastMissAddr + (zeroExtend(sentRequestCounter) + 1)*fromInteger(valueOf(DataSz));
        $display("%t Prefetcher getNextPrefetchAddr requesting %h", $time, addrToRequest);
        return addrToRequest;
    endmethod
endmodule

module mkNextLineOnAllPrefetcher(Prefetcher)
    provisos (
        NumAlias#(nextLinesOnAccess, 3),
        Alias#(rqCntT, Bit#(TLog#(TAdd#(nextLinesOnAccess, 1))))
    );
    Reg#(Addr) lastAccessAddr <- mkReg(0);
    Reg#(rqCntT) sentRequestCounter <- mkReg(fromInteger(valueOf(nextLinesOnAccess)));

    method Action reportAccess(Addr addr, HitOrMiss hitMiss);
        if (hitMiss == HIT) begin
            $display("%t Prefetcher report HIT %h", $time, addr);
            lastAccessAddr <= addr;
            sentRequestCounter <= 0;
        end
        else begin
            $display("%t Prefetcher report MISS %h", $time, addr);
            lastAccessAddr <= addr;
            sentRequestCounter <= 0;
        end
    endmethod
    method ActionValue#(Addr) getNextPrefetchAddr if 
        (sentRequestCounter < fromInteger(valueOf(nextLinesOnAccess)));

        sentRequestCounter <= sentRequestCounter + 1;
        let addrToRequest = lastAccessAddr + (zeroExtend(sentRequestCounter) + 1)*fromInteger(valueOf(DataSz));
        $display("%t Prefetcher getNextPrefetchAddr requesting %h", $time, addrToRequest);
        return addrToRequest;
    endmethod
endmodule

module mkSingleWindowPrefetcher(Prefetcher);
    Integer cacheLinesInRange = 2;
    Reg#(LineAddr) rangeEnd <- mkReg(0); //Points to one CLine after end of range
    Reg#(LineAddr) nextToAsk <- mkReg(0);
    method Action reportAccess(Addr addr, HitOrMiss hitMiss);
        let cl = getLineAddr(addr);
        if (hitMiss == HIT && 
            rangeEnd - fromInteger(cacheLinesInRange) - 1 < cl && 
            cl < rangeEnd) begin

            let nextEnd = cl + fromInteger(cacheLinesInRange) + 1;
            $display("%t Prefetcher report HIT %h, moving window end to %h", $time, addr, Addr'{nextEnd, '0});
            rangeEnd <= nextEnd;
        end
        else begin
            $display("%t Prefetcher report MISS %h", $time, addr);
            //Reset window
            nextToAsk <= getLineAddr(addr) + 1;
            rangeEnd <= getLineAddr(addr) + fromInteger(cacheLinesInRange) + 1;
        end
    endmethod
    method ActionValue#(Addr) getNextPrefetchAddr if (nextToAsk != rangeEnd);
        nextToAsk <= nextToAsk + 1;
        let retAddr = Addr'{nextToAsk, '0}; //extend cache line address to regular address
        $display("%t Prefetcher getNextPrefetchAddr requesting %h", $time, retAddr);
        return retAddr; 
    endmethod
endmodule


typedef struct {
    LineAddr rangeEnd;
    LineAddr nextToAsk;
} StreamEntry deriving (Bits);

module mkMultiWindowPrefetcher(Prefetcher)
provisos(
    NumAlias#(numWindows, 4),
    Alias#(windowIdxT, Bit#(TLog#(numWindows)))
);
    Integer cacheLinesInRange = 2;
    Vector#(numWindows, Reg#(StreamEntry)) streams 
        <- replicateM(mkReg(StreamEntry {rangeEnd: '0, nextToAsk: '0}));
    Vector#(numWindows, Reg#(windowIdxT)) shiftReg <- genWithM(compose(mkReg, fromInteger));

    function Action moveWindowToFront(windowIdxT window) = 
    action
        if (shiftReg[0] == window) begin
        end
        else if (shiftReg[1] == window) begin
            shiftReg[0] <= window;
            shiftReg[1] <= shiftReg[0];
        end
        else if (shiftReg[2] == window) begin
            shiftReg[0] <= window;
            shiftReg[1] <= shiftReg[0];
            shiftReg[2] <= shiftReg[1];
        end
        else if (shiftReg[3] == window) begin
            shiftReg[0] <= window;
            shiftReg[1] <= shiftReg[0];
            shiftReg[2] <= shiftReg[1];
            shiftReg[3] <= shiftReg[2];
        end
    endaction;

    function ActionValue#(Maybe#(windowIdxT)) getMatchingWindow(Addr addr) = 
    actionvalue
        //Finds the first window that contains addr
        let cl = getLineAddr(addr);
        function Bool pred(StreamEntry se);
            //TODO < gives 100 cycles less??????
            return (se.rangeEnd - fromInteger(cacheLinesInRange) - 1 <= cl 
                && cl < se.rangeEnd);
        endfunction
        //Find first window that contains cache line cl
        if (findIndex(pred, readVReg(streams)) matches tagged Valid .idx) begin
            return Valid(pack(idx));
        end
        else begin
            return Invalid;
        end
    endactionvalue;

    method Action reportAccess(Addr addr, HitOrMiss hitMiss);
        //Check if any stream line matches request
        //If so, advance that stream line and advance LRU shift reg
        //Otherwise if miss, allocate new stream line, and shift LRU reg completely,
        let idxMaybe <- getMatchingWindow(addr);
        if (idxMaybe matches tagged Valid .idx) begin
            moveWindowToFront(pack(idx)); //Update window as just used
            let newRangeEnd = getLineAddr(addr) + fromInteger(cacheLinesInRange) + 1;
            if (hitMiss == HIT) begin
                $display("%t Prefetcher report HIT %h, moving window end to %h for window idx %h", 
                    $time, addr, Addr'{newRangeEnd, '0}, idx);
                streams[idx].rangeEnd <= newRangeEnd;
            end
            else if (hitMiss == MISS) begin
                //Also reset nextToAsk on miss
                $display("%t Prefetcher report MISS %h, moving window end to %h for window idx %h", 
                    $time, addr, Addr'{newRangeEnd, '0}, idx);
                streams[idx] <= 
                    StreamEntry {nextToAsk: getLineAddr(addr) + 1, 
                                rangeEnd: newRangeEnd};
            end
        end
        else if (hitMiss == MISS) begin
            $display("%t Prefetcher report MISS %h, allocating new window, idx %h", $time, addr, shiftReg[3]);
            streams[shiftReg[3]] <= 
                StreamEntry {nextToAsk: getLineAddr(addr) + 1,
                            rangeEnd: getLineAddr(addr) + fromInteger(cacheLinesInRange) + 1};
            shiftReg[0] <= shiftReg[3];
            shiftReg[1] <= shiftReg[0];
            shiftReg[2] <= shiftReg[1];
            shiftReg[3] <= shiftReg[2];
        end
    endmethod

    method ActionValue#(Addr) getNextPrefetchAddr 
        if (streams[shiftReg[0]].nextToAsk != streams[shiftReg[0]].rangeEnd);

        streams[shiftReg[0]].nextToAsk <= streams[shiftReg[0]].nextToAsk + 1;
        let retAddr = Addr'{streams[shiftReg[0]].nextToAsk, '0}; //extend cache line address to regular address
        $display("%t Prefetcher getNextPrefetchAddr requesting %h from window idx %h", $time, retAddr, shiftReg[0]);
        return retAddr; 
    endmethod

endmodule

module mkMultiWindowCrossCachePrefetcher(Prefetcher)
provisos(
    NumAlias#(numWindows, 4),
    Alias#(windowIdxT, Bit#(TLog#(numWindows)))
);
    Integer cacheLinesInRange = 2;
    Vector#(numWindows, Reg#(StreamEntry)) streams 
        <- replicateM(mkReg(StreamEntry {rangeEnd: '0, nextToAsk: '0}));
    Vector#(numWindows, Reg#(windowIdxT)) shiftReg <- genWithM(compose(mkReg, fromInteger));

    function Action moveWindowToFront(windowIdxT window) = 
    action
        if (shiftReg[0] == window) begin
        end
        else if (shiftReg[1] == window) begin
            shiftReg[0] <= window;
            shiftReg[1] <= shiftReg[0];
        end
        else if (shiftReg[2] == window) begin
            shiftReg[0] <= window;
            shiftReg[1] <= shiftReg[0];
            shiftReg[2] <= shiftReg[1];
        end
        else if (shiftReg[3] == window) begin
            shiftReg[0] <= window;
            shiftReg[1] <= shiftReg[0];
            shiftReg[2] <= shiftReg[1];
            shiftReg[3] <= shiftReg[2];
        end
    endaction;

    function ActionValue#(Maybe#(windowIdxT)) getMatchingWindow(Addr addr) = 
    actionvalue
        //Finds the first window that contains addr
        let cl = getLineAddr(addr);
        function Bool pred(StreamEntry se);
            //TODO < gives 100 cycles less??????
            return (se.rangeEnd - fromInteger(cacheLinesInRange) - 1 <= cl 
                && cl < se.rangeEnd);
        endfunction
        //Find first window that contains cache line cl
        if (findIndex(pred, readVReg(streams)) matches tagged Valid .idx) begin
            return Valid(pack(idx));
        end
        else begin
            return Invalid;
        end
    endactionvalue;

    method Action reportAccess(Addr addr, HitOrMiss hitMiss);
        //Check if any stream line matches request
        //If so, advance that stream line and advance LRU shift reg
        //Otherwise if miss, allocate new stream line, and shift LRU reg completely,
        let idxMaybe <- getMatchingWindow(addr);
        if (idxMaybe matches tagged Valid .idx) begin
            moveWindowToFront(pack(idx)); //Update window as just used
            let newRangeEnd = getLineAddr(addr) + fromInteger(cacheLinesInRange) + 1;
            $display("%t Prefetcher report access %h, moving window end to %h for window idx %h", 
                $time, addr, Addr'{newRangeEnd, '0}, idx);
            streams[idx].rangeEnd <= newRangeEnd;
        end
        else if (hitMiss == MISS) begin
            //A miss in L1 is not necessarily a miss in L2, so this might create a window for lines already in L2
            $display("%t Prefetcher report MISS %h, allocating new window, idx %h", $time, addr, shiftReg[3]);
            streams[shiftReg[3]] <= 
                StreamEntry {nextToAsk: getLineAddr(addr) + 1,
                            rangeEnd: getLineAddr(addr) + fromInteger(cacheLinesInRange) + 1};
            shiftReg[0] <= shiftReg[3];
            shiftReg[1] <= shiftReg[0];
            shiftReg[2] <= shiftReg[1];
            shiftReg[3] <= shiftReg[2];
        end
    endmethod

    method ActionValue#(Addr) getNextPrefetchAddr 
        if (streams[shiftReg[0]].nextToAsk != streams[shiftReg[0]].rangeEnd);

        streams[shiftReg[0]].nextToAsk <= streams[shiftReg[0]].nextToAsk + 1;
        let retAddr = Addr'{streams[shiftReg[0]].nextToAsk, '0}; //extend cache line address to regular address
        $display("%t Prefetcher getNextPrefetchAddr requesting %h from window idx %h", $time, retAddr, shiftReg[0]);
        return retAddr; 
    endmethod

endmodule

typedef struct {
    Bit#(tagBits) tag;
    Bit#(distanceBits) distance;
} NarrowTargetEntry#(numeric type tagBits, numeric type distanceBits) deriving (Bits, Eq, FShow);

typedef struct {
    Bit#(tagBits) tag;
    LineAddr target;
} WideTargetEntry#(numeric type tagBits) deriving (Bits, Eq, FShow);

interface TargetTable#(numeric type narrowTableSize, numeric type wideTableSize);
    method Action set(LineAddr prevAddr, LineAddr currAddr);
    method ActionValue#(Maybe#(LineAddr)) getAndRemove(LineAddr addr);
endinterface

module mkTargetTable(TargetTable#(narrowTableSize, wideTableSize)) provisos
(
    NumAlias#(narrowTableIdxBits, TLog#(narrowTableSize)),
    NumAlias#(wideTableIdxBits, TLog#(wideTableSize)),
    NumAlias#(narrowTableTagBits, TSub#(32, narrowTableIdxBits)),
    NumAlias#(wideTableTagBits, TSub#(32, wideTableIdxBits)),
    NumAlias#(narrowDistanceBits, 10),
    NumAlias#(narrowMaxDistanceAbs, TExp#(TSub#(narrowDistanceBits, 1))),
    Alias#(narrowTargetEntryT, NarrowTargetEntry#(narrowTableTagBits, narrowDistanceBits)),
    Alias#(wideTargetEntryT, WideTargetEntry#(wideTableTagBits)),
    Add#(a__, TLog#(narrowTableSize), 32),
    Add#(b__, TLog#(narrowTableSize), 58),
    Add#(c__, TLog#(wideTableSize), 32),
    Add#(d__, TLog#(wideTableSize), 58)
);
    Vector#(narrowTableSize, Ehr#(2, Maybe#(narrowTargetEntryT))) narrowTable <- replicateM(mkEhr(Invalid));
    Vector#(wideTableSize, Ehr#(2, Maybe#(wideTargetEntryT))) wideTable <- replicateM(mkEhr(Invalid));
    method Action set(LineAddr prevAddr, LineAddr currAddr);
        let distance = currAddr - prevAddr;
        Bit#(32) prevAddrHash = hash(prevAddr);
        if (abs(distance) < fromInteger(valueOf(narrowMaxDistanceAbs))) begin
            //Store in narrow table
            narrowTargetEntryT entry;
            entry.tag = prevAddrHash[31:valueOf(narrowTableIdxBits)];
            entry.distance = truncate(distance);
            Bit#(narrowTableIdxBits) idx = truncate(prevAddrHash);
            narrowTable[idx][1] <= tagged Valid entry;
        end
        else begin
            //Store in wide table
            wideTargetEntryT entry;
            entry.tag = prevAddrHash[31:valueOf(wideTableIdxBits)];
            entry.target = currAddr;
            Bit#(wideTableIdxBits) idx = truncate(prevAddrHash);
            wideTable[idx][1] <= tagged Valid entry;
        end
    endmethod
    method ActionValue#(Maybe#(LineAddr)) getAndRemove(LineAddr addr);
        Bit#(narrowTableIdxBits) narrowIdx = truncate(addr);
        Bit#(wideTableIdxBits) wideIdx = truncate(addr);
        if (narrowTable[narrowIdx][0] matches tagged Valid .entry 
            &&& entry.tag == addr[31:valueOf(narrowTableIdxBits)]) begin
            narrowTable[narrowIdx][0] <= Invalid; 
            //$display("%t found narrow table entry %h", $time, addr + signExtend(pack(entry.distance)));
            return Valid(addr + signExtend(pack(entry.distance)));
        end
        else if (wideTable[wideIdx][0] matches tagged Valid .entry 
            &&& entry.tag == addr[31:valueOf(wideTableIdxBits)]) begin
            wideTable[wideIdx][0] <= Invalid; 
            //$display("%t found wide table entry %h", $time, entry.target);
            return Valid(entry.target);
        end
        else
            return Invalid;
    endmethod
endmodule

interface TargetTableBRAM#(numeric type narrowTableSize, numeric type wideTableSize);
    method Action writeReq(LineAddr prevAddr, LineAddr currAddr);
    method Action readReq(LineAddr addr);
    method ActionValue#(Maybe#(LineAddr)) readRespAndClear();
endinterface

module mkTargetTableBRAM(TargetTableBRAM#(narrowTableSize, wideTableSize)) provisos
(
    NumAlias#(narrowTableIdxBits, TLog#(narrowTableSize)),
    NumAlias#(wideTableIdxBits, TLog#(wideTableSize)),
    NumAlias#(narrowTableTagBits, TSub#(32, narrowTableIdxBits)),
    NumAlias#(wideTableTagBits, TSub#(32, wideTableIdxBits)),
    NumAlias#(narrowDistanceBits, 10),
    NumAlias#(narrowMaxDistanceAbs, TExp#(TSub#(narrowDistanceBits, 1))),
    Alias#(narrowTargetEntryT, NarrowTargetEntry#(narrowTableTagBits, narrowDistanceBits)),
    Alias#(wideTargetEntryT, WideTargetEntry#(wideTableTagBits)),
    Add#(a__, TLog#(narrowTableSize), 32),
    Add#(b__, TLog#(narrowTableSize), 58),
    Add#(c__, TLog#(wideTableSize), 32),
    Add#(d__, TLog#(wideTableSize), 58)
);
    RWBramCore#(Bit#(narrowTableIdxBits), Maybe#(narrowTargetEntryT)) narrowTable <- mkRWBramCoreForwarded;
    RWBramCore#(Bit#(wideTableIdxBits), Maybe#(wideTargetEntryT)) wideTable <- mkRWBramCoreForwarded;
    Reg#(LineAddr) readReqLineAddr <- mkReg(?); 

    method Action writeReq(LineAddr prevAddr, LineAddr currAddr);
        let distance = currAddr - prevAddr;
        Bit#(32) prevAddrHash = hash(prevAddr);
        if (abs(distance) < fromInteger(valueOf(narrowMaxDistanceAbs))) begin
            //Store in narrow table
            narrowTargetEntryT entry;
            entry.tag = prevAddrHash[31:valueOf(narrowTableIdxBits)];
            entry.distance = truncate(distance);
            Bit#(narrowTableIdxBits) idx = truncate(prevAddrHash);
            narrowTable.wrReq(idx, tagged Valid entry);
        end
        else begin
            //Store in wide table
            wideTargetEntryT entry;
            entry.tag = prevAddrHash[31:valueOf(wideTableIdxBits)];
            entry.target = currAddr;
            Bit#(wideTableIdxBits) idx = truncate(prevAddrHash);
            wideTable.wrReq(idx, tagged Valid entry);
        end
    endmethod

    method Action readReq(LineAddr addr);
        Bit#(narrowTableIdxBits) narrowIdx = truncate(addr);
        narrowTable.rdReq(narrowIdx);
        Bit#(wideTableIdxBits) wideIdx = truncate(addr);
        wideTable.rdReq(wideIdx);
        readReqLineAddr <= addr;
    endmethod

    method ActionValue#(Maybe#(LineAddr)) readRespAndClear();
        // Returns the read response and if a table had a hit, 
        // sends a write request to clear the entry in that table
        narrowTable.deqRdResp;
        wideTable.deqRdResp;
        let addr = readReqLineAddr;
        Bit#(narrowTableIdxBits) narrowIdx = truncate(addr);
        Bit#(wideTableIdxBits) wideIdx = truncate(addr);
        if (narrowTable.rdResp matches tagged Valid .entry 
            &&& entry.tag == addr[31:valueOf(narrowTableIdxBits)]) begin
            narrowTable.wrReq(narrowIdx, Invalid); 
            $display("%t found narrow table entry %h", $time, addr + signExtend(pack(entry.distance)));
            return Valid(addr + signExtend(pack(entry.distance)));
        end
        else if (wideTable.rdResp matches tagged Valid .entry 
            &&& entry.tag == addr[31:valueOf(wideTableIdxBits)]) begin
            wideTable.wrReq(wideIdx, Invalid); 
            $display("%t found wide table entry %h", $time, entry.target);
            return Valid(entry.target);
        end
        else begin
            return Invalid;
        end
        
    endmethod
endmodule

module mkBRAMSingleWindowTargetPrefetcher(Prefetcher) provisos
();
    Integer cacheLinesInRange = 2;
    Reg#(LineAddr) rangeEnd <- mkReg(0); //Points to one CLine after end of range
    Reg#(LineAddr) nextToAsk <- mkReg(0);
    Reg#(LineAddr) lastChildRequest <- mkReg(0);
    TargetTableBRAM#(64, 8) targetTable <- mkTargetTableBRAM;
    FIFOF#(LineAddr) targetTableReadResp <- mkBypassFIFOF;

    rule sendReadReq;
        let lastAsked = nextToAsk-1;
        targetTable.readReq(lastAsked);
    endrule

    rule getReadResp;
        let res <- targetTable.readRespAndClear();
        if (res matches tagged Valid .cline) begin
            //Reset table entry, so on further calls we prefetch the next successive clines
            //If we actually take the jump, the table entry will be restored
            targetTableReadResp.enq(cline);
        end
    endrule

    method Action reportAccess(Addr addr, HitOrMiss hitMiss);
        let cl = getLineAddr(addr);
        if (hitMiss == HIT && 
            rangeEnd - fromInteger(cacheLinesInRange) - 1 < cl && 
            cl < rangeEnd) begin

            let nextEnd = cl + fromInteger(cacheLinesInRange) + 1;
            $display("%t Prefetcher report HIT %h, moving window end to %h", $time, addr, Addr'{nextEnd, '0});
            rangeEnd <= nextEnd;
        end
        else if (hitMiss == MISS) begin
            $display("%t Prefetcher report MISS %h", $time, addr);
            //Reset window
            nextToAsk <= cl + 1;
            rangeEnd <= cl + fromInteger(cacheLinesInRange) + 1;
        end

        if (cl != lastChildRequest + 1 && cl != lastChildRequest && cl != lastChildRequest - 1) begin
            $display("%t Prefetcher add target entry from addr %h to addr %h", $time, Addr'{lastChildRequest, '0}, addr);
            targetTable.writeReq(lastChildRequest, cl);
        end
        lastChildRequest <= cl;
    endmethod

    method ActionValue#(Addr) getNextPrefetchAddr if (targetTableReadResp.notEmpty || nextToAsk != rangeEnd);
        Addr retAddr;
        if (targetTableReadResp.notEmpty) begin
            //If have valid table entry for some of the last requested clines, 
            // prefetch the stored target first
            retAddr = {targetTableReadResp.first, '0};
            targetTableReadResp.deq();
            $display("%t Prefetcher getNextPrefetchAddr requesting target entry %h", $time, retAddr);
        end
        else begin
            //If no table entry, prefetch further in window
            nextToAsk <= nextToAsk + 1;
            retAddr = {nextToAsk, '0}; 
            $display("%t Prefetcher getNextPrefetchAddr requesting next-line %h", $time, retAddr);
        end
        return retAddr; 
    endmethod
endmodule

module mkSingleWindowTargetPrefetcher(Prefetcher) provisos
();
    Integer cacheLinesInRange = 2;
    Reg#(LineAddr) rangeEnd <- mkReg(0); //Points to one CLine after end of range
    Reg#(LineAddr) nextToAsk <- mkReg(0);
    Reg#(LineAddr) lastChildRequest <- mkReg(0);
    TargetTable#(64, 8) targetTable <- mkTargetTable;
    //If hit outside window, reallocate window (test!)
    method Action reportAccess(Addr addr, HitOrMiss hitMiss);
        let cl = getLineAddr(addr);
        if (hitMiss == HIT && 
            rangeEnd - fromInteger(cacheLinesInRange) - 1 < cl && 
            cl < rangeEnd) begin

            let nextEnd = cl + fromInteger(cacheLinesInRange) + 1;
            $display("%t Prefetcher report HIT %h, moving window end to %h", $time, addr, Addr'{nextEnd, '0});
            rangeEnd <= nextEnd;
        end
        else if (hitMiss == MISS) begin
            $display("%t Prefetcher report MISS %h", $time, addr);
            //Reset window
            nextToAsk <= cl + 1;
            rangeEnd <= cl + fromInteger(cacheLinesInRange) + 1;
        end

        if (cl != lastChildRequest + 1 && cl != lastChildRequest && cl != lastChildRequest - 1) begin
            $display("%t Prefetcher add target entry from addr %h to addr %h", $time, Addr'{lastChildRequest, '0}, addr);
            targetTable.set(lastChildRequest, cl);
        end
        lastChildRequest <= cl;
    endmethod

    method ActionValue#(Addr) getNextPrefetchAddr if (nextToAsk != rangeEnd);
        Addr retAddr;
        let lastAsked = nextToAsk-1;
        let entryMaybe <- targetTable.getAndRemove(lastAsked);
        if (entryMaybe matches tagged Valid .cl) begin
            //If have valid entry for the last requested cline, prefetch the stored target first
            //Reset table entry, so on further calls we prefetch the next successive clines
            //If we actually take the jump, the table entry will be restored
            retAddr = {cl, '0};
            $display("%t Prefetcher getNextPrefetchAddr requesting target entry %h", $time, retAddr);
        end
        else begin
            //If no table entry, prefetch further in window
            nextToAsk <= nextToAsk + 1;
            retAddr = {nextToAsk, '0}; 
            $display("%t Prefetcher getNextPrefetchAddr requesting next-line %h", $time, retAddr);
        end
        return retAddr; 
    endmethod
endmodule

module mkBRAMMultiWindowTargetPrefetcher(Prefetcher)
provisos(
    NumAlias#(numWindows, 4),
    Alias#(windowIdxT, Bit#(TLog#(numWindows)))
);
    Integer cacheLinesInRange = 2;
    Vector#(numWindows, Reg#(StreamEntry)) streams 
        <- replicateM(mkReg(StreamEntry {rangeEnd: '0, nextToAsk: '0}));
    Vector#(numWindows, Reg#(windowIdxT)) shiftReg <- genWithM(compose(mkReg, fromInteger));
    Reg#(LineAddr) lastChildRequest <- mkReg(0);

    TargetTableBRAM#(64, 8) targetTable <- mkTargetTableBRAM;
    FIFOF#(LineAddr) targetTableReadResp <- mkBypassFIFOF;

    rule sendReadReq;
        let lastAsked = streams[shiftReg[0]].nextToAsk-1;
        targetTable.readReq(lastAsked);
    endrule

    rule getReadResp;
        let res <- targetTable.readRespAndClear();
        if (res matches tagged Valid .cline) begin
            //Reset table entry, so on further calls we prefetch the next successive clines
            //If we actually take the jump, the table entry will be restored
            targetTableReadResp.enq(cline);
        end
    endrule

    function Action moveWindowToFront(windowIdxT window) = 
    action
        if (shiftReg[0] == window) begin
        end
        else if (shiftReg[1] == window) begin
            shiftReg[0] <= window;
            shiftReg[1] <= shiftReg[0];
        end
        else if (shiftReg[2] == window) begin
            shiftReg[0] <= window;
            shiftReg[1] <= shiftReg[0];
            shiftReg[2] <= shiftReg[1];
        end
        else if (shiftReg[3] == window) begin
            shiftReg[0] <= window;
            shiftReg[1] <= shiftReg[0];
            shiftReg[2] <= shiftReg[1];
            shiftReg[3] <= shiftReg[2];
        end
    endaction;

    function ActionValue#(Maybe#(windowIdxT)) getMatchingWindow(LineAddr cl) = 
    actionvalue
        //Finds the first window that contains cache line
        function Bool pred(StreamEntry se);
            //TODO < gives 100 cycles less??????
            return (se.rangeEnd - fromInteger(cacheLinesInRange) - 1 <= cl 
                && cl < se.rangeEnd);
        endfunction
        //Find first window that contains cache line cl
        if (findIndex(pred, readVReg(streams)) matches tagged Valid .idx) begin
            return Valid(pack(idx));
        end
        else begin
            return Invalid;
        end
    endactionvalue;
    // test: allocate new window on hit too (mostly for target prefetching)

    method Action reportAccess(Addr addr, HitOrMiss hitMiss);
        //Check if any stream line matches request
        //If so, advance that stream line and advance LRU shift reg
        //Otherwise if miss, allocate new stream line, and shift LRU reg completely,
        let cl = getLineAddr(addr);
        // Update window prefetcher
        let idxMaybe <- getMatchingWindow(cl);
        if (idxMaybe matches tagged Valid .idx) begin
            moveWindowToFront(pack(idx)); //Update window as just used
            let newRangeEnd = getLineAddr(addr) + fromInteger(cacheLinesInRange) + 1;
            if (hitMiss == HIT) begin
                $display("%t Prefetcher report HIT %h, moving window end to %h for window idx %h", 
                    $time, addr, Addr'{newRangeEnd, '0}, idx);
                streams[idx].rangeEnd <= newRangeEnd;
            end
            else if (hitMiss == MISS) begin
                //Also reset nextToAsk on miss
                $display("%t Prefetcher report MISS %h, moving window end to %h for window idx %h", 
                    $time, addr, Addr'{newRangeEnd, '0}, idx);
                streams[idx] <= 
                    StreamEntry {nextToAsk: getLineAddr(addr) + 1, 
                                rangeEnd: newRangeEnd};
            end
        end
        else if (hitMiss == MISS) begin
            $display("%t Prefetcher report MISS %h, allocating new window, idx %h", $time, addr, shiftReg[3]);
            streams[shiftReg[3]] <= 
                StreamEntry {nextToAsk: getLineAddr(addr) + 1,
                            rangeEnd: getLineAddr(addr) + fromInteger(cacheLinesInRange) + 1};
            shiftReg[0] <= shiftReg[3];
            shiftReg[1] <= shiftReg[0];
            shiftReg[2] <= shiftReg[1];
            shiftReg[3] <= shiftReg[2];
        end

        // Update target prefetcher
        if (cl != lastChildRequest + 1 && cl != lastChildRequest && cl != lastChildRequest - 1) begin
            $display("%t Prefetcher add target entry from addr %h to addr %h", $time, Addr'{lastChildRequest, '0}, addr);
            targetTable.writeReq(lastChildRequest, cl);
        end
        lastChildRequest <= cl;
    endmethod

    method ActionValue#(Addr) getNextPrefetchAddr 
        if (targetTableReadResp.notEmpty || streams[shiftReg[0]].nextToAsk != streams[shiftReg[0]].rangeEnd);

        Addr retAddr;
        let lastAsked = streams[shiftReg[0]].nextToAsk-1;
        if (targetTableReadResp.notEmpty) begin
            //If have valid table entry for some of the last requested clines, 
            // prefetch the stored target first
            retAddr = {targetTableReadResp.first, '0};
            targetTableReadResp.deq();
            $display("%t Prefetcher getNextPrefetchAddr requesting target entry %h", $time, retAddr);
        end
        else begin
            streams[shiftReg[0]].nextToAsk <= streams[shiftReg[0]].nextToAsk + 1;
            retAddr = Addr'{streams[shiftReg[0]].nextToAsk, '0}; //extend cache line address to regular address
            $display("%t Prefetcher getNextPrefetchAddr requesting %h from window idx %h", $time, retAddr, shiftReg[0]);
        end
        return retAddr; 
    endmethod

endmodule

module mkMultiWindowTargetPrefetcher(Prefetcher)
provisos(
    NumAlias#(numWindows, 4),
    Alias#(windowIdxT, Bit#(TLog#(numWindows)))
);
    Integer cacheLinesInRange = 2;
    Vector#(numWindows, Reg#(StreamEntry)) streams 
        <- replicateM(mkReg(StreamEntry {rangeEnd: '0, nextToAsk: '0}));
    Vector#(numWindows, Reg#(windowIdxT)) shiftReg <- genWithM(compose(mkReg, fromInteger));
    Reg#(LineAddr) lastChildRequest <- mkReg(0);
    TargetTable#(64, 8) targetTable <- mkTargetTable;

    function Action moveWindowToFront(windowIdxT window) = 
    action
        if (shiftReg[0] == window) begin
        end
        else if (shiftReg[1] == window) begin
            shiftReg[0] <= window;
            shiftReg[1] <= shiftReg[0];
        end
        else if (shiftReg[2] == window) begin
            shiftReg[0] <= window;
            shiftReg[1] <= shiftReg[0];
            shiftReg[2] <= shiftReg[1];
        end
        else if (shiftReg[3] == window) begin
            shiftReg[0] <= window;
            shiftReg[1] <= shiftReg[0];
            shiftReg[2] <= shiftReg[1];
            shiftReg[3] <= shiftReg[2];
        end
    endaction;

    function ActionValue#(Maybe#(windowIdxT)) getMatchingWindow(LineAddr cl) = 
    actionvalue
        //Finds the first window that contains cache line
        function Bool pred(StreamEntry se);
            //TODO < gives 100 cycles less??????
            return (se.rangeEnd - fromInteger(cacheLinesInRange) - 1 <= cl 
                && cl < se.rangeEnd);
        endfunction
        //Find first window that contains cache line cl
        if (findIndex(pred, readVReg(streams)) matches tagged Valid .idx) begin
            return Valid(pack(idx));
        end
        else begin
            return Invalid;
        end
    endactionvalue;
    // test: allocate new window on hit too (mostly for target prefetching)

    method Action reportAccess(Addr addr, HitOrMiss hitMiss);
        //Check if any stream line matches request
        //If so, advance that stream line and advance LRU shift reg
        //Otherwise if miss, allocate new stream line, and shift LRU reg completely,
        let cl = getLineAddr(addr);
        // Update window prefetcher
        let idxMaybe <- getMatchingWindow(cl);
        if (idxMaybe matches tagged Valid .idx) begin
            moveWindowToFront(pack(idx)); //Update window as just used
            let newRangeEnd = getLineAddr(addr) + fromInteger(cacheLinesInRange) + 1;
            if (hitMiss == HIT) begin
                $display("%t Prefetcher report HIT %h, moving window end to %h for window idx %h", 
                    $time, addr, Addr'{newRangeEnd, '0}, idx);
                streams[idx].rangeEnd <= newRangeEnd;
            end
            else if (hitMiss == MISS) begin
                //Also reset nextToAsk on miss
                $display("%t Prefetcher report MISS %h, moving window end to %h for window idx %h", 
                    $time, addr, Addr'{newRangeEnd, '0}, idx);
                streams[idx] <= 
                    StreamEntry {nextToAsk: getLineAddr(addr) + 1, 
                                rangeEnd: newRangeEnd};
            end
        end
        else if (hitMiss == MISS) begin
            $display("%t Prefetcher report MISS %h, allocating new window, idx %h", $time, addr, shiftReg[3]);
            streams[shiftReg[3]] <= 
                StreamEntry {nextToAsk: getLineAddr(addr) + 1,
                            rangeEnd: getLineAddr(addr) + fromInteger(cacheLinesInRange) + 1};
            shiftReg[0] <= shiftReg[3];
            shiftReg[1] <= shiftReg[0];
            shiftReg[2] <= shiftReg[1];
            shiftReg[3] <= shiftReg[2];
        end

        // Update target prefetcher
        if (cl != lastChildRequest + 1 && cl != lastChildRequest && cl != lastChildRequest - 1) begin
            $display("%t Prefetcher add target entry from addr %h to addr %h", $time, Addr'{lastChildRequest, '0}, addr);
            targetTable.set(lastChildRequest, cl);
        end
        lastChildRequest <= cl;
    endmethod

    method ActionValue#(Addr) getNextPrefetchAddr 
        if (streams[shiftReg[0]].nextToAsk != streams[shiftReg[0]].rangeEnd);

        Addr retAddr;
        let lastAsked = streams[shiftReg[0]].nextToAsk-1;
        let clMaybe <- targetTable.getAndRemove(lastAsked);
        if (clMaybe matches tagged Valid .cl) begin
            //If have valid entry for the last requested cline, prefetch the stored target first
            //Reset table entry, so on further calls we prefetch the next successive clines
            //If we actually take the jump, the table entry will be restored
            retAddr = {cl, '0};
            $display("%t Prefetcher getNextPrefetchAddr requesting target entry %h", $time, retAddr);
        end
        else begin
            streams[shiftReg[0]].nextToAsk <= streams[shiftReg[0]].nextToAsk + 1;
            retAddr = Addr'{streams[shiftReg[0]].nextToAsk, '0}; //extend cache line address to regular address
            $display("%t Prefetcher getNextPrefetchAddr requesting %h from window idx %h", $time, retAddr, shiftReg[0]);
        end
        return retAddr; 
    endmethod

endmodule

module mkBRAMMarkovPrefetcher(Prefetcher) provisos
(
    NumAlias#(maxChainLength, 2),
    Alias#(chainLengthT, Bit#(TLog#(TAdd#(maxChainLength,1))))
);
    Reg#(LineAddr) lastLastChildRequest <- mkReg(0);
    Reg#(LineAddr) lastChildRequest <- mkReg(0);
    TargetTableBRAM#(64, 8) targetTable <- mkTargetTableBRAM;
    FIFOF#(LineAddr) targetTableReadResp <- mkBypassFIFOF;

    // Stores how many prefetches we can still do in the current chain
    Reg#(chainLengthT) chainNumberToPrefetch <- mkReg(0); 
    Reg#(LineAddr) chainNextToLookup <- mkReg(?);

    rule sendReadReq (chainNumberToPrefetch != 0);
        targetTable.readReq(chainNextToLookup);
    endrule

    (* descending_urgency = "getReadResp, sendReadReq" *)
    (* execution_order = "getReadResp, sendReadReq" *)
    rule getReadResp;
        let res <- targetTable.readRespAndClear();
        if (res matches tagged Valid .cline) begin
            targetTableReadResp.enq(cline);
            chainNextToLookup <= cline;
            chainNumberToPrefetch <= chainNumberToPrefetch - 1;
        end
        else begin
            chainNumberToPrefetch <= 0;
        end
    endrule

    method ActionValue#(Addr) getNextPrefetchAddr;
        targetTableReadResp.deq();
        let cline = targetTableReadResp.first;
        Addr retAddr = {cline, '0};
        $display("%t Prefetcher getNextPrefetchAddr requesting chain entry %h", $time, retAddr);
        return retAddr; 
    endmethod

    method Action reportAccess(Addr addr, HitOrMiss hitMiss);
        let cl = getLineAddr(addr);
        if (cl != lastChildRequest + 1 && cl != lastChildRequest && cl != lastChildRequest - 1) begin
            $display("%t Prefetcher report %s add target entry from addr %h to addr %h", 
                $time, hitMiss == HIT ? "HIT" : "MISS", Addr'{lastChildRequest, '0}, addr);
            targetTable.writeReq(lastChildRequest, cl);
        end
        lastChildRequest <= cl;
        lastLastChildRequest <= lastChildRequest;

        if (lastLastChildRequest != cl) begin 
            //Don't start markov chain if its very recent
            //$display("%t Prefetcher start new chain with %h", $time, addr);
            chainNextToLookup <= cl;
            chainNumberToPrefetch <= fromInteger(valueOf(maxChainLength));
        end
    endmethod

endmodule

module mkMarkovPrefetcher(Prefetcher) provisos
(
    NumAlias#(maxChainLength, 2),
    Alias#(chainLengthT, Bit#(TLog#(TAdd#(maxChainLength,1))))
);
    Reg#(LineAddr) lastLastChildRequest <- mkReg(0);
    Reg#(LineAddr) lastChildRequest <- mkReg(0);
    TargetTable#(64, 8) targetTable <- mkTargetTable;

    // Stores how many prefetches we can still do in the current chain
    Reg#(chainLengthT) chainNumberToPrefetch <- mkReg(0); 
    Reg#(LineAddr) chainNextToPrefetch <- mkReg(?);

    method Action reportAccess(Addr addr, HitOrMiss hitMiss);
        let cl = getLineAddr(addr);
        if (cl != lastChildRequest + 1 && cl != lastChildRequest && cl != lastChildRequest - 1) begin
            $display("%t Prefetcher report %s add target entry from addr %h to addr %h", 
                $time, hitMiss == HIT ? "HIT" : "MISS", Addr'{lastChildRequest, '0}, addr);
            targetTable.set(lastChildRequest, cl);
        end
        lastChildRequest <= cl;
        lastLastChildRequest <= lastChildRequest;

        if (lastLastChildRequest != cl) begin 
            //Don't start markov chain if its very recent
            let x <- targetTable.getAndRemove(cl);
            if (x matches tagged Valid .nextCl) begin
                //Start new prefetch chain
                $display("%t Prefetcher start new chain with %h", $time, addr);
                chainNextToPrefetch <= nextCl;
                chainNumberToPrefetch <= fromInteger(valueOf(maxChainLength));
            end
        end
    endmethod

    method ActionValue#(Addr) getNextPrefetchAddr if 
        (chainNumberToPrefetch != 0);
        
        let x <- targetTable.getAndRemove(chainNextToPrefetch);
        if (x matches tagged Valid .nextCl) begin
            chainNumberToPrefetch <= chainNumberToPrefetch - 1;
            chainNextToPrefetch <= nextCl;
        end
        else begin
            //End chain here
            chainNumberToPrefetch <= 0;
        end
        Addr retAddr = {chainNextToPrefetch, '0};
        $display("%t Prefetcher getNextPrefetchAddr requesting chain entry %h", $time, retAddr);
        return retAddr; 
    endmethod
endmodule

module mkBlockPrefetcher(Prefetcher) provisos (
    NumAlias#(numLinesEachWay, 2),
    Alias#(lineCountT, Bit#(TLog#(TAdd#(numLinesEachWay, 1))))
);
    Reg#(Bool) nextIsForward <- mkReg(?);
    Reg#(LineAddr) prefetchAround <- mkReg(?);
    Reg#(lineCountT) linesEachWayPrefetched <- mkReg(?);
    method Action reportAccess(Addr addr, HitOrMiss hitMiss);
        if (hitMiss == MISS) begin
            $display("%t Prefetcher report MISS %h", $time, addr);
            nextIsForward <= True;
            prefetchAround <= getLineAddr(addr);
            linesEachWayPrefetched <= 0;
        end
        else 
            $display("%t Prefetcher report HIT %h", $time, addr);
    endmethod
    method ActionValue#(Addr) getNextPrefetchAddr if (linesEachWayPrefetched != fromInteger(valueOf(numLinesEachWay)));
        nextIsForward <= !nextIsForward;
        if (nextIsForward) begin
            Addr retAddr = {prefetchAround + (extend(linesEachWayPrefetched)+1), 0};
            $display("%t Prefetcher getNextPrefetchAddr requesting forward %h", $time, retAddr);
            return retAddr;
        end
        else begin
            Addr retAddr = {prefetchAround - (extend(linesEachWayPrefetched)+1), 0};
            $display("%t Prefetcher getNextPrefetchAddr requesting backward %h", $time, retAddr);
            linesEachWayPrefetched <= linesEachWayPrefetched + 1;
            return retAddr;
        end
    endmethod

endmodule

interface PCPrefetcher;
    (* always_ready *)
    method Action reportAccess(Addr addr, Bit#(16) pcHash, HitOrMiss hitMiss);
    method ActionValue#(Addr) getNextPrefetchAddr();
endinterface

module mkPCPrefetcherAdapter#(module#(Prefetcher) mkPrefetcher)(PCPrefetcher);
    let p <- mkPrefetcher;
    method Action reportAccess(Addr addr, Bit#(16) pcHash, HitOrMiss hitMiss);
        p.reportAccess(addr, hitMiss);
    endmethod
    method ActionValue#(Addr) getNextPrefetchAddr;
        let x <- p.getNextPrefetchAddr;
        return x;
    endmethod
endmodule

module mkDoNothingPCPrefetcher(PCPrefetcher);
    method Action reportAccess(Addr addr, Bit#(16) pcHash, HitOrMiss hitMiss);
    endmethod
    method ActionValue#(Addr) getNextPrefetchAddr if (False);
        return 64'h0000000080000080;
    endmethod
endmodule

module mkPrintPCPrefetcher(PCPrefetcher);
    method Action reportAccess(Addr addr, Bit#(16) pcHash, HitOrMiss hitMiss);
        if (hitMiss == HIT)
            $display("%t PCPrefetcher report HIT %h", $time, addr);
        else
            $display("%t PCPrefetcher report MISS %h", $time, addr);
    endmethod
    method ActionValue#(Addr) getNextPrefetchAddr if (False);
        return 64'h0000000080000080;
    endmethod
endmodule

typedef enum {
  EMPTY = 2'b00, INIT = 2'b01, TRANSIENT = 2'b10, STEADY = 2'b11
} StrideState deriving (Bits, Eq, FShow);

typedef struct {
    Bit#(12) lastAddr; 
    Bit#(13) stride;
    StrideState state;
    Bit#(4) cLinesPrefetched; //Stores how many cache lines have been prefetched for this instruction
} StrideEntry deriving (Bits, Eq, FShow);

module mkStridePCPrefetcher(PCPrefetcher)
provisos(
    NumAlias#(historyLen, 8),
    NumAlias#(strideTableSize, 64),
    NumAlias#(cLinesAheadToPrefetch, 3), // TODO fetch more if have repeatedly hit an entry, and if stride big
    Alias#(strideTableIndexT, Bit#(TLog#(strideTableSize))),
    Alias#(historyVecIndexT, Bit#(TLog#(historyLen)))
    );
    Reg#(Vector#(historyLen, Tuple2#(strideTableIndexT, Addr))) historyVec <- mkReg(replicate(?));
    Vector#(strideTableSize, Reg#(StrideEntry)) strideTable <- replicateM(mkReg(unpack(0)));

    function Maybe#(historyVecIndexT) getNextPrefetchHistoryIndex;
        function Bool canPrefetch(Tuple2#(strideTableIndexT, Addr) entry);
            strideTableIndexT idx = tpl_1(entry);
            return (strideTable[idx].state == STEADY && 
                strideTable[idx].cLinesPrefetched != 
                    fromInteger(valueof(cLinesAheadToPrefetch)));
        endfunction

        //Find first entry that allows more prefetches
        case (findIndex(canPrefetch, historyVec)) matches 
            tagged Valid .historyIdx: return Valid(pack(historyIdx));
            Invalid: return Invalid;
        endcase
    endfunction

    method Action reportAccess(Addr addr, Bit#(16) pcHash, HitOrMiss hitMiss);
        //Find slot in vector
        //if miss and slot empty
        //if slot init, put address, stride and move to transit
        //if slot transit or steady, verify stride, and move to steady
        //    also put last_prefetched
        //if stride wrong, move to transit
        strideTableIndexT index = truncate(pcHash);
        Reg#(StrideEntry) se = strideTable[index];
        StrideEntry seNext = se;
        Bit#(13) observedStride = {1'b0, addr[11:0]} - {1'b0, se.lastAddr};
        $writeh("%t Stride Prefetcher reportAccess ", $time,
            fshow(hitMiss), " ", addr,
            ". Entry ", index, " state is ", fshow(se.state));
        if (se.state == EMPTY) begin
            if (hitMiss == MISS) begin 
                seNext.lastAddr = truncate(addr);
                seNext.state = INIT;
                $display(", allocate entry");
            end
            else begin
                $display(", ignore");
            end
        end 
        else if (se.state == INIT && observedStride != 0) begin
            seNext.stride = observedStride;
            seNext.state = TRANSIENT;
            seNext.lastAddr = truncate(addr);
            $display(", set stride to %h", seNext.stride);
        end
        else if ((se.state == TRANSIENT || se.state == STEADY) && observedStride != 0) begin
            if (observedStride == se.stride) begin
                if (se.state == TRANSIENT) begin
                    //Here we transition from TRANSIENT to STEADY, so init this field
                    seNext.cLinesPrefetched = 0;
                end
                else begin
                    //state == STEADY
                    if (se.lastAddr[11:6] != addr[11:6]) begin
                        //This means we have crossed a cache line since last access
                        seNext.cLinesPrefetched = 
                            (se.cLinesPrefetched == 0) ? 0 : se.cLinesPrefetched - 1;
                    end
                end
                seNext.state = STEADY;
                seNext.lastAddr = truncate(addr);
                historyVec <= shiftInAt0(historyVec, tuple2(index, addr));
                $display(", stride %h is confirmed!", seNext.stride);
            end
            else begin
                seNext.state = TRANSIENT;
                seNext.stride = observedStride;
                seNext.lastAddr = truncate(addr);
                $display(", old stride is broken! New stride: %h", seNext.stride);
            end
        end
        else
            $display("");
        se <= seNext;
    endmethod

    method ActionValue#(Addr) getNextPrefetchAddr if 
        (getNextPrefetchHistoryIndex() matches tagged Valid .historyIdx);
        match {.strideIdx, .fullAddr} = historyVec[historyIdx];
        Reg#(StrideEntry) se = strideTable[strideIdx];
        se.cLinesPrefetched <= se.cLinesPrefetched + 1;
        Bit#(13) strideToUse;
        Bit#(13) cLineSize = fromInteger(valueof(DataSz));
        if (se.stride[12] == 1 && se.stride > -cLineSize) begin
            //stride is negative and jumps less than one cline
            strideToUse = -cLineSize;
        end
        else if (se.stride[12] == 0 && se.stride < cLineSize) begin
            //stride is positive and jumps less than one cline
            strideToUse = cLineSize;
        end 
        else begin
            strideToUse = se.stride;
        end

        let reqAddr = fullAddr + 
            (signExtend(strideToUse) * zeroExtend(se.cLinesPrefetched + 1));
        $display("%t Stride Prefetcher getNextPrefetchAddr requesting %h for entry %h", $time, reqAddr, strideIdx);
        return reqAddr;
    endmethod

endmodule

module mkBRAMStridePCPrefetcher(PCPrefetcher)
provisos(
    NumAlias#(strideTableSize, 64),
    NumAlias#(cLinesAheadToPrefetch, 3), // TODO fetch more if have repeatedly hit an entry, and if stride big
    Alias#(strideTableIndexT, Bit#(TLog#(strideTableSize)))
    );
    //Vector#(strideTableSize, Reg#(StrideEntry)) strideTable <- replicateM(mkReg(unpack(0)));
    RWBramCore#(strideTableIndexT, StrideEntry) strideTable <- mkRWBramCoreForwarded;
    FIFOF#(Tuple3#(Addr, Bit#(16), HitOrMiss)) memAccesses <- mkSizedBypassFIFOF(8);
    Reg#(Tuple3#(Addr, Bit#(16), HitOrMiss)) rdRespEntry <- mkReg(?);

    FIFO#(Addr) addrToPrefetch <- mkSizedFIFO(8);
    FIFO#(Tuple3#(StrideEntry, Addr, Bit#(16))) strideEntryForPrefetch <- mkBypassFIFO();
    Reg#(Maybe#(Bit#(4))) cLinesPrefetchedLatest <- mkReg(?);
    PulseWire holdReadReq <- mkPulseWire;

    rule sendReadReq if (!holdReadReq);
        match {.addr, .pcHash, .hitMiss} = memAccesses.first;
        $display("%t Sending read req for %h!", $time, pcHash);
        strideTable.rdReq(truncate(pcHash));
        rdRespEntry <= memAccesses.first;
        memAccesses.deq;
    endrule


    rule updateStrideEntry;
        //Find slot in vector
        //if miss and slot empty
        //if slot init, put address, stride and move to transit
        //if slot transit or steady, verify stride, and move to steady
        //    also put last_prefetched
        //if stride wrong, move to transit
        match {.addr, .pcHash, .hitMiss} = rdRespEntry;
        strideTableIndexT index = truncate(pcHash);
        StrideEntry se = strideTable.rdResp;
        strideTable.deqRdResp;
        StrideEntry seNext = se;
        Bit#(13) observedStride = {1'b0, addr[11:0]} - {1'b0, se.lastAddr};
        $writeh("%t Stride Prefetcher updateStrideEntry ", $time,
            fshow(hitMiss), " ", addr,
            ". Entry ", index, " state is ", fshow(se.state));
        if (se.state == EMPTY) begin
            if (hitMiss == MISS) begin 
                seNext.lastAddr = truncate(addr);
                seNext.state = INIT;
                $display(", allocate entry");
            end
            else begin
                $display(", ignore");
            end
        end 
        else if (se.state == INIT && observedStride != 0) begin
            seNext.stride = observedStride;
            seNext.state = TRANSIENT;
            seNext.lastAddr = truncate(addr);
            $display(", set stride to %h", seNext.stride);
        end
        else if ((se.state == TRANSIENT || se.state == STEADY) && observedStride != 0) begin
            if (observedStride == se.stride) begin
                if (se.state == TRANSIENT) begin
                    //Here we transition from TRANSIENT to STEADY, so init this field
                    seNext.cLinesPrefetched = 0;
                end
                else begin
                    //state == STEADY
                    if (se.lastAddr[11:6] != addr[11:6]) begin
                        //This means we have crossed a cache line since last access
                        seNext.cLinesPrefetched = 
                            (se.cLinesPrefetched == 0) ? 0 : se.cLinesPrefetched - 1;
                    end
                end
                seNext.state = STEADY;
                seNext.lastAddr = truncate(addr);
                $display(", stride %h is confirmed!", seNext.stride);
            end
            else begin
                seNext.state = TRANSIENT;
                seNext.stride = observedStride;
                seNext.lastAddr = truncate(addr);
                $display(", old stride is broken! New stride: %h", seNext.stride);
            end
        end
        else
            $display("");
        
        strideEntryForPrefetch.enq(tuple3(seNext, addr, pcHash));
    endrule

    rule createPrefetchRequests;
        match {.se, .addr, .pcHash} = strideEntryForPrefetch.first;
        //If this rule is looping, then we'll have a valid cLinesPrefetchedLatest
        Bit#(4) cLinesPrefetched = fromMaybe(se.cLinesPrefetched, cLinesPrefetchedLatest);

        if (se.state == STEADY && 
            cLinesPrefetched != 
            fromInteger(valueof(cLinesAheadToPrefetch))) begin
            //can prefetch
            
            Bit#(13) strideToUse;
            Bit#(13) cLineSize = fromInteger(valueof(DataSz));
            if (se.stride[12] == 1 && se.stride > -cLineSize) begin
                //stride is negative and jumps less than one cline
                strideToUse = -cLineSize;
            end
            else if (se.stride[12] == 0 && se.stride < cLineSize) begin
                //stride is positive and jumps less than one cline
                strideToUse = cLineSize;
            end 
            else begin
                strideToUse = se.stride;
            end

            let reqAddr = addr + 
                (signExtend(strideToUse) * zeroExtend(cLinesPrefetched + 1));

            addrToPrefetch.enq(reqAddr);
            // We will still be processing this StrideEntry next cycle, 
            // so hold off any potential read requests until we do a writeback
            holdReadReq.send();
            cLinesPrefetchedLatest <= Valid(cLinesPrefetched + 1);
            $display("%t Stride Prefetcher getNextPrefetchAddr requesting %h for entry %h", $time, reqAddr, pcHash[7:0]);
        end
        else begin
            //cant prefetch
            $display("%t Stride Prefetcher no possible prefetch for entry %h", $time, strideTableIndexT'(truncate(pcHash)));
            strideEntryForPrefetch.deq;
            se.cLinesPrefetched = cLinesPrefetched;
            cLinesPrefetchedLatest <= Invalid;
            strideTable.wrReq(truncate(pcHash), se);
        end
    endrule

    method Action reportAccess(Addr addr, Bit#(16) pcHash, HitOrMiss hitMiss);
        memAccesses.enq(tuple3 (addr, pcHash, hitMiss));
    endmethod

    method ActionValue#(Addr) getNextPrefetchAddr;
        addrToPrefetch.deq;
        return addrToPrefetch.first;
    endmethod

endmodule
module mkL1IPrefetcher(Prefetcher);
`ifdef INSTR_PREFETCHER_IN_L1
    `ifdef INSTR_PREFETCHER_NEXT_LINE_ON_ALL
        let m <-  mkNextLineOnAllPrefetcher;
    `elsif INSTR_PREFETCHER_NEXT_LINE_ON_MISS
        let m <-  mkNextLineOnMissPrefetcher;
    `elsif INSTR_PREFETCHER_SINGLE_WINDOW
        let m <-  mkSingleWindowPrefetcher;
    `elsif INSTR_PREFETCHER_SINGLE_WINDOW_TARGET
        let m <-  mkBRAMSingleWindowTargetPrefetcher;
    `elsif INSTR_PREFETCHER_MULTI_WINDOW
        let m <-  mkMultiWindowPrefetcher;
    `elsif INSTR_PREFETCHER_MULTI_WINDOW_TARGET
        let m <-  mkBRAMMultiWindowTargetPrefetcher;
    `endif
    //let m <- mkAlwaysRequestPrefetcher;
    //let m <- mkPrintPrefetcher;
`else
    let m <- mkDoNothingPrefetcher;
`endif
    return m;
endmodule

module mkLLIPrefetcherInL1I(Prefetcher);
`ifdef INSTR_PREFETCHER_IN_L1LL
    `ifdef INSTR_PREFETCHER_NEXT_LINE_ON_ALL
        let m <-  mkNextLineOnAllPrefetcher;
    `elsif INSTR_PREFETCHER_NEXT_LINE_ON_MISS
        let m <-  mkNextLineOnMissPrefetcher;
    `elsif INSTR_PREFETCHER_SINGLE_WINDOW
        let m <-  mkSingleWindowPrefetcher;
    `elsif INSTR_PREFETCHER_SINGLE_WINDOW_TARGET
        let m <-  mkBRAMSingleWindowTargetPrefetcher;
    `elsif INSTR_PREFETCHER_MULTI_WINDOW
        let m <-  mkMultiWindowPrefetcher;
    `elsif INSTR_PREFETCHER_MULTI_WINDOW_TARGET
        let m <-  mkBRAMMultiWindowTargetPrefetcher;
    `endif
    //let m <- mkAlwaysRequestPrefetcher;
    //let m <- mkPrintPrefetcher;
`else
    let m <- mkDoNothingPrefetcher;
`endif
    return m;
endmodule

module mkLLIPrefetcher(Prefetcher);
`ifdef INSTR_PREFETCHER_IN_LL
    `ifdef INSTR_PREFETCHER_NEXT_LINE_ON_ALL
        let m <-  mkNextLineOnAllPrefetcher;
    `elsif INSTR_PREFETCHER_NEXT_LINE_ON_MISS
        let m <-  mkNextLineOnMissPrefetcher;
    `elsif INSTR_PREFETCHER_SINGLE_WINDOW
        let m <-  mkSingleWindowPrefetcher;
    `elsif INSTR_PREFETCHER_SINGLE_WINDOW_TARGET
        let m <-  mkBRAMSingleWindowTargetPrefetcher;
    `elsif INSTR_PREFETCHER_MULTI_WINDOW
        let m <-  mkMultiWindowPrefetcher;
    `elsif INSTR_PREFETCHER_MULTI_WINDOW_TARGET
        let m <-  mkBRAMMultiWindowTargetPrefetcher;
    `endif
    //let m <- mkAlwaysRequestPrefetcher;
    //let m <- mkPrintPrefetcher;
`else
    let m <- mkDoNothingPrefetcher;
`endif
    return m;
endmodule

module mkL1DPrefetcher(PCPrefetcher);
`ifdef DATA_PREFETCHER_IN_L1
    `ifdef DATA_PREFETCHER_BLOCK
        let m <- mkPCPrefetcherAdapter(mkBlockPrefetcher);
    `elsif DATA_PREFETCHER_STRIDE
        let m <- mkBRAMStridePCPrefetcher;
    `elsif DATA_PREFETCHER_MARKOV
        let m <- mkPCPrefetcherAdapter(mkBRAMMarkovPrefetcher);
    `endif
    //let m <- mkPCPrefetcherAdapter(mkAlwaysRequestPrefetcher);
`else 
    let m <- mkPCPrefetcherAdapter(mkDoNothingPrefetcher);
`endif
    return m;
endmodule

module mkLLDPrefetcherInL1D(PCPrefetcher);
`ifdef DATA_PREFETCHER_IN_L1LL
    `ifdef DATA_PREFETCHER_BLOCK
        let m <- mkPCPrefetcherAdapter(mkBlockPrefetcher);
    `elsif DATA_PREFETCHER_STRIDE
        let m <- mkBRAMStridePCPrefetcher;
    `elsif DATA_PREFETCHER_MARKOV
        let m <- mkPCPrefetcherAdapter(mkBRAMMarkovPrefetcher);
    `endif
    //let m <- mkPCPrefetcherAdapter(mkAlwaysRequestPrefetcher);
`else 
    let m <- mkPCPrefetcherAdapter(mkDoNothingPrefetcher);
`endif
    return m;
endmodule

module mkLLDPrefetcher(Prefetcher);
`ifdef DATA_PREFETCHER_IN_LL
    `ifdef DATA_PREFETCHER_BLOCK
        let m <- mkBlockPrefetcher;
    `elsif DATA_PREFETCHER_STRIDE
        doAssert(False, "Illegal data prefetcher type for LL cache!")
    `elsif DATA_PREFETCHER_MARKOV
        let m <- mkBRAMMarkovPrefetcher;
    `endif
    //let m <- mkPCPrefetcherAdapter(mkAlwaysRequestPrefetcher);
`else 
    let m <- mkDoNothingPrefetcher;
`endif
    return m;
endmodule