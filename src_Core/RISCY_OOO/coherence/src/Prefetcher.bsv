import ISA_Decls   :: *;
import CrossBar::*;
import GetPut::*;
import RWBramCore::*;
import FIFO::*;
import Fifos::*;
import FIFOF::*;
import SpecialFIFOs :: *;
import Ehr::*;
import CacheUtils::*;
import CCTypes::*;
import Types::*;
import Vector::*;
import BuildVector::*;
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
        NumAlias#(nextLinesOnMiss, 1),
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
        else if (hitMiss == MISS) begin
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

module mkSingleWindowL1LLPrefetcher(Prefetcher);
    Integer cacheLinesInRange = 2;
    Reg#(LineAddr) rangeEnd <- mkReg(0); //Points to one CLine after end of range
    Reg#(LineAddr) nextToAsk <- mkReg(0);
    method Action reportAccess(Addr addr, HitOrMiss hitMiss);
        let cl = getLineAddr(addr);
        if (rangeEnd - fromInteger(cacheLinesInRange) - 1 <= cl && 
            cl < rangeEnd) begin

            let nextEnd = cl + fromInteger(cacheLinesInRange) + 1;
            $display("%t Prefetcher report HIT %h, moving window end to %h", $time, addr, Addr'{nextEnd, '0});
            rangeEnd <= nextEnd;
        end
        else if (hitMiss == MISS) begin
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
    method ActionValue#(Maybe#(LineAddr)) readResp(Bool clearEntry);
endinterface

module mkTargetTableBRAM(TargetTableBRAM#(narrowTableSize, wideTableSize)) provisos
(
    NumAlias#(narrowTableIdxBits, TLog#(narrowTableSize)),
    NumAlias#(wideTableIdxBits, TLog#(wideTableSize)),
    NumAlias#(narrowTableTagBits, TSub#(24, narrowTableIdxBits)),
    NumAlias#(wideTableTagBits, TSub#(24, wideTableIdxBits)),
    NumAlias#(narrowDistanceBits, 16),
    NumAlias#(narrowMaxDistanceAbs, TExp#(TSub#(narrowDistanceBits, 1))),
    Alias#(narrowTargetEntryT, NarrowTargetEntry#(narrowTableTagBits, narrowDistanceBits)),
    Alias#(wideTargetEntryT, WideTargetEntry#(wideTableTagBits)),
    Add#(a__, TLog#(narrowTableSize), 32),
    Add#(b__, TLog#(narrowTableSize), 58),
    Add#(c__, TLog#(wideTableSize), 32),
    Add#(d__, TLog#(wideTableSize), 58)
);
    RWBramCore#(Bit#(narrowTableIdxBits), Maybe#(narrowTargetEntryT)) narrowTable <- mkRWBramCore;
    RWBramCore#(Bit#(wideTableIdxBits), Maybe#(wideTargetEntryT)) wideTable <- mkRWBramCore;
    Reg#(LineAddr) readReqLineAddr <- mkReg(?); 

    method Action writeReq(LineAddr prevAddr, LineAddr currAddr);
        let distance = currAddr - prevAddr;
        Bit#(32) prevAddrHash = hash(prevAddr);
        if (abs(distance) < fromInteger(valueOf(narrowMaxDistanceAbs))) begin
            //Store in narrow table
            narrowTargetEntryT entry;
            entry.tag = prevAddrHash[23:valueOf(narrowTableIdxBits)];
            entry.distance = truncate(distance);
            Bit#(narrowTableIdxBits) idx = truncate(prevAddrHash);
            narrowTable.wrReq(idx, tagged Valid entry);
        end
        else begin
            //Store in wide table
            wideTargetEntryT entry;
            entry.tag = prevAddrHash[23:valueOf(wideTableIdxBits)];
            entry.target = currAddr;
            Bit#(wideTableIdxBits) idx = truncate(prevAddrHash);
            wideTable.wrReq(idx, tagged Valid entry);
        end
    endmethod

    method Action readReq(LineAddr addr);
        Bit#(32) addrHash = hash(addr);
        Bit#(narrowTableIdxBits) narrowIdx = truncate(addrHash);
        narrowTable.rdReq(narrowIdx);
        Bit#(wideTableIdxBits) wideIdx = truncate(addrHash);
        wideTable.rdReq(wideIdx);
        readReqLineAddr <= addr;
    endmethod

    method ActionValue#(Maybe#(LineAddr)) readResp(Bool clearEntry);
        // Returns the read response and if a table had a hit, 
        // sends a write request to clear the entry in that table
        narrowTable.deqRdResp;
        wideTable.deqRdResp;
        let addr = readReqLineAddr;
        Bit#(32) addrHash = hash(addr);
        Bit#(narrowTableIdxBits) narrowIdx = truncate(addrHash);
        Bit#(wideTableIdxBits) wideIdx = truncate(addrHash);
        if (narrowTable.rdResp matches tagged Valid .entry 
            &&& entry.tag == addrHash[23:valueOf(narrowTableIdxBits)]) begin
            if (clearEntry) begin
                //narrowTable.wrReq(narrowIdx, Invalid); 
            end
            $display("%t found narrow table entry %h", $time, addr + signExtend(pack(entry.distance)));
            return Valid(addr + signExtend(pack(entry.distance)));
        end
        else if (wideTable.rdResp matches tagged Valid .entry 
            &&& entry.tag == addrHash[23:valueOf(wideTableIdxBits)]) begin
            if (clearEntry) begin
                //wideTable.wrReq(wideIdx, Invalid); 
            end
            $display("%t found wide table entry %h", $time, entry.target);
            return Valid(entry.target);
        end
        else begin
            return Invalid;
        end
        
    endmethod
endmodule

interface TargetTableDouble#(numeric type narrowTableSize, numeric type wideTableSize);
    method Action sendReadWriteReq(LineAddr addr, HitOrMiss hitMiss);
    method ActionValue#(Tuple2#(Maybe#(LineAddr), Maybe#(LineAddr))) readResp();
endinterface

module mkTargetTableDouble(TargetTableDouble#(narrowTableSize, wideTableSize)) provisos
(
    NumAlias#(narrowTableIdxBits, TLog#(narrowTableSize)),
    NumAlias#(wideTableIdxBits, TLog#(wideTableSize)),
    NumAlias#(narrowTableTagBits, TSub#(24, narrowTableIdxBits)),
    NumAlias#(wideTableTagBits, TSub#(24, wideTableIdxBits)),
    NumAlias#(narrowDistanceBits, 10),
    NumAlias#(narrowMaxDistanceAbs, TExp#(TSub#(narrowDistanceBits, 1))),
    Alias#(narrowTargetEntryT, NarrowTargetEntry#(narrowTableTagBits, narrowDistanceBits)),
    Alias#(wideTargetEntryT, WideTargetEntry#(wideTableTagBits)),
    Add#(a__, TLog#(narrowTableSize), 32),
    Add#(b__, TLog#(narrowTableSize), 58),
    Add#(c__, TLog#(wideTableSize), 32),
    Add#(d__, TLog#(wideTableSize), 58)
);

    //on any request, read all 4 tables. get prefetches. if it's a miss save both MRU entries.
    //on a miss, perform a regular table write to the MRU table. if it's a narrow write, write the old MRU narrow entry to the LRU narrow table.
    //one method, readReq, with parameter isMiss. sends read requests to all 4 tables.
    //also saves read address
    //one method setMRU.
    // sends a write request with the previous miss address to the current miss address.
    // if narrow table write, then also write the old MRU narrow entry to the LRU narrow table.
    //one method, getPrefetches, which also saves all results if isMiss true.
    RWBramCore#(Bit#(narrowTableIdxBits), Maybe#(narrowTargetEntryT)) narrowTableMRU <- mkRWBramCoreForwarded;
    RWBramCore#(Bit#(wideTableIdxBits), Maybe#(wideTargetEntryT)) wideTableMRU <- mkRWBramCoreForwarded;
    RWBramCore#(Bit#(narrowTableIdxBits), Maybe#(narrowTargetEntryT)) narrowTableLRU <- mkRWBramCoreForwarded;
    RWBramCore#(Bit#(wideTableIdxBits), Maybe#(wideTargetEntryT)) wideTableLRU <- mkRWBramCoreForwarded;
    Reg#(LineAddr) readReqLineAddr <- mkReg(0); 
    Reg#(HitOrMiss) readReqHitMiss <- mkReg(HIT);

    Reg#(LineAddr) lastMissAddr <- mkReg(0);
    Reg#(Maybe#(wideTargetEntryT)) lastMissWideMRUEntry <- mkReg(unpack(0));
    Reg#(Maybe#(narrowTargetEntryT)) lastMissNarrowMRUEntry <- mkReg(unpack(0));
    Reg#(Maybe#(wideTargetEntryT)) lastMissWideLRUEntry <- mkReg(unpack(0));
    Reg#(Maybe#(narrowTargetEntryT)) lastMissNarrowLRUEntry <- mkReg(unpack(0));

    function Bool narrowTagMatch(Bit#(32) addrHash, narrowTargetEntryT narrow);
        return narrow.tag == addrHash[23:valueOf(narrowTableIdxBits)];
    endfunction

    function Bool narrowTagMatchMaybe(Bit#(32) addrHash, Maybe#(narrowTargetEntryT) narrowMaybe);
        if (narrowMaybe matches tagged Valid .narrow &&& narrowTagMatch(addrHash, narrow))
            return True;
        else
            return False;
    endfunction

    function Bool wideTagMatch(Bit#(32) addrHash, wideTargetEntryT wide);
        return wide.tag == addrHash[23:valueOf(wideTableIdxBits)];
    endfunction

    function Bool wideTagMatchMaybe(Bit#(32) addrHash, Maybe#(wideTargetEntryT) wideMaybe);
        if (wideMaybe matches tagged Valid .wide &&& wideTagMatch(addrHash, wide))
            return True;
        else 
            return False;
    endfunction


    function ActionValue#(Maybe#(LineAddr))
        checkReadResp(LineAddr addr, Bit#(32) addrHash, Maybe#(narrowTargetEntryT) narrowWrapped, Maybe#(wideTargetEntryT) wideWrapped);
    actionvalue
        if (narrowWrapped matches tagged Valid .narrow
            &&& narrowTagMatch(addrHash, narrow)) begin
            //$display("%t found narrow table entry %h", $time, addr + signExtend(pack(narrow.distance)));
            return Valid(addr + signExtend(pack(narrow.distance)));
        end
        else if (wideWrapped matches tagged Valid .wide
            &&& wideTagMatch(addrHash, wide)) begin
            //$display("%t found wide table entry %h", $time, wide.target);
            return Valid(wide.target);
        end
        else
            return Invalid;
    endactionvalue
    endfunction

    function Action updateTables(
                                Bit#(32) lastMissAddrHash,
                                Bit#(idxBits) idx,
                                RWBramCore#(Bit#(idxBits), Maybe#(otherEntryT)) otherMRU,
                                RWBramCore#(Bit#(idxBits), Maybe#(otherEntryT)) otherLRU,
                                function Bool otherTagMatch(Bit#(32) addrHash, Maybe#(otherEntryT) entry),
                                function Bool myTagMatch(Bit#(32) addrHash, Maybe#(myEntryT) entry),
                                Maybe#(otherEntryT) otherMRUEntry,
                                Maybe#(otherEntryT) otherLRUEntry,
                                Maybe#(myEntryT) myMRUEntry
                                );
    action
    //Am inserting a new table entry into "my" table.
    //So need to re-order the entries in the "other" table.
    //To maintain the property that can't have both a narrow and wide entry in the same recency table.
        if (otherTagMatch(lastMissAddrHash, otherMRUEntry)) begin
            //If MRU other entry matches
            if (otherTagMatch(lastMissAddrHash, otherLRUEntry)) begin
                //If LRU other entry matches
                // Shift other down
                otherMRU.wrReq(idx, Invalid);
                otherLRU.wrReq(idx, otherMRUEntry);
            end
            else begin
                //If LRU other entry doesnt match
                // Switch other entries around
                otherMRU.wrReq(idx, otherLRUEntry);
                otherLRU.wrReq(idx, otherMRUEntry);
            end
        end
        else begin
            //If other MRU entry doesn't match
            if (otherTagMatch(lastMissAddrHash, otherLRUEntry) &&
                myTagMatch(lastMissAddrHash, myMRUEntry)) begin

                //will shift my MRU entry down, so
                // invalidate other LRU entry
                otherLRU.wrReq(idx, Invalid);
            end
        end
    endaction
    endfunction

    function Action writeMissEntry(LineAddr currAddr);
    action
        let distance = currAddr - lastMissAddr;
        Bit#(32) lastMissAddrHash = hash(lastMissAddr);
        $display("%t Recording miss from %x to %x", $time, lastMissAddr, currAddr);
        if (abs(distance) < fromInteger(valueOf(narrowMaxDistanceAbs))) begin
            //Store lastMissAddr -> currAddr in narrow MRU table

            Bit#(narrowTableIdxBits) idx = truncate(lastMissAddrHash);
            narrowTargetEntryT entry;
            entry.tag = lastMissAddrHash[23:valueOf(narrowTableIdxBits)];
            entry.distance = truncate(distance);

            if (Valid(entry) != lastMissNarrowMRUEntry) begin
                //$display("%t Recording miss -- modifying narrow table", $time);
                //Maintain the property that one address can only have 
                // at most 2 of 4 table entries for it.
                //Shift narrow table entries down, storing in MRU.
                //But only if the entry was not already the MRU entry.
                narrowTableMRU.wrReq(idx, Valid(entry));
                narrowTableLRU.wrReq(idx, lastMissNarrowMRUEntry);
                Bit#(wideTableIdxBits) wideIdx = truncate(lastMissAddrHash);
                updateTables(lastMissAddrHash, wideIdx, wideTableMRU, wideTableLRU, wideTagMatchMaybe, narrowTagMatchMaybe,
                    lastMissWideMRUEntry, lastMissWideLRUEntry, lastMissNarrowMRUEntry);
            end
        end
        else begin
            //Store lastMissAddr -> currAddr in wide MRU table
            wideTargetEntryT entry;
            Bit#(wideTableIdxBits) idx = truncate(lastMissAddrHash);
            entry.tag = lastMissAddrHash[23:valueOf(wideTableIdxBits)];
            entry.target = currAddr;

            if (Valid(entry) != lastMissWideMRUEntry) begin
                //$display("%t Recording miss -- modifying wide table", $time);
                wideTableMRU.wrReq(idx, Valid(entry));
                wideTableLRU.wrReq(idx, lastMissWideMRUEntry);
                Bit#(narrowTableIdxBits) narrowIdx = truncate(lastMissAddrHash);
                updateTables(lastMissAddrHash, narrowIdx, narrowTableMRU, narrowTableLRU, narrowTagMatchMaybe, wideTagMatchMaybe,
                    lastMissNarrowMRUEntry, lastMissNarrowLRUEntry, lastMissWideMRUEntry);
            end
        end
    endaction
    endfunction

    method Action sendReadWriteReq(LineAddr addr, HitOrMiss hitMiss);
        $display("%t send read write req for %x", $time, addr);
        Bit#(32) addrHash = hash(addr);
        Bit#(narrowTableIdxBits) narrowIdx = truncate(addrHash);
        narrowTableMRU.rdReq(narrowIdx);
        narrowTableLRU.rdReq(narrowIdx);
        Bit#(wideTableIdxBits) wideIdx = truncate(addrHash);
        wideTableMRU.rdReq(wideIdx);
        wideTableLRU.rdReq(wideIdx);
        readReqLineAddr <= addr;
        readReqHitMiss <= hitMiss;
    endmethod


    method ActionValue#(Tuple2#(Maybe#(LineAddr), Maybe#(LineAddr))) readResp();
        // Returns the read response and if a table had a hit, 
        // sends a write request to clear the entry in that table
        narrowTableMRU.deqRdResp;
        narrowTableLRU.deqRdResp;
        wideTableMRU.deqRdResp;
        wideTableLRU.deqRdResp;
        let addr = readReqLineAddr;
        Bit#(32) addrHash = hash(addr);
        if (readReqHitMiss == MISS) begin
            //Update the entries for the last miss to point to this one
            writeMissEntry(addr);
            //Save the raw table entries
            //$display("idx: %x", addrHash);
            //$display("%t Read resp: nMRU: ", fshow(narrowTableMRU.rdResp), "wMRU: ", fshow(wideTableMRU.rdResp));
            //$display("%t Read resp: nLRU: ", fshow(narrowTableLRU.rdResp), "wLRU: ", fshow(wideTableLRU.rdResp));
            lastMissWideMRUEntry <= wideTableMRU.rdResp;
            lastMissNarrowMRUEntry <= narrowTableMRU.rdResp;
            lastMissWideLRUEntry <= wideTableLRU.rdResp;
            lastMissNarrowLRUEntry <= narrowTableLRU.rdResp;
            lastMissAddr <= addr; // save for future use
        end
        let entryMRU <- checkReadResp(addr, addrHash, narrowTableMRU.rdResp, wideTableMRU.rdResp);
        let entryLRU <- checkReadResp(addr, addrHash, narrowTableLRU.rdResp, wideTableLRU.rdResp);
        Tuple2#(Maybe#(LineAddr), Maybe#(LineAddr)) retval = tuple2(entryMRU, entryLRU);
        $display("%t read resp for %x returning ", $time, addr, fshow(retval));
        return retval;
    endmethod
endmodule

module mkBRAMSingleWindowTargetPrefetcher(Prefetcher) provisos
(
    NumAlias#(numLastRequests, 16)
);
    Integer cacheLinesInRange = 1;
    Reg#(LineAddr) rangeEnd <- mkReg(0); //Points to one CLine after end of range
    Reg#(LineAddr) nextToAsk <- mkReg(0);

    Reg#(LineAddr) lastChildRequest <- mkReg(0);
    TargetTableBRAM#(1024, 128) targetTable <- mkTargetTableBRAM;
    FIFOF#(LineAddr) targetTableReadResp <- mkBypassFIFOF;

    Reg#(Vector#(numLastRequests, Bit#(32))) lastTargetRequests <- mkReg(replicate(0));

    rule sendReadReq;
        if (!elem(hash(lastChildRequest), lastTargetRequests)) begin 
            targetTable.readReq(lastChildRequest);
            $display("%t Prefetcher sending target read request for %h", $time, lastChildRequest);
            lastTargetRequests <= shiftInAt0(lastTargetRequests, hash(lastChildRequest));
        end
    endrule

    rule getReadResp;
        let res <- targetTable.readResp(False);
        if (res matches tagged Valid .cline) begin
            targetTableReadResp.enq(cline);
        end
    endrule

    method Action reportAccess(Addr addr, HitOrMiss hitMiss);
        let cl = getLineAddr(addr);
        $display("%t prefecher reportAccess", $time);
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

        if (hitMiss == MISS && cl != lastChildRequest + 1 && cl != lastChildRequest && cl != lastChildRequest - 1) begin
            //Try only recording table entries on a miss!
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

module mkBRAMMultiWindowTargetPrefetcher(Prefetcher)
provisos(
    NumAlias#(numWindows, 4),
    NumAlias#(numLastRequests, 16),
    Alias#(windowIdxT, Bit#(TLog#(numWindows)))
);
    Integer cacheLinesInRange = 2;
    Vector#(numWindows, Reg#(StreamEntry)) streams 
        <- replicateM(mkReg(StreamEntry {rangeEnd: '0, nextToAsk: '0}));
    Vector#(numWindows, Reg#(windowIdxT)) shiftReg <- genWithM(compose(mkReg, fromInteger));
    Reg#(LineAddr) lastChildRequest <- mkReg(0);

    TargetTableBRAM#(64, 8) targetTable <- mkTargetTableBRAM;
    FIFOF#(LineAddr) targetTableReadResp <- mkBypassFIFOF;
    Reg#(Vector#(numLastRequests, Bit#(32))) lastTargetRequests <- mkReg(replicate(0));

    rule sendReadReq;
        if (!elem(hash(lastChildRequest), lastTargetRequests)) begin 
            targetTable.readReq(lastChildRequest);
            $display("%t Prefetcher sending target read request for %h", $time, lastChildRequest);
            lastTargetRequests <= shiftInAt0(lastTargetRequests, hash(lastChildRequest));
        end
    endrule

    rule getReadResp;
        let res <- targetTable.readResp(False);
        if (res matches tagged Valid .cline) begin
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
        if (hitMiss == MISS && cl != lastChildRequest + 1 && cl != lastChildRequest && cl != lastChildRequest - 1) begin
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

module mkBRAMMarkovPrefetcher(Prefetcher) provisos
(
    NumAlias#(maxChainLength, 2),
    Alias#(chainLengthT, Bit#(TLog#(TAdd#(maxChainLength,1))))
);
    Reg#(LineAddr) lastLastChildRequest <- mkReg(0);
    Reg#(LineAddr) lastChildRequest <- mkReg(0);
    TargetTableBRAM#(65536, 4096) targetTable <- mkTargetTableBRAM;
    FIFOF#(LineAddr) targetTableReadResp <- mkBypassFIFOF;

    // Stores how many prefetches we can still do in the current chain
    Reg#(chainLengthT) chainNumberToPrefetch <- mkReg(0); 
    Reg#(LineAddr) chainNextToLookup <- mkReg(?);

    rule sendReadReq (chainNumberToPrefetch != 0);
        $display ("%t Prefetcher send read req for  %h", $time, Addr'{chainNextToLookup, '0});
        targetTable.readReq(chainNextToLookup);
    endrule

    (* descending_urgency = "getReadResp, sendReadReq" *)
    (* execution_order = "getReadResp, sendReadReq" *)
    rule getReadResp;
        let res <- targetTable.readResp(False);
        if (res matches tagged Valid .cline) begin
            targetTableReadResp.enq(cline);
            chainNextToLookup <= cline;
            chainNumberToPrefetch <= chainNumberToPrefetch - 1;
            $display("%t Prefetcher found read resp data! %h", $time, Addr'{cline, '0});
        end
        else begin
            $display("%t Prefetcher found read resp invalid!", $time);
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
        if (hitMiss == MISS && cl != lastChildRequest) begin
            //Test only tracking misses, to avoid double noise of too many requests, many to the same location
            $display("%t Prefetcher report %s add target entry from addr %h to addr %h", 
                $time, hitMiss == HIT ? "HIT" : "MISS", Addr'{lastChildRequest, '0}, Addr'{cl, '0});
            targetTable.writeReq(lastChildRequest, cl);
            lastChildRequest <= cl;
            lastLastChildRequest <= lastChildRequest;
        end

        if (hitMiss == MISS) begin 
            //Don't start markov chain if its very recent
            //$display("%t Prefetcher start new chain with %h", $time, addr);
            chainNextToLookup <= cl;
            chainNumberToPrefetch <= fromInteger(valueOf(maxChainLength));
        end
    endmethod

endmodule

module mkBRAMMarkovOnHitPrefetcher(Prefetcher) provisos
(
    NumAlias#(maxChainLength, 2),
    NumAlias#(numLastRequests, 32),
    Alias#(chainLengthT, Bit#(TLog#(TAdd#(maxChainLength,1))))
);
    Reg#(LineAddr) lastLastChildRequest <- mkReg(0);
    Reg#(LineAddr) lastChildRequest <- mkReg(0);
    Reg#(Vector#(numLastRequests, Bit#(32))) lastAddrRequests <- mkReg(replicate(0));
    TargetTableBRAM#(2048, 128) targetTable <- mkTargetTableBRAM;
    FIFOF#(LineAddr) targetTableReadResp <- mkBypassFIFOF;

    // Stores how many prefetches we can still do in the current chain
    Reg#(chainLengthT) chainNumberToPrefetch <- mkReg(0); 
    Reg#(LineAddr) chainNextToLookup <- mkReg(?);

    rule sendReadReq (chainNumberToPrefetch != 0);
        $display ("%t Prefetcher send read req for  %h", $time, Addr'{chainNextToLookup, '0});
        targetTable.readReq(chainNextToLookup);
    endrule

    (* descending_urgency = "getReadResp, sendReadReq" *)
    (* execution_order = "getReadResp, sendReadReq" *)
    rule getReadResp;
        let res <- targetTable.readResp(False);
        if (res matches tagged Valid .cline) begin
            targetTableReadResp.enq(cline);
            chainNextToLookup <= cline;
            chainNumberToPrefetch <= chainNumberToPrefetch - 1;
            $display("%t Prefetcher found read resp data! %h", $time, Addr'{cline, '0});
        end
        else begin
            $display("%t Prefetcher found read resp invalid!", $time);
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
        if (hitMiss == MISS && cl != lastChildRequest) begin
            //Test only tracking misses, to avoid double noise of too many requests, many to the same location
            $display("%t Prefetcher report %s add target entry from addr %h to addr %h", 
                $time, hitMiss == HIT ? "HIT" : "MISS", Addr'{lastChildRequest, '0}, Addr'{cl, '0});
            targetTable.writeReq(lastChildRequest, cl);
            lastChildRequest <= cl;
            lastLastChildRequest <= lastChildRequest;
        end

        if (!elem(hash(cl), lastAddrRequests)) begin 
            //Don't start markov chain if we started a markov chain with that address recently
            $display("%t Prefetcher start new chain with %h", $time, addr);
            lastAddrRequests <= shiftInAt0(lastAddrRequests, hash(cl));
            chainNextToLookup <= cl;
            chainNumberToPrefetch <= fromInteger(valueOf(maxChainLength));
        end
    endmethod

endmodule

module mkMarkovOnHit2Prefetcher(Prefetcher) provisos
(
    NumAlias#(maxChainLength, 1),
    NumAlias#(numLastRequests, 32),
    Alias#(chainLengthT, Bit#(TLog#(TAdd#(maxChainLength,1))))
);
    Reg#(LineAddr) lastChildRequest <- mkReg(0);
    Reg#(Vector#(numLastRequests, Bit#(32))) lastAddrRequests <- mkReg(replicate(0));
    TargetTableDouble#(2048, 256) targetTable <- mkTargetTableDouble;
    Fifo#(8, LineAddr) highPriorityPrefetches <- mkOverflowBypassFifo;
    Fifo#(8, LineAddr) lowPriorityPrefetches <- mkOverflowBypassFifo;

    //on reportAccess, read the current address in both tables, and save it
    // Next cycle, add any found next lines to prefetch queues. Have 2 prefetch queues, High and low priority.
    // If this was a miss, save as lastMissEntry
    // And issue a write of table[lastMissEntry] = thisMiss.
    // but checking for LRU and everything.

    rule addPrefetchesToQueues;
        match { .highPrio, .lowPrio } <- targetTable.readResp();
        if (highPrio matches tagged Valid .cl)
            highPriorityPrefetches.enq(cl);
        if (lowPrio matches tagged Valid .cl)
            lowPriorityPrefetches.enq(cl);
    endrule

    method ActionValue#(Addr) getNextPrefetchAddr 
                 if (highPriorityPrefetches.notEmpty || lowPriorityPrefetches.notEmpty);
                 //if (false);
        Addr retAddr = unpack(0);
        if (highPriorityPrefetches.notEmpty) begin
            retAddr = {highPriorityPrefetches.first, '0};
            highPriorityPrefetches.deq;
        end
        else if (lowPriorityPrefetches.notEmpty) begin
            retAddr = {lowPriorityPrefetches.first, '0};
            lowPriorityPrefetches.deq;
        end
        $display("%t Prefetcher getNextPrefetchAddr requesting chain entry %h", $time, retAddr);
        return retAddr; 
    endmethod

    method Action reportAccess(Addr addr, HitOrMiss hitMiss);
        let cl = getLineAddr(addr);
        if (hitMiss == MISS)
            $display("%t Prefetcher report MISS %h", $time, addr);

        if (hitMiss == MISS || !elem(hash(cl), lastAddrRequests)) begin 
            //Don't start a markov chain if we started a markov chain with that address recently
            $display("%t Prefetcher start new chain with %h", $time, addr);
            targetTable.sendReadWriteReq(cl, hitMiss);
            lastAddrRequests <= shiftInAt0(lastAddrRequests, hash(cl));
        end
    endmethod

endmodule
module mkBlockPrefetcher(Prefetcher) provisos (
    NumAlias#(numLinesEachWay, 1),
    Alias#(lineCountT, Bit#(TLog#(TAdd#(numLinesEachWay, 1))))
);
    Reg#(Bool) nextIsForward <- mkReg(?);
    Reg#(LineAddr) prefetchAround <- mkReg(?);
    Reg#(lineCountT) linesEachWayPrefetched <- mkReg(fromInteger(valueOf(numLinesEachWay)));
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

module mkBRAMStridePCPrefetcher(PCPrefetcher)
provisos(
    NumAlias#(strideTableSize, 512),
    NumAlias#(cLinesAheadToPrefetch, 2), // TODO fetch more if have repeatedly hit an entry, and if stride big
    Alias#(strideTableIndexT, Bit#(TLog#(strideTableSize)))
    );
    //Vector#(strideTableSize, Reg#(StrideEntry)) strideTable <- replicateM(mkReg(unpack(0)));
    RWBramCore#(strideTableIndexT, StrideEntry) strideTable <- mkRWBramCoreForwarded;
    FIFOF#(Tuple3#(Addr, Bit#(16), HitOrMiss)) memAccesses <- mkSizedBypassFIFOF(8);
    Reg#(Tuple3#(Addr, Bit#(16), HitOrMiss)) rdRespEntry <- mkReg(?);

    Fifo#(8, Addr) addrToPrefetch <- mkOverflowPipelineFifo;
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

typedef enum {
  INIT = 2'd0, TRANSIENT = 2'd1, STEADY = 2'd2, NO_PRED = 2'd3
} StrideState2 deriving (Bits, Eq, FShow);

typedef struct {
    Bit#(12) lastAddr; 
    Int#(13) stride;
    Bit#(2) cLinesPrefetched; //Stores how many cache lines have been prefetched for this instruction
    StrideState2 state;
} StrideEntry2 deriving (Bits, Eq, FShow);

module mkStride2PCPrefetcher(PCPrefetcher)
provisos(
    NumAlias#(strideTableSize, 512),
    NumAlias#(cLinesAheadToPrefetch, 2), // TODO fetch more if have repeatedly hit an entry, and if stride big
    Alias#(strideTableIndexT, Bit#(TLog#(strideTableSize)))
    );
    RWBramCore#(strideTableIndexT, StrideEntry2) strideTable <- mkRWBramCoreForwarded;
    FIFOF#(Tuple3#(Addr, Bit#(16), HitOrMiss)) memAccesses <- mkSizedBypassFIFOF(8);
    Reg#(Tuple3#(Addr, Bit#(16), HitOrMiss)) rdRespEntry <- mkReg(?);

    Fifo#(8, Addr) addrToPrefetch <- mkOverflowPipelineFifo;
    FIFO#(Tuple3#(StrideEntry2, Addr, Bit#(16))) strideEntryForPrefetch <- mkBypassFIFO();
    Reg#(Maybe#(Bit#(2))) cLinesPrefetchedLatest <- mkReg(?);
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
        StrideEntry2 se = strideTable.rdResp;
        strideTable.deqRdResp;
        StrideEntry2 seNext = se;
        Int#(13) observedStride = unpack({1'b0, addr[11:0]} - {1'b0, se.lastAddr});
        $writeh("%t Stride Prefetcher updateStrideEntry ", $time,
            fshow(hitMiss), " ", addr,
            ". Entry ", index, " state is ", fshow(se.state));
        if (se.state == INIT && observedStride != 0) begin
            if (se.stride == observedStride) begin
                //fast track to steady
                seNext.state = STEADY;
                $display(", stride matches so fast track back to STEADY");
            end
            else begin
                seNext.stride = observedStride;
                seNext.state = TRANSIENT;
                $display(", stride doesn't match, so set to %h", seNext.stride);
            end
            seNext.lastAddr = truncate(addr);
        end
        else if (se.state == TRANSIENT && observedStride != 0) begin
            if (observedStride == se.stride) begin
                //stride confimed, move to steady
                seNext.cLinesPrefetched = 0;
                seNext.state = STEADY;
                $display(", stride %h is confirmed!", seNext.stride);
            end
            else begin
                //We're seeing random accesses, go to no pred
                seNext.state = NO_PRED;
                seNext.stride = observedStride;
                $display(", we have a random stride (%h), go to NO_PRED", seNext.stride);
            end
            seNext.lastAddr = truncate(addr);
        end
        else if (se.state == STEADY && observedStride != 0) begin
            if (observedStride == se.stride) begin
                if (se.lastAddr[11:6] != addr[11:6]) begin
                    //This means we have crossed a cache line since last access
                    seNext.cLinesPrefetched = 
                        (se.cLinesPrefetched == 0) ? 0 : se.cLinesPrefetched - 1;
                end
                $display(", stride %h stays confirmed!", seNext.stride);
            end
            else begin
                //We jump to some other random location, so reset number of lines prefetched
                seNext.cLinesPrefetched = 0;
                seNext.state = INIT;
                $display(", random jump! Move to INIT, don't reset stride");
            end
            seNext.lastAddr = truncate(addr);
        end
        else if (se.state == NO_PRED && observedStride != 0) begin
            if (observedStride == se.stride) begin
                seNext.state = TRANSIENT;
                $display(", have repeated stride: %h, move to TRANSIENT", seNext.stride);
            end
            else begin
                seNext.stride = observedStride;
                $display(", have random stride: %h", seNext.stride);
            end
            seNext.lastAddr = truncate(addr);
        end
        else
            $display("");
        
        strideEntryForPrefetch.enq(tuple3(seNext, addr, pcHash));
    endrule

    rule createPrefetchRequests;
        match {.se, .addr, .pcHash} = strideEntryForPrefetch.first;
        //If this rule is looping, then we'll have a valid cLinesPrefetchedLatest
        Bit#(2) cLinesPrefetched = fromMaybe(se.cLinesPrefetched, cLinesPrefetchedLatest);

        if (se.state == STEADY && 
            cLinesPrefetched != 
            fromInteger(valueof(cLinesAheadToPrefetch))) begin
            //can prefetch

            Int#(13) cLineSize = fromInteger(valueof(DataSz));
            Int#(13) strideToUse = se.stride;
            if (abs(strideToUse) < cLineSize) begin
                strideToUse = (strideToUse < 0) ? -cLineSize : cLineSize; 
            end

            Bit#(13) jumpDist = pack(strideToUse) * zeroExtend(cLinesPrefetched+1);
            let reqAddr = addr + signExtend(jumpDist);

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

typedef struct {
    Addr lastAddr; 
    Bit#(13) stride;
    Bit#(2) confidence;
} SimpleStrideEntry deriving (Bits, Eq, FShow);

//10 minutes of planning
//25 minutes of implementation
//30 minutes of writing tests and fixing bugs
module mkSimpleStridePCPrefetcher(PCPrefetcher)
provisos(
    NumAlias#(strideTableSize, 512),
    NumAlias#(cLinesAheadToPrefetch, 2), 
    NumAlias#(minConfidenceToPrefetch, 2),
    Alias#(strideTableIndexT, Bit#(TLog#(strideTableSize)))
    );
    Vector#(strideTableSize, Reg#(SimpleStrideEntry)) strideTable <- replicateM(mkReg(unpack(0)));

    Reg#(Addr) addrToPrefetch <- mkReg(0);
    Reg#(Bit#(13)) strideToPrefetch <- mkReg(0);
    Ehr#(2, Bit#(3)) prefetchesIssued <- mkEhr(fromInteger(valueOf(cLinesAheadToPrefetch)));

    method Action reportAccess(Addr addr, Bit#(16) pcHash, HitOrMiss hitMiss);
        $display("%t report access %x %x", $time, addr, pcHash);
        strideTableIndexT idx = truncate(pcHash);
        SimpleStrideEntry entry = strideTable[idx];
        Bit#(13) calc_stride = truncate(addr - entry.lastAddr);
        $display("found stride %x", entry.stride);
        entry.lastAddr = addr;
        if (calc_stride == entry.stride) begin
            if (entry.confidence != 2'd3) begin
                entry.confidence = entry.confidence + 1;
            end
        end
        else begin
            if (entry.confidence > 0) begin
                entry.confidence = entry.confidence - 1;
            end
            if (entry.confidence < fromInteger(valueOf(minConfidenceToPrefetch))) begin
                entry.stride = calc_stride;
                entry.confidence = 0;
            end
        end
        
        if (entry.confidence >= fromInteger(valueOf(minConfidenceToPrefetch))) begin
            prefetchesIssued[1] <= 0;
            addrToPrefetch <= addr;
            strideToPrefetch <= entry.stride;
        end
        strideTable[idx] <= entry;
    endmethod

    method ActionValue#(Addr) getNextPrefetchAddr 
            if (prefetchesIssued[0] < fromInteger(valueOf(cLinesAheadToPrefetch)));
        
        Bit#(13) strideToUse;
        Bit#(13) cLineSize = fromInteger(valueof(DataSz));
        if (abs(strideToPrefetch) < cLineSize) begin
            strideToUse = (strideToPrefetch[12] == 1) ? -cLineSize : cLineSize;
        end
        else begin
            strideToUse = strideToPrefetch;
        end

        prefetchesIssued[0] <= prefetchesIssued[0] + 1; 
        let reqAddr = addrToPrefetch + 
            (signExtend(strideToUse) * zeroExtend(prefetchesIssued[0] + 1));

        check(reqAddr[63:12] == addrToPrefetch[63:12]);
        $display("%t getprefetchaddr ret %x", $time, reqAddr);
        return reqAddr;
    endmethod

endmodule

typedef enum {
    EMPTY = 3'd0, INIT = 3'd1, TRANSIENT = 3'd2, STEADY1 = 3'd3, 
    STEADY2 = 3'd4, STEADY3 = 3'd5, STEADY4 = 4'd6, STEADYLAST = 3'd7
} StrideStateAdaptive deriving (Bits, Eq, FShow);

typedef struct {
    Bit#(12) lastAddr; 
    Bit#(13) stride;
    StrideStateAdaptive state;
    Bit#(4) cLinesPrefetched; //Stores how many cache lines have been prefetched for this instruction
} StrideEntryAdaptive deriving (Bits, Eq, FShow);

module mkBRAMStrideAdaptivePCPrefetcher(PCPrefetcher)
provisos(
    NumAlias#(strideTableSize, 64),
    NumAlias#(cLinesPrefetchMin, 2), 
    NumAlias#(cLinesSmallStridePrefetchMax, 3), 
    NumAlias#(cLinesBigStridePrefetchMax, 5), 
    Alias#(strideTableIndexT, Bit#(TLog#(strideTableSize)))
    );
    RWBramCore#(strideTableIndexT, StrideEntryAdaptive) strideTable <- mkRWBramCoreForwarded;
    FIFOF#(Tuple3#(Addr, Bit#(16), HitOrMiss)) memAccesses <- mkSizedBypassFIFOF(8);
    Reg#(Tuple3#(Addr, Bit#(16), HitOrMiss)) rdRespEntry <- mkReg(?);

    Fifo#(8, Addr) addrToPrefetch <- mkOverflowPipelineFifo;
    FIFO#(Tuple3#(StrideEntryAdaptive, Addr, Bit#(16))) strideEntryForPrefetch <- mkBypassFIFO();
    Reg#(Maybe#(Bit#(4))) cLinesPrefetchedLatest <- mkReg(?);
    PulseWire holdReadReq <- mkPulseWire;

    function StrideStateAdaptive incrementSteady(StrideStateAdaptive state);
        case (state)
            STEADY1: return STEADY2;
            STEADY2: return STEADY3; 
            STEADY3: return STEADY4; 
            STEADY4: return STEADYLAST; 
            STEADYLAST: return STEADYLAST; 
            default: return STEADY1;
        endcase
    endfunction

    function Bool stateAtLeastSteady(StrideStateAdaptive state);
        return (state == STEADY1 ||
                state == STEADY2 ||
                state == STEADY3 ||
                state == STEADY4 ||
                state == STEADYLAST);
    endfunction

    function Bit#(4) cLinesAheadToPrefetch(StrideStateAdaptive state, Bit#(13) stride);
        let absStride = abs(stride);
        if (absStride <= 8) begin
            if (state == STEADY4 || state == STEADYLAST) begin
                return fromInteger(valueof(cLinesSmallStridePrefetchMax));
            end
            else begin
                return fromInteger(valueof(cLinesPrefetchMin));
            end
        end
        else begin
            //big strides
            if (state == STEADYLAST) begin
                return fromInteger(valueof(cLinesBigStridePrefetchMax));
            end
            else if (state == STEADY3 || state == STEADY4) begin
                return fromInteger(valueof(cLinesBigStridePrefetchMax))-1;
            end
            else begin
                return fromInteger(valueof(cLinesPrefetchMin));
            end
        end
    endfunction

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
        StrideEntryAdaptive se = strideTable.rdResp;
        strideTable.deqRdResp;
        StrideEntryAdaptive seNext = se;
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
        else if (se.state == TRANSIENT && observedStride != 0) begin
            if (observedStride == se.stride) begin
                //Here we transition from TRANSIENT to STEADY, so init this field
                seNext.cLinesPrefetched = 0;
                seNext.state = STEADY1;
                $display(", stride %h is confirmed!", seNext.stride);
            end
            else begin
                seNext.state = TRANSIENT;
                seNext.stride = observedStride;
                $display(", old stride is broken! New stride: %h", seNext.stride);
            end
            seNext.lastAddr = truncate(addr);
        end
        else if (stateAtLeastSteady(se.state) && observedStride != 0) begin
            if (observedStride == se.stride) begin
                if (se.lastAddr[11:6] != addr[11:6]) begin
                    //This means we have crossed a cache line since last access
                    seNext.cLinesPrefetched = 
                        (se.cLinesPrefetched == 0) ? 0 : se.cLinesPrefetched - 1;
                end
                seNext.state = incrementSteady(se.state);
                $display(", stride %h is sustained, advance STEADY number!", se.stride);
            end
            else if (se.state == STEADY4 || se.state == STEADYLAST) begin
                //Leniency towards some stride changes
                seNext.state = STEADY1;
                seNext.cLinesPrefetched = 0; //We've jumped to some other address, so start fetching again!
                $display(", old stride is broken, but tolerate it! Keep old stride: %h", se.stride);
            end
            else begin
                seNext.state = TRANSIENT;
                seNext.stride = observedStride;
                $display(", old stride is broken! New stride: %h", seNext.stride);
            end
            seNext.lastAddr = truncate(addr);
        end
        else
            $display("");
        
        strideEntryForPrefetch.enq(tuple3(seNext, addr, pcHash));
    endrule

    rule createPrefetchRequests;
        match {.se, .addr, .pcHash} = strideEntryForPrefetch.first;
        //If this rule is looping, then we'll have a valid cLinesPrefetchedLatest
        Bit#(4) cLinesPrefetched = fromMaybe(se.cLinesPrefetched, cLinesPrefetchedLatest);

        if (stateAtLeastSteady(se.state) && 
            cLinesPrefetched != 
            cLinesAheadToPrefetch(se.state, se.stride)) begin
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


interface PrefetcherVector#(numeric type size);
    method ActionValue#(Tuple2#(Addr, Bit#(TLog#(size)))) getNextPrefetchAddr;
    method Action reportAccess(Bit#(TLog#(size)) idx, Addr addr, HitOrMiss hitMiss);
endinterface

module mkPrefetcherVector#(module#(Prefetcher) mkPrefetcher)
(
    PrefetcherVector#(size)
) provisos (
    Alias#(idxT, Bit#(TLog#(size)))
);
    Vector#(size, Prefetcher) prefetchers <- replicateM(mkPrefetcher);
    Fifo#(1, Tuple2#(Addr, idxT)) prefetchRq <- mkBypassFifo;

    function XBarDstInfo#(Bit#(0),Tuple2#(Addr, idxT)) convertPrefetchRq(idxT item, Addr a);
        return XBarDstInfo { 
            idx: 0,
            data: tuple2(a, item)
        };
    endfunction
    function Get#(Addr) reqGet(Prefetcher p) = toGet(p.getNextPrefetchAddr);
    mkXBar(convertPrefetchRq, map(reqGet, prefetchers), vec(toPut(prefetchRq)));

    method ActionValue#(Tuple2#(Addr, idxT)) getNextPrefetchAddr;
        prefetchRq.deq;
        return prefetchRq.first;
    endmethod

    method Action reportAccess(idxT idx, Addr addr, HitOrMiss hitMiss);
        prefetchers[idx].reportAccess(addr, hitMiss);
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
        let m <-  mkSingleWindowL1LLPrefetcher;
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
        //let m <- mkBRAMStridePCPrefetcher;
        let m <- mkStride2PCPrefetcher;
    `elsif DATA_PREFETCHER_STRIDE_ADAPTIVE
        let m <- mkBRAMStrideAdaptivePCPrefetcher;
    `elsif DATA_PREFETCHER_MARKOV
        let m <- mkPCPrefetcherAdapter(mkBRAMMarkovPrefetcher);
    `elsif DATA_PREFETCHER_MARKOV_ON_HIT
        let m <- mkPCPrefetcherAdapter(mkBRAMMarkovOnHitPrefetcher);
    `elsif DATA_PREFETCHER_MARKOV_ON_HIT_2
        let m <- mkPCPrefetcherAdapter(mkMarkovOnHit2Prefetcher);
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
    `elsif DATA_PREFETCHER_STRIDE_ADAPTIVE
        let m <- mkBRAMStrideAdaptivePCPrefetcher;
    `elsif DATA_PREFETCHER_MARKOV
        let m <- mkPCPrefetcherAdapter(mkBRAMMarkovPrefetcher);
    `elsif DATA_PREFETCHER_MARKOV_ON_HIT
        let m <- mkPCPrefetcherAdapter(mkBRAMMarkovOnHitPrefetcher);
    `elsif DATA_PREFETCHER_MARKOV_ON_HIT_2
        let m <- mkPCPrefetcherAdapter(mkMarkovOnHit2Prefetcher);
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
    `elsif DATA_PREFETCHER_STRIDE
        doAssert(False, "Illegal data prefetcher type for LL cache!")
    `elsif DATA_PREFETCHER_MARKOV
        let m <- mkBRAMMarkovPrefetcher;
    `elsif DATA_PREFETCHER_MARKOV_ON_HIT
        let m <- mkBRAMMarkovOnHitPrefetcher;
    `elsif DATA_PREFETCHER_MARKOV_ON_HIT_2
        let m <- mkMarkovOnHit2Prefetcher;
    `endif
    //let m <- mkPCPrefetcherAdapter(mkAlwaysRequestPrefetcher);
`else 
    let m <- mkDoNothingPrefetcher;
`endif
    return m;
endmodule