import ISA_Decls   :: *;
import CacheUtils::*;
import CCTypes::*;
import Types::*;
import Vector::*;

interface Prefetcher;
    method Action reportHit(Addr hitAddr);
    method Action reportMiss(Addr missAddr);
    method ActionValue#(Addr) getNextPrefetchAddr();
    //method Action flush;
    //method Bool flush_done;
endinterface

module mkTestPrefetcher(Prefetcher);
    Reg#(Addr) lastMissAddr <- mkReg(0);
    Reg#(Bit#(2)) reqSent <- mkReg(0);
    method Action reportHit(Addr hitAddr);
        $display("%t I Prefetcher reportHit %h", $time, hitAddr);
    endmethod
    method Action reportMiss(Addr missAddr);
        $display("%t I Prefetcher reportMiss %h", $time, missAddr);
        if (missAddr == 'h0000000080000040)
            lastMissAddr <= missAddr;
    endmethod
    method ActionValue#(Addr) getNextPrefetchAddr if (reqSent < 2 && lastMissAddr == 'h0000000080000040);
        $display("%t I Prefetcher getNextPrefetchAddr", $time);
        reqSent <= reqSent + 1;
        if (reqSent == 0)  begin
            return 64'h0000000080000080;
        end
        else begin
            return 64'h00000000800000c0;
        end
    endmethod
endmodule

module mkDoNothingPrefetcher(Prefetcher);
    method Action reportHit(Addr hitAddr);
    endmethod
    method Action reportMiss(Addr missAddr);
    endmethod
    method ActionValue#(Addr) getNextPrefetchAddr if (False);
        return 64'h0000000080000080;
    endmethod
endmodule

module mkPrintPrefetcher(Prefetcher);
    method Action reportHit(Addr hitAddr);
        $display("%t PrintPrefetcher reportHit %h", $time, hitAddr);
    endmethod
    method Action reportMiss(Addr missAddr);
        $display("%t PrintPrefetcher reportMiss %h", $time, missAddr);
    endmethod
    method ActionValue#(Addr) getNextPrefetchAddr if (False);
        return 64'h0000000080000080;
    endmethod
endmodule

typedef 3 INextLinesOnMiss;
typedef 0 DNextLinesOnMiss;

module mkNextLineOnMissPrefetcher(Prefetcher)
    provisos (
        Alias#(rqCntT, Bit#(TLog#(TAdd#(INextLinesOnMiss, 1))))
    );
    Reg#(Addr) lastMissAddr <- mkReg(0);
    Reg#(rqCntT) sentRequestCounter <- mkReg(fromInteger(valueOf(INextLinesOnMiss)));
    method Action reportHit(Addr hitAddr);
        $display("%t Prefetcher reportHit %h", $time, hitAddr);
    endmethod
    method Action reportMiss(Addr missAddr);
        $display("%t Prefetcher reportMiss %h", $time, missAddr);
        lastMissAddr <= missAddr;
        sentRequestCounter <= 0;
    endmethod
    method ActionValue#(Addr) getNextPrefetchAddr if (sentRequestCounter < fromInteger(valueOf(INextLinesOnMiss)));
        sentRequestCounter <= sentRequestCounter + 1;
        let addrToRequest = lastMissAddr + (zeroExtend(sentRequestCounter) + 1)*fromInteger(valueOf(DataSz));
        $display("%t Prefetcher getNextPrefetchAddr requesting %h", $time, addrToRequest);
        return addrToRequest;
    endmethod
endmodule

module mkNextLineOnAllPrefetcher(Prefetcher)
    provisos (
        Alias#(rqCntT, Bit#(TLog#(TAdd#(INextLinesOnMiss, 1))))
    );
    Reg#(Addr) lastMissAddr <- mkReg(0);
    Reg#(rqCntT) sentRequestCounter <- mkReg(fromInteger(valueOf(INextLinesOnMiss)));
    method Action reportHit(Addr hitAddr);
        $display("%t Prefetcher reportHit %h", $time, hitAddr);
        lastMissAddr <= hitAddr;
        sentRequestCounter <= 0;
    endmethod
    method Action reportMiss(Addr missAddr);
        $display("%t Prefetcher reportMiss %h", $time, missAddr);
        lastMissAddr <= missAddr;
        sentRequestCounter <= 0;
    endmethod
    method ActionValue#(Addr) getNextPrefetchAddr if (sentRequestCounter < fromInteger(valueOf(INextLinesOnMiss)));
        sentRequestCounter <= sentRequestCounter + 1;
        let addrToRequest = lastMissAddr + (zeroExtend(sentRequestCounter) + 1)*fromInteger(valueOf(DataSz));
        $display("%t Prefetcher getNextPrefetchAddr requesting %h", $time, addrToRequest);
        return addrToRequest;
    endmethod
endmodule

module mkNextLinePrefetcherBackwards(Prefetcher)
    provisos (
        Alias#(rqCntT, Bit#(TLog#(TAdd#(DNextLinesOnMiss, 1))))
    );
    Reg#(Addr) lastMissAddr <- mkReg(0);
    Reg#(rqCntT) sentRequestCounter <- mkReg(fromInteger(valueOf(DNextLinesOnMiss)));
    method Action reportHit(Addr hitAddr);
        $display("%t Prefetcher reportHit %h", $time, hitAddr);
    endmethod
    method Action reportMiss(Addr missAddr);
        $display("%t Prefetcher reportMiss %h", $time, missAddr);
        lastMissAddr <= missAddr;
        sentRequestCounter <= 0;
    endmethod
    method ActionValue#(Addr) getNextPrefetchAddr if (sentRequestCounter < fromInteger(valueOf(DNextLinesOnMiss)));
        sentRequestCounter <= sentRequestCounter + 1;
        let addrToRequest = lastMissAddr - (zeroExtend(sentRequestCounter) + 1)*fromInteger(valueOf(DataSz));
        $display("%t Prefetcher getNextPrefetchAddr requesting %h", $time, addrToRequest);
        return addrToRequest;
    endmethod
endmodule

module mkSingleWindowPrefetcher(Prefetcher);
    Integer cacheLinesInRange = 3;
    Reg#(LineAddr) rangeEnd <- mkReg(0); //Points to one CLine after end of range
    Reg#(LineAddr) nextToAsk <- mkReg(0);
    method Action reportHit(Addr hitAddr);
        let cl = getLineAddr(hitAddr);
        if (rangeEnd - fromInteger(cacheLinesInRange) - 1 < cl && cl < rangeEnd) begin
            let nextEnd = cl + fromInteger(cacheLinesInRange) + 1;
            $display("%t Prefetcher reportHit %h, moving window end to %h", $time, hitAddr, Addr'{nextEnd, '0});
            rangeEnd <= nextEnd;
        end
    endmethod
    method Action reportMiss(Addr missAddr);
        $display("%t Prefetcher reportMiss %h", $time, missAddr);
        nextToAsk <= getLineAddr(missAddr) + 1;
        rangeEnd <= getLineAddr(missAddr) + fromInteger(cacheLinesInRange) + 1;
        //LgLineSzBytes
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

typedef 4 NumWindows;

module mkMultiWindowPrefetcher(Prefetcher)
provisos(
    Alias#(windowIdxT, Bit#(TLog#(NumWindows)))
);
    Integer cacheLinesInRange = 2;
    Vector#(NumWindows, Reg#(StreamEntry)) streams 
        <- replicateM(mkReg(StreamEntry {rangeEnd: '0, nextToAsk: '0}));
    Vector#(NumWindows, Reg#(windowIdxT)) shiftReg <- genWithM(compose(mkReg, fromInteger));

    //function 
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
        //Finds the first window that contains addr, 
        //and moves it to the front of the LRU shift reg
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

    method Action reportHit(Addr hitAddr);
        //Check if any stream line matches request
        //if so, advance that stream line
        //also advance LRU shift reg
        let idxMaybe <- getMatchingWindow(hitAddr);
        if (idxMaybe matches tagged Valid .idx) begin
            moveWindowToFront(pack(idx)); //Update window as just used
            let newRangeEnd = getLineAddr(hitAddr) + fromInteger(cacheLinesInRange) + 1;
            streams[idx].rangeEnd <= newRangeEnd;
            $display("%t Prefetcher reportHit %h, moving window end to %h for window idx %h", 
                $time, hitAddr, Addr'{newRangeEnd, '0}, idx);
        end
        else begin
            $display("%t Prefetcher reportHit %h, no matching window found.", 
                $time, hitAddr);
        end
    endmethod

    method Action reportMiss(Addr missAddr);
        //Check if any stream line matches request
        //If so, advance that stream line and advance LRU shift reg
        //Otherwise, allocate new stream line, and shift LRU reg completely,
        let idxMaybe <- getMatchingWindow(missAddr);
        if (idxMaybe matches tagged Valid .idx) begin
            moveWindowToFront(pack(idx)); //Update window as just used
            let newRangeEnd = getLineAddr(missAddr) + fromInteger(cacheLinesInRange) + 1;
            //Also refresh nextToAsk on miss
            streams[idx] <= 
                StreamEntry {nextToAsk: getLineAddr(missAddr) + 1, 
                            rangeEnd: newRangeEnd};
            $display("%t Prefetcher reportMiss %h, moving window end to %h for window idx %h", 
                $time, missAddr, Addr'{newRangeEnd, '0}, idx);
        end
        else begin
            $display("%t Prefetcher reportMiss %h, allocating new stream, idx %h", $time, missAddr, shiftReg[3]);
            streams[shiftReg[3]] <= 
                StreamEntry {nextToAsk: getLineAddr(missAddr) + 1,
                            rangeEnd: getLineAddr(missAddr) + fromInteger(cacheLinesInRange) + 1};
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

interface PCPrefetcher;
    method Action reportAccess(Addr addr, Bit#(16) pcHash, HitOrMiss hitMiss);
    method ActionValue#(Addr) getNextPrefetchAddr();
endinterface

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
            $display("%t PCPrefetcher reportHit %h", $time, addr);
        else
            $display("%t PCPrefetcher reportMiss %h", $time, addr);
    endmethod
    method ActionValue#(Addr) getNextPrefetchAddr if (False);
        return 64'h0000000080000080;
    endmethod
endmodule

typedef enum {
  EMPTY = 2'b00, INIT = 2'b01, TRANSIENT = 2'b10, STEADY = 2'b11
} StrideState deriving (Bits, Eq, FShow);

typedef enum {
    HIT = 1'b0, MISS = 1'b1
} HitOrMiss deriving (Bits, Eq, FShow);

typedef struct {
    Addr lastAddr; //TODO maybe store less bits here?
    Bit#(13) stride;
    StrideState state;
    Bit#(4) lastPrefetch; //Stores how many strides ahead of lastAddr was the last prefetch done
} StrideEntry deriving (Bits, Eq, FShow);

typedef 8 HistoryLen;
typedef 64 StrideTableSize;
typedef 4 StridesAheadToPrefetch;
module mkStridePCPrefetcher(PCPrefetcher)
provisos(
    Alias#(strideTableIndexT, Bit#(TLog#(StrideTableSize)))
    );
    Reg#(Vector#(HistoryLen, strideTableIndexT)) historyVec <- mkReg(replicate(?));
    Vector#(StrideTableSize, Reg#(StrideEntry)) strideTable <- replicateM(mkReg(unpack(0)));

    function Maybe#(strideTableIndexT) getNextPrefetchIndex;
        function Bool canPrefetch(strideTableIndexT idx);
            return (strideTable[idx].state == STEADY && 
                strideTable[idx].lastPrefetch != fromInteger(valueof(StridesAheadToPrefetch)));
        endfunction

        //Find first entry that allows more prefetches
        case (find(canPrefetch, historyVec)) matches 
            tagged Valid .idx: return Valid(pack(idx));
            Invalid: return Invalid;
        endcase
    endfunction

    method ActionValue#(Addr) getNextPrefetchAddr if 
        (getNextPrefetchIndex() matches tagged Valid .idx);
        //could store some most recent table entries,
        //then check if any of those entries can be prefetched more

        //could just store the recent table entries in a circular buffer with duplication 
        // since shouldn't have duplicates within one loop iteration
        // but we need to access all entries (or have rule that discards first entry, it we can't prefetch for it)
        Reg#(StrideEntry) se = strideTable[idx];
        se.lastPrefetch <= se.lastPrefetch + 1;
        return se.lastAddr + (signExtend(se.stride) * zeroExtend(se.lastPrefetch + 1));
    endmethod
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
        $writeh("Prefetcher: ",
            fshow(hitMiss), " ", addr,
            ". Entry state is ", fshow(se.state));

        if (hitMiss == MISS && se.state == EMPTY) begin
            seNext.lastAddr = addr;
            seNext.state = INIT;
            $display(", allocate entry");
        end 
        else if (se.state == INIT) begin
            seNext.stride = truncate(addr - se.lastAddr);
            seNext.state = TRANSIENT;
            seNext.lastAddr = addr;
            $display(", set stride to %h", seNext.stride);
        end
        else if (se.state == TRANSIENT || se.state == STEADY) begin
            if (truncate(addr - se.lastAddr) == se.stride) begin
                case (se.state)
                    TRANSIENT: seNext.lastPrefetch = 0;
                    STEADY: seNext.lastPrefetch = (seNext.lastPrefetch == 0) ? 0 : se.lastPrefetch - 1;
                endcase
                seNext.state = STEADY;
                seNext.lastAddr = addr;
                historyVec <= shiftInAt0(historyVec, index);
                $display(", stride %h is confirmed!", seNext.stride);
            end
            else begin
                seNext.state = TRANSIENT;
                seNext.stride = truncate(addr - se.lastAddr);
                seNext.lastAddr = addr;
                $display(", old stride is broken! New stride: %h", seNext.stride);
            end
        end
        se <= seNext;
    endmethod
endmodule

typedef struct {
    Bit#(12) lastAddr; //TODO maybe store less bits here?
    Bit#(13) stride;
    StrideState state;
    Bit#(4) cLinesPrefetched; //Stores how many cache lines have been prefetched for this instruction
} StrideEntry2 deriving (Bits, Eq, FShow);

typedef 3 CLinesAheadToPrefetch;

module mkStridePCPrefetcher2(PCPrefetcher)
provisos(
    Alias#(strideTableIndexT, Bit#(TLog#(StrideTableSize))),
    Alias#(historyVecIndexT, Bit#(TLog#(HistoryLen)))
    );
    Reg#(Vector#(HistoryLen, Tuple2#(strideTableIndexT, Addr))) historyVec <- mkReg(replicate(?));
    Vector#(StrideTableSize, Reg#(StrideEntry2)) strideTable <- replicateM(mkReg(unpack(0)));

    function Maybe#(historyVecIndexT) getNextPrefetchHistoryIndex;
        function Bool canPrefetch(Tuple2#(strideTableIndexT, Addr) entry);
            strideTableIndexT idx = tpl_1(entry);
            return (strideTable[idx].state == STEADY && 
                strideTable[idx].cLinesPrefetched != 
                    fromInteger(valueof(CLinesAheadToPrefetch)));
        endfunction

        //Find first entry that allows more prefetches
        case (findIndex(canPrefetch, historyVec)) matches 
            tagged Valid .historyIdx: return Valid(pack(historyIdx));
            Invalid: return Invalid;
        endcase
    endfunction

    method ActionValue#(Addr) getNextPrefetchAddr if 
        (getNextPrefetchHistoryIndex() matches tagged Valid .historyIdx);
        match {.strideIdx, .fullAddr} = historyVec[historyIdx];
        Reg#(StrideEntry2) se = strideTable[strideIdx];
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
        else
            strideToUse = se.stride;

        let reqAddr = fullAddr + 
            (signExtend(strideToUse) * zeroExtend(se.cLinesPrefetched + 1));
        $display("%t Stride Prefetcher getNextPrefetchAddr requesting %h for entry %h", $time, reqAddr, strideIdx);
        return reqAddr;
    endmethod
    method Action reportAccess(Addr addr, Bit#(16) pcHash, HitOrMiss hitMiss);
        //Find slot in vector
        //if miss and slot empty
        //if slot init, put address, stride and move to transit
        //if slot transit or steady, verify stride, and move to steady
        //    also put last_prefetched
        //if stride wrong, move to transit
        strideTableIndexT index = truncate(pcHash);
        Reg#(StrideEntry2) se = strideTable[index];
        StrideEntry2 seNext = se;
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



endmodule

module mkL1IPrefetcher(Prefetcher);
    //let m <- mkNextLinePrefetcher;
    //let m <- mkMultiWindowPrefetcher;
    //let m <- mkNextLineOnAllPrefetcher;
    let m <- mkDoNothingPrefetcher;
    return m;
endmodule

module mkL1DPrefetcher(PCPrefetcher);
    //let m <- mkNextLineOnAllPrefetcher;
    //let m <- mkStridePCPrefetcher2;
    let m <- mkDoNothingPCPrefetcher;
    return m;
endmodule

module mkLLIPrefetcher(Prefetcher);
    //let m <- mkNextLineOnMissPrefetcher;
    let m <- mkMultiWindowPrefetcher;
    //let m <- mkStridePCPrefetcher2;
    //let m <- mkPrintPrefetcher;
    //let m <- mkDoNothingPrefetcher;
    return m;
endmodule

module mkLLDPrefetcher(Prefetcher);
    //let m <- mkNextLineOnAllPrefetcher;
    let m <- mkPrintPrefetcher;
    //let m <- mkDoNothingPrefetcher;
    return m;
endmodule