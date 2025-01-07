import GlobalBranchHistory::*;
import FoldedHistory::*;
import BrPred::*;
import BranchParams::*;
import ProcTypes::*;
import Types::*;
import Util::*;
import RegFile::*;


`define MAX_TAGGED 12
`define MAX_INDEX_SIZE 10

typedef 3 PredCtrSz;
typedef Bit#(PredCtrSz) PredCtr;

typedef 2 UsefulCtrSz;
typedef Bit#(UsefulCtrSz) UsefulCtr;

typedef struct {
    PredCtr predictionCounter;
    UsefulCtr usefulCounter;
    Bit#(tagSize) tag;
} TaggedTableEntry#(numeric type tagSize) deriving(Bits, Eq, FShow);

typedef enum {
    INCREMENT,
    PRESERVE,
    DECREMENT
} UsefulCtrUpdate deriving (Bits, Eq, FShow);

typedef enum {
    BEFORE_RECOVERY,
    AFTER_RECOVERY,
    PREV_LAST_UPDATE
} HistoryRetrieve deriving (Bits, Eq, FShow);

function Bool takenFromCounter(PredCtr ctr);
    return unpack(pack(ctr)[valueOf(TSub#(PredCtrSz,1))]);
endfunction


// 100
// 011
function Bool weakCounter(PredCtr ctr);
    return (pack(ctr) == (1 << valueOf(TSub#(PredCtrSz,1)))) || (pack(ctr) == ((1 << valueOf(TSub#(PredCtrSz,1)))-1));
endfunction

interface TaggedTable#(numeric type indexSize, numeric type tagSize, numeric type historyLength);
    method TaggedTableEntry#(tagSize) access_entry(Addr pc);
    method TaggedTableEntry#(`MAX_TAGGED) access_wrapped_entry(Addr pc);
    method Tuple2#(Bit#(tagSize), Bit#(indexSize)) trainingInfo(Addr pc, HistoryRetrieve recovered); // To be used in training

    method Action updateHistory(Bit#(SupSize) results, SupCnt count);
    method Action updateRecovered(Bit#(1) taken);
    method Action recoverHistory(Bit#(TLog#(MaxSpecSize)) numRecovery);
    

    method Action updateEntry(Bit#(`MAX_INDEX_SIZE) index, Bit#(`MAX_TAGGED) tag, Bool taken, UsefulCtrUpdate usefulUpdate);
    
    // Only done on misprediction
    method Action decrementUsefulCounter(Addr pc);

    method Action allocateEntry(Addr pc, Bool taken);

    /// Debug
    `ifdef DEBUG
        method Action debugUnsetEntry(Addr pc);
        method TaggedTableEntry#(tagSize) debugGetEntry(Bit#(indexSize) index);
    `endif
endinterface




module mkTaggedTable#(GlobalBranchHistory#(GlobalHistoryLength) global) (TaggedTable#(indexSize, tagSize, historyLength)) provisos(
    Add#(a__, indexSize, 64), 
    Add#(b__, tagSize, 64), 
    Add#(indexSize, tagSize, foldedSize),
    Add#(f__, TAdd#(tagSize, indexSize), 64),
    Add#(d__, tagSize, `MAX_TAGGED),
    Add#(c__, indexSize, `MAX_INDEX_SIZE));
    
    FoldedHistory#(TAdd#(tagSize, indexSize)) folded <- mkFoldedHistory(valueOf(historyLength), global);
    RegFile#(Bit#(indexSize), TaggedTableEntry#(tagSize)) tab <- mkRegFileWCF(0, maxBound);
    PulseWire sameCycleRecovery <- mkPulseWire;

    function Tuple2#(Bit#(tagSize), Bit#(indexSize)) getHistory(HistoryRetrieve hr, Addr pc);
        Bit#(TAdd#(tagSize, indexSize)) hist = 0;
        if(hr == AFTER_RECOVERY)
            hist = folded.recoveredHistory;
        else if(hr == BEFORE_RECOVERY)
            hist = folded.history;
        else
            hist = folded.historyOneBefore;

        let combined = (pack(pc) ^ (pack(pc) >> 2) ^ (pack(pc) >> 5)) ^ zeroExtend(hist);
        
        let index = combined[valueOf(indexSize)-1:0];
        let tag = combined[valueOf(tagSize)+valueOf(indexSize)-1:valueOf(indexSize)];
        return tuple2(tag, index);
    endfunction

    // ----------------- DEBUG
    `ifdef DEBUG
    /*rule debug(False);
        $display("Folded: %b\n", folded.history);
    endrule*/
    
    method TaggedTableEntry#(tagSize) debugGetEntry(Bit#(indexSize) index);
        return tab.sub(index);
    endmethod

    method Action debugUnsetEntry(Addr pc);
        match {.tag, .index} = getHistory(AFTER_RECOVERY, pc);
        tab.upd(index, TaggedTableEntry{tag: 0, predictionCounter:0, usefulCounter:0});
    endmethod
    `endif
    
    
    
    // ----------------- DEBUG



    method Action updateHistory(Bit#(SupSize) results, SupCnt count) = folded.updateHistory(results, count);
    method Action updateRecovered(Bit#(1) taken) = folded.updateRecoveredHistory(taken);
    method Action recoverHistory(Bit#(TLog#(MaxSpecSize)) numRecovery);
        sameCycleRecovery.send;
        folded.recoverFrom[numRecovery].undo;
    endmethod

  
    method Tuple2#(Bit#(tagSize), Bit#(indexSize)) trainingInfo(Addr pc, HistoryRetrieve recovered); // To be used in training
        return getHistory(recovered, pc);
    endmethod

    method TaggedTableEntry#(tagSize) access_entry(Addr pc);
         // Shift necessary?
        //folded.history[valueOf(indexSize)-1:0] ^ truncate(pc >> 2);
        Bit#(indexSize) index = tpl_2(getHistory(BEFORE_RECOVERY, pc));
        return tab.sub(index);
    endmethod

    method TaggedTableEntry#(`MAX_TAGGED) access_wrapped_entry(Addr pc);
        // Shift necessary?
       Bit#(indexSize) index = tpl_2(getHistory(BEFORE_RECOVERY, pc));
       TaggedTableEntry#(tagSize) entry = tab.sub(index);
       TaggedTableEntry#(`MAX_TAGGED) ret = TaggedTableEntry{tag: zeroExtend(entry.tag), predictionCounter: entry.predictionCounter, usefulCounter: entry.usefulCounter};
       return ret;
   endmethod

    method Action updateEntry(Bit#(`MAX_INDEX_SIZE) index, Bit#(`MAX_TAGGED) tag, Bool taken, UsefulCtrUpdate usefulUpdate);
        let currentEntry = tab.sub(truncate(index));
        if (currentEntry.tag == truncate(tag)) begin
            TaggedTableEntry#(tagSize) newEntry = currentEntry;   
            // Update prediction and useful counter
            newEntry.predictionCounter = boundedUpdate(currentEntry.predictionCounter, taken);
            if (usefulUpdate != PRESERVE) begin
                newEntry.usefulCounter = boundedUpdate(currentEntry.usefulCounter, usefulUpdate == INCREMENT);
            end

            // Probably completely unnecessary and unhelpful
            if ({newEntry.predictionCounter, newEntry.usefulCounter} != {currentEntry.predictionCounter, currentEntry.usefulCounter}) begin
                tab.upd(truncate(index), newEntry);            
            end
        end
    endmethod

    
    method Action decrementUsefulCounter(Addr pc);
        match {.tag, .index} = getHistory(AFTER_RECOVERY, pc); // Need to use the recovered history!
        // Idea - seperate the useful counters? or some other way of doing this without a read. Could instead drag useful counters.
        TaggedTableEntry#(tagSize) entry = tab.sub(index);
        entry.usefulCounter = boundedUpdate(entry.usefulCounter, False);
        tab.upd(index, entry);
    endmethod

    // 3 bits 100 011
    method Action allocateEntry(Addr pc,  Bool taken);
        /*
            Need to remove last history bit to get the correct index
            Alternatively could drag the indices of every table in the training data.

            If recovered in this cycle - can use getHistory(True), otherwise we need to remove a bit.
        */        
        HistoryRetrieve hr;
        if(sameCycleRecovery) begin
            hr = AFTER_RECOVERY;
        end
        else
            hr = PREV_LAST_UPDATE;

        match {.tag, .index} = getHistory(AFTER_RECOVERY, pc);
        
        // Weakly taken = 100 - 1, weakly not taken = 100 - 1
        Bit#(PredCtrSz) counter_init = 1 << (valueOf(PredCtrSz)-1);
        if (!taken) begin
            counter_init = (1 << (valueOf(PredCtrSz)-1))-1;
        end
        
        TaggedTableEntry#(tagSize) toWrite = TaggedTableEntry{predictionCounter: counter_init, usefulCounter:  0, tag: tag};
        tab.upd(index, toWrite);
    endmethod
endmodule