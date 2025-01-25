import GlobalBranchHistory::*;
import FoldedHistory::*;
import BrPred::*;
import BranchParams::*;
import ProcTypes::*;
import Types::*;
import Util::*;

import RegFile::*;
import Vector::*;


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
    AFTER_RECOVERY
} HistoryRetrieve deriving (Bits, Eq, FShow);

function Bool takenFromCounter(PredCtr ctr);
    return unpack(pack(ctr)[valueOf(TSub#(PredCtrSz,1))]);
endfunction


// 100
// 011
function Bool weakCounter(PredCtr ctr);
    return (pack(ctr) == (1 << valueOf(TSub#(PredCtrSz,1)))) || (pack(ctr) == ((1 << valueOf(TSub#(PredCtrSz,1)))-1));
endfunction

interface AccessPredInfo#(numeric type tagSize, numeric type indexSize);
    method Tuple2#(Bit#(`MAX_TAGGED), Bit#(`MAX_INDEX_SIZE)) access(Addr pc);
endinterface

interface TaggedTable#(numeric type indexSize, numeric type tagSize, numeric type historyLength);
    interface Vector#(SupSize, AccessPredInfo#(tagSize, indexSize)) accessPredInfo;
    method TaggedTableEntry#(`MAX_TAGGED) access_wrapped_entry(Addr pc, Bit#(indexSize) index);

    //method TaggedTableEntry#(`MAX_TAGGED) access_wrapped_entry(Addr pc);
    method Tuple2#(Bit#(tagSize), Bit#(indexSize)) trainingInfo(Addr pc, HistoryRetrieve recovered); // To be used in training

    method Action updateHistory(Bit#(SupSize) results, SupCnt count);
    method Action updateRecovered(Bit#(1) taken);
    method Action recoverHistory(Bit#(TLog#(MaxSpecSize)) numRecovery);
    

    method Action updateEntry(Bit#(`MAX_INDEX_SIZE) index, Bit#(`MAX_TAGGED) tag, Bool taken, UsefulCtrUpdate usefulUpdate);
    
    // Only done on misprediction
    method Action decrementUsefulCounter(Bit#(indexSize) index);

    method Action allocateEntry(Bit#(indexSize) index, Bit#(tagSize) tag, Bool taken);

    /// Debug
    `ifdef DEBUG
        method Action debugUnsetEntry(Addr pc);
        method TaggedTableEntry#(tagSize) debugGetEntry(Bit#(indexSize) index);
    `endif

    `ifdef DEBUG_TAGETEST
        method Bit#(TAdd#(tagSize, indexSize)) debugGetHistory(HistoryRetrieve hr, Maybe#(Bit#(TLog#(SupSize))) count);
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
    RegFile#(Bit#(indexSize), TaggedTableEntry#(tagSize)) tab <- mkRegFileWCFLoad(regInitTaggedTableFilename, 0, maxBound);
    PulseWire sameCycleRecovery <- mkPulseWire;

    Vector#(SupSize, AccessPredInfo#(tagSize, indexSize)) accessPredInfoIfc;

    

    function Tuple2#(Bit#(tagSize), Bit#(indexSize)) getHistory(HistoryRetrieve hr, Addr pc, Maybe#(Bit#(TLog#(SupSize))) count);
        Bit#(TAdd#(tagSize, indexSize)) hist = 0;
        if(hr == AFTER_RECOVERY)
            hist = folded.recoveredHistory;
        else if(hr == BEFORE_RECOVERY)
            if(count matches tagged Valid .num)
                hist = folded.sameWindowHistory[num].history;
            else
                hist = folded.history;

        let combined = (pack(pc) ^ (pack(pc) >> 2) ^ (pack(pc) >> 5)) ^ zeroExtend(hist);
        
        let index = combined[valueOf(indexSize)-1:0];
        let tag = combined[valueOf(tagSize)+valueOf(indexSize)-1:valueOf(indexSize)] ^ truncate(pack(pc));
        return tuple2(tag, index);
    endfunction
    
    for(Integer i = 0; i < valueOf(SupSize); i = i+1) begin
        accessPredInfoIfc[i] = (interface AccessPredInfo#(tagSize, indexSize);
            method Tuple2#(Bit#(`MAX_TAGGED), Bit#(`MAX_INDEX_SIZE)) access(Addr pc);
                match {.tag, .index} = getHistory(BEFORE_RECOVERY, pc, tagged Valid fromInteger(i));
                return tuple2(zeroExtend(tag), zeroExtend(index));
            endmethod
        endinterface);
    end
    interface accessPredInfo = accessPredInfoIfc;

    method TaggedTableEntry#(`MAX_TAGGED) access_wrapped_entry(Addr pc, Bit#(indexSize) index);
        TaggedTableEntry#(tagSize) entry = tab.sub(index);
        TaggedTableEntry#(`MAX_TAGGED) ret = TaggedTableEntry{tag: zeroExtend(entry.tag), predictionCounter: entry.predictionCounter, usefulCounter: entry.usefulCounter};      
        return ret;
    endmethod

    // ----------------- DEBUG
    `ifdef DEBUG
    /*rule debug(False);
        $display("Folded: %b\n", folded.history);
    endrule*/
    
    method TaggedTableEntry#(tagSize) debugGetEntry(Bit#(indexSize) index);
        return tab.sub(index);
    endmethod

    method Action debugUnsetEntry(Addr pc);
        match {.tag, .index} = getHistory(AFTER_RECOVERY, pc, tagged Invalid);
        tab.upd(index, TaggedTableEntry{tag: 0, predictionCounter:0, usefulCounter:0});
    endmethod
    `endif

    `ifdef DEBUG_TAGETEST
        method Bit#(TAdd#(tagSize, indexSize)) debugGetHistory(HistoryRetrieve hr, Maybe#(Bit#(TLog#(SupSize))) count);
            Bit#(TAdd#(tagSize, indexSize)) hist = 0;
            if(hr == AFTER_RECOVERY)
                hist = folded.recoveredHistory;
            else if(hr == BEFORE_RECOVERY)
                if(count matches tagged Valid .num)
                    hist = folded.sameWindowHistory[num].history;
                else
                    hist = folded.history;
            return hist;
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
        return getHistory(recovered, pc, tagged Invalid);
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

    
    method Action decrementUsefulCounter(Bit#(indexSize) index);
        //match {.tag, .index} = getHistory(AFTER_RECOVERY, pc, tagged Invalid); // Need to use the recovered history!
        
        // Idea - seperate the useful counters? or some other way of doing this without a read. Could instead drag useful counters.
        TaggedTableEntry#(tagSize) entry = tab.sub(index);
        entry.usefulCounter = boundedUpdate(entry.usefulCounter, False);
        tab.upd(index, entry);
    endmethod

    // 3 bits 100 011
    method Action allocateEntry(Bit#(indexSize) index, Bit#(tagSize) tag, Bool taken);
        /*
            Need to remove last history bit to get the correct index
            Alternatively could drag the indices of every table in the training data.

            If recovered in this cycle - can use getHistory(True), otherwise we need to remove a bit.
        */    
        // Weakly taken = 100 - 1, weakly not taken = 100 - 1
        Bit#(PredCtrSz) counter_init = 1 << (valueOf(PredCtrSz)-1);
        if (!taken) begin
            counter_init = (1 << (valueOf(PredCtrSz)-1))-1;
        end
        
        TaggedTableEntry#(tagSize) toWrite = TaggedTableEntry{predictionCounter: counter_init, usefulCounter:  0, tag: tag};
        tab.upd(index, toWrite);
    endmethod
endmodule