import GlobalBranchHistory::*;
import FoldedHistory::*;
import BrPred::*;
import Types::*;
import BranchParams::*;
import ProcTypes::*;
import RegFile::*;
import Util::*;

// Bimodal just uses the PC

// Two bit counters

typedef struct {
    Bit#(1) predBit;
} BimodalPredictionEntry deriving(Bits, Eq, FShow);

typedef struct {
    Bit#(1) hysteresisBit;
} BimodalHysteresisEntry deriving(Bits, Eq, FShow);

typedef struct {
    BimodalPredictionEntry pred;
    BimodalHysteresisEntry hyst;
} BimodalTableEntry deriving(Bits, Eq, FShow);


typedef Tuple2#(Bit#(pIndex), Bit#(hIndex)) BimodalInd#(numeric type pIndex, numeric type hIndex);

interface BimodalTable#(numeric type predIndexSize, numeric type hystIndexSize);
    method BimodalPredictionEntry accessPrediction(Addr pc);
    method BimodalInd#(predIndexSize, hystIndexSize) trainingInfo(Addr pc); // To be used in training    
    
    // No need to drag along the indices, as entry will always be the same
    method Action updateEntry(Addr pc, Bool taken);
    /// Debug
    `ifdef DEBUG
        method BimodalTableEntry debugGetEntry(BimodalInd#(predIndexSize, hystIndexSize) index);
    `endif
endinterface

module mkBimodalTable#(String predInitFile, String hystInitFile)(BimodalTable#(predIndexSize, hystIndexSize)) provisos(
    Add#(hystIndexSize, c_, predIndexSize),
    Add#(predIndexSize, a_, 64),
    Add#(hystIndexSize, b_, 64)
    );
    RegFile#(Bit#(predIndexSize), BimodalPredictionEntry) predTable <- mkRegFileWCF(0, maxBound);
    RegFile#(Bit#(hystIndexSize), BimodalHysteresisEntry) hystTable <- mkRegFileWCF(0, maxBound);

    
    function BimodalInd#(predIndexSize, hystIndexSize) pcToIndex(Addr pc);
        let combined = pack(pc) ^ (pack(pc) >> 2) ^ (pack(pc) >> 5);
        return tuple2(truncate(combined), truncate(combined >> valueOf(TSub#(predIndexSize, hystIndexSize))));
    endfunction

    method BimodalPredictionEntry accessPrediction(Addr pc);
        return predTable.sub(tpl_1(pcToIndex(pc)));
    endmethod

    method BimodalInd#(predIndexSize, hystIndexSize) trainingInfo(Addr pc); // To be used in training    
        return pcToIndex(pc);
    endmethod

    method Action updateEntry(Addr pc, Bool taken);
        // Will need to change this later
        match {.pred_ind, .hyst_ind} = pcToIndex(pc);
        Bit#(2) currentEntry = {pack(predTable.sub(pred_ind)), pack(hystTable.sub(hyst_ind))};
        currentEntry = boundedUpdate(currentEntry, taken);
        predTable.upd(pred_ind, unpack(currentEntry[1]));
        hystTable.upd(hyst_ind, unpack(currentEntry[0]));
    endmethod

    `ifdef DEBUG
        method BimodalTableEntry debugGetEntry(BimodalInd#(predIndexSize, hystIndexSize) index);
            return BimodalTableEntry{pred: predTable.sub(tpl_1(index)), hyst: hystTable.sub(tpl_2(index))};
        endmethod
    `endif
endmodule