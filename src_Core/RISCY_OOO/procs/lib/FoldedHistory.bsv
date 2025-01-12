import GlobalBranchHistory::*;
import BranchParams::*;
import BrPred::*;
import ProcTypes::*;

import Vector::*;
import ConfigReg::*; // Need to use this because of run rule reading the history
import Ehr::*;



// Assuming out of order updates, would actually be simpler with in order updates as I could keep a pointer
/*
Alternatively - why not simply recompute the global brnach history which will be easier since we will just shift and load
back in old values, then we can do a full recomputation of the folded history, however it may increase the cycle time

Also need to think about folding historu size, what if less than th

Periodically shift for recovery?

Multiple recovery updates to history? hopefully not possible but may need EHRs


*/

interface FoldedHistory#(numeric type length);
    method Bit#(length) history;
    method Bit#(length) recoveredHistory;
    method Action updateHistory(Bit#(SupSize) taken, SupCnt count);
    method Action updateRecoveredHistory(Bit#(1) taken);

    method Bit#(length) historyOneBefore;
    interface Vector#(MaxSpecSize, RecoverMechanism#(length)) recoverFrom;
    `ifdef DEBUG
    method Action debugInitialise(Bit#(length) newHistory);
    `endif
endinterface


module mkFoldedHistory#(Integer histLength, GlobalBranchHistory#(GlobalHistoryLength) global)(FoldedHistory#(length));
    Ehr#(2, Bit#(length)) folded_history <- mkEhr(0);
    
    // For out of order recovery of branch history
    Ehr#(2,Bit#(MaxSpecSize)) last_spec_outcomes <- mkEhr(0);
    Ehr#(2,Bit#(MaxSpecSize)) last_removed_history <- mkEhr(0);

    PulseWire recover <- mkPulseWire;

    RWire#(Tuple2#(Bit#(1), Bit#(1))) historyRecoveredUpdateData <- mkRWire;
    RWire#(Tuple3#(Bit#(SupSize), Bit#(SupSize), SupCnt)) historyUpdateData <- mkRWire;

    Vector#(MaxSpecSize, RecoverMechanism#(length)) recoverIfc;

    function Action updateWith(Bit#(SupSize) eliminateBits, Bit#(SupSize) newHistory, SupCnt count);
        action
            let folded = folded_history[1];
            let newHist = reverseBits(newHistory);
            Bit#(SupSize) new_bits = newHist ^ folded[valueOf(length)-1: valueOf(length)-valueOf(SupSize)];
            Bit#(length) new_folded_history = truncateLSB({folded, new_bits} << count);

            for(Integer j = 0; j < valueOf(SupSize); j = j + 1) begin
                // Eliminate history out of bounds
                if(fromInteger(j) < count) begin
                    Integer i = (histLength - j) % valueOf(length);
                    new_folded_history[i] = new_folded_history[i] ^ reverseBits(eliminateBits)[j];
                end
            end
            folded_history[1] <= new_folded_history;

            // For recovery updates 0001, 1000
            last_spec_outcomes[1] <= truncateLSB({last_spec_outcomes[1], newHist} << count);
            last_removed_history[1] <= truncateLSB({last_removed_history[1], eliminateBits} << count);
        endaction
    endfunction

    // Normal update
    (* no_implicit_conditions, fire_when_enabled *)
    rule updateHist(!recover &&& historyUpdateData.wget matches tagged Valid {.eliminateBits, .newHistory, .count});
        updateWith(eliminateBits, newHistory, count);
    endrule

    (* no_implicit_conditions, fire_when_enabled *)
    rule updateHistRecovered(recover &&& historyRecoveredUpdateData.wget matches tagged Valid {.eliminateBit, .newHistory});
        updateWith({eliminateBit,0}, zeroExtend(newHistory), 1);
    endrule

    // Recovery
    function Bit#(length) getUndidHistory(Bit#(TLog#(MaxSpecSize)) i, Bit#(TLog#(length)) shiftNum);
        UInt#(TLog#(MaxSpecSize)) recoverIndex = unpack(i); 
        // Restore deleted historu
        Bit#(length) recovered = folded_history[0];
        Integer j = histLength % valueOf(length);
        for(Integer k = 0; k < valueOf(MaxSpecSize); k = k +1) begin                    
            if(fromInteger(k) <= i) begin
                Bit#(1) eliminateBit = last_removed_history[0][k];
                Integer position = (j + k) % valueOf(length);
                recovered[position] = eliminateBit^recovered[position];
            end
        end
        
        Bit#(length) removed = recovered[recoverIndex:0] ^ last_spec_outcomes[0][recoverIndex:0];
        recovered = (removed[recoverIndex:0] << shiftNum) | truncateLSB(recovered >> (i+1));

        return recovered;
    endfunction

    function ActionValue#(Bit#(length)) undoHistory(Bit#(TLog#(MaxSpecSize)) i, Bit#(TLog#(length)) shiftNum);
        actionvalue
            recover.send;
            let recovered = getUndidHistory(i, shiftNum);
            folded_history[0] <= recovered;
            
            last_removed_history[0] <= last_removed_history[0] >> (i+1);
            last_spec_outcomes[0] <= last_spec_outcomes[0] >> (i+1);
            return recovered;
        endactionvalue
    endfunction


    for(Integer i = 0; i < valueOf(MaxSpecSize); i = i+1) begin
        recoverIfc[i] = (interface RecoverMechanism#(length);
            method Action undo;
                let a <- undoHistory(fromInteger(i), fromInteger(valueOf(length)-1-i));
            endmethod

            `ifdef DEBUG
            method ActionValue#(Bit#(length)) debugUndo;
                let ret <- undoHistory(fromInteger(i), fromInteger(valueOf(length)-1-i));
                return ret;
            endmethod
            `endif
        endinterface);
    end

    interface recoverFrom = recoverIfc;

    method Bit#(length) history = folded_history[0];

    method Bit#(length) recoveredHistory = folded_history[1];

    method Bit#(length) historyOneBefore = getUndidHistory(0,fromInteger(valueOf(length)-1));

    // How to know the pointer? Realistically commit stage cannot know
    // If in order then fetch stage will know which branch because we can keep a pointer
    // But that also requires sending back correct updates to the global history

    method Action updateHistory(Bit#(SupSize) newHistory, SupCnt count);
        // Shift and add new history bit, with older history
        Integer i = histLength % valueOf(length);
        Bit#(SupSize) eliminateBits = global.history[histLength-1 : histLength-valueOf(SupSize)];
        historyUpdateData.wset(tuple3(eliminateBits, newHistory, count));
    endmethod

    method Action updateRecoveredHistory(Bit#(1) taken);
        Integer i = histLength % valueOf(length);
        Bit#(1) eliminateBit = global.recoveredHistory[histLength-1];
        historyRecoveredUpdateData.wset(tuple2(eliminateBit, taken));
    endmethod

    `ifdef DEBUG
    method Action debugInitialise(Bit#(length) newHistory);
        folded_history[0] <= newHistory;
    endmethod
    `endif
endmodule
//if(lat[j].wget matches tagged Valid .x)