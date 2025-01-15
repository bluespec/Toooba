import GlobalBranchHistory::*;
import BranchParams::*;
import BrPred::*;
import ProcTypes::*;

import Vector::*;
import ConfigReg::*; // Need to use this because of run rule reading the history
import Ehr::*;

import Assert::*;
// Assuming out of order updates, would actually be simpler with in order updates as I could keep a pointer
/*
Alternatively - why not simply recompute the global brnach history which will be easier since we will just shift and load
back in old values, then we can do a full recomputation of the folded history, however it may increase the cycle time

Also need to think about folding historu size, what if less than th

Periodically shift for recovery?

Multiple recovery updates to history? hopefully not possible but may need EHRs
*/
interface HistorySameWindow#(numeric type length);
    method Bit#(length) history;
endinterface

interface FoldedHistory#(numeric type length);
    method Bit#(length) history;
    method Bit#(length) recoveredHistory;
    method Action updateHistory(Bit#(SupSize) taken, SupCnt count);
    method Action updateRecoveredHistory(Bit#(1) taken);

    interface Vector#(MaxSpecSize, RecoverMechanism#(length)) recoverFrom;
    interface Vector#(SupSize, HistorySameWindow#(length)) sameWindowHistory;
        
    `ifdef DEBUG
    method Action debugInitialise(Bit#(length) newHistory);
    method Bit#(length) recomputedHistory(Bool recovery, Maybe#(Bit#(TLog#(SupSize))) count);
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
    Vector#(SupSize, HistorySameWindow#(length)) sameWindowHistoryIfc;

    // Some repeated code I should sort out at some point
    function Bit#(length) updatedHistory(Bit#(length) folded, Bit#(SupSize) eliminateBits, Bit#(SupSize) newHist, SupCnt count);
        Bit#(SupSize) new_bits = newHist ^ folded[valueOf(length)-1: valueOf(length)-valueOf(SupSize)];
        Bit#(length) new_folded_history = truncateLSB({folded, new_bits} << count);

        Bit#(SupSize) elim = 0;
        elim = truncateLSB({elim, eliminateBits} << count);
        for(Integer j = 0; j < valueOf(SupSize); j = j + 1) begin
            // Eliminate history out of bounds
            if(fromInteger(j) < count) begin
                Integer i = (histLength + j) % valueOf(length);
                new_folded_history[i] = new_folded_history[i] ^ elim[j];
            end
        end
        return new_folded_history;
    endfunction

    function Action updateWith(Bit#(SupSize) eliminateBits, Bit#(SupSize) newHistory, SupCnt count);
        action
            let newHist = reverseBits(newHistory);
           
            folded_history[1] <= updatedHistory(folded_history[1], eliminateBits, newHist, count);

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

    /*for(Integer i = 0; i < valueOf(SupSize); i = i+1) begin
        sameWindowHistoryIfc[i] = (interface HistorySameWindow#(length);
            method Bit#(length) history;
                return recompute(False, tagged Valid fromInteger(i));
            endmethod
        endinterface);
    end*/

    for(Integer i = 0; i < valueOf(SupSize); i = i+1) begin
    sameWindowHistoryIfc[i] = (interface HistorySameWindow#(length);
        method Bit#(length) history;
            if (i == 0) begin
                return folded_history[0];
            end
            else begin
                Bit#(SupSize) eliminateBits = global.history[histLength-1 : histLength-valueOf(SupSize)];
                return updatedHistory(folded_history[0], eliminateBits, 0, fromInteger(i));
            end
        endmethod
    endinterface);
    end

    interface recoverFrom = recoverIfc;

    interface sameWindowHistory = sameWindowHistoryIfc;

    //recompute(False, tagged Invalid);
    //recompute(True, tagged Invalid);
    method Bit#(length) history;
        return folded_history[0];
    endmethod

    method Bit#(length) recoveredHistory; 
        return folded_history[1];
    endmethod


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

    method Bit#(length) recomputedHistory(Bool recovery, Maybe#(Bit#(TLog#(SupSize))) count);
        Bit#(length) ret = 0;
        Bit#(GlobalHistoryLength) g = 0;
        
        if(recovery)
            g = global.recoveredHistory;
        else
            g = global.history;
            if(count matches tagged Valid .c)
                g = g << c;
        
        Integer div = histLength / valueOf(length);
        Integer rem = histLength - (div * valueOf(length));

        for(Integer i = 0; i < div; i = i + 1) begin
            Bit#(length) val = g[(i+1)*valueOf(length)-1:i*valueOf(length)];
            ret = ret ^ val;
        end
        
        if(rem > 0) begin
            Bit#(length) val2 = g[rem+(div*valueOf(length))-1 : div*valueOf(length)];
            ret = ret ^ val2;
        end
        return ret;
    endmethod
    `endif
endmodule