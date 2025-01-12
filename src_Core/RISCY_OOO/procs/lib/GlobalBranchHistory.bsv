// Simple global history
// No speculative recovery or anything
import BrPred::*;
import BranchParams::*;
import Vector::*;
import ConfigReg::*;
import Ehr::*;
import ProcTypes::*;

interface RecoverMechanism#(numeric type length);
    method Action undo;
    `ifdef DEBUG
    method ActionValue#(Bit#(length)) debugUndo;
    `endif
endinterface

interface GlobalBranchHistory#(numeric type length);
    method Bit#(length) history;
    method Bit#(length) recoveredHistory;
    method Action addHistoryBits(Bit#(SupSize) taken, SupCnt count);
    method Action updateRecoveredHistory(Bit#(1) taken);
    interface Vector#(MaxSpecSize, RecoverMechanism#(length)) recoverFrom;
    `ifdef DEBUG
    method Action debugInitialise(Bit#(length) newHistory);
    `endif
endinterface

module mkGlobalBranchHistory(GlobalBranchHistory#(length));
    Ehr#(2, Bit#(length)) shift_register <- mkEhr(0);
    Ehr#(2, Bit#(MaxSpecSize)) last_removed_history <- mkEhr(0);
    
    PulseWire recover <- mkPulseWire;
    RWire#(Tuple2#(Bit#(SupSize), SupCnt)) updateHistoryData <- mkRWire;
    RWire#(Bit#(1)) updateRecoveredHistoryData <- mkRWire;

    Vector#(MaxSpecSize, RecoverMechanism#(length)) recoverIfc;

    (* no_implicit_conditions, fire_when_enabled *)
    rule updateHist(updateHistoryData.wget matches tagged Valid {.results, .count} &&& !recover);
        shift_register[1] <= truncateLSB({shift_register[1], reverseBits(results)} << count);
        Bit#(SupSize) bits = shift_register[1][valueOf(length)-1: valueOf(length)-valueOf(SupSize)];
        last_removed_history[1] <= truncateLSB({last_removed_history[1], bits} << count);
    endrule

    (* no_implicit_conditions, fire_when_enabled *)
    rule updateHistRecovered(updateRecoveredHistoryData.wget matches tagged Valid .taken &&& recover);
        shift_register[1] <= truncateLSB({shift_register[1], taken} << 1);
        last_removed_history[1] <= truncateLSB({last_removed_history[1], shift_register[1][valueOf(length)-1]} << 1);
    endrule

    function ActionValue#(Bit#(length)) undoHistory(Bit#(TLog#(MaxSpecSize)) i);
        actionvalue
            recover.send;
            UInt#(TLog#(MaxSpecSize)) j = unpack(i);
            Bit#(length) recovered = (last_removed_history[0][j:0] << (valueOf(length)-1)) >> i | truncateLSB(shift_register[0] >> (i+1));
            shift_register[0] <= recovered;

            last_removed_history[0] <= last_removed_history[0] >> (i+1);
            return recovered;
        endactionvalue
    endfunction

    for(Integer i = 0; i < valueOf(MaxSpecSize); i = i+1) begin
        recoverIfc[i] = (interface RecoverMechanism#(length);
            method Action undo;
                let a <- undoHistory(fromInteger(i));
            endmethod

            `ifdef DEBUG
            method ActionValue#(Bit#(length)) debugUndo;
                let ret <- undoHistory(fromInteger(i));
                return ret;
            endmethod
            `endif
        endinterface);
    end
    interface recoverFrom = recoverIfc;

    method Action addHistoryBits(Bit#(SupSize) taken, SupCnt count);
        updateHistoryData.wset(tuple2(taken, count));
        //update.send;
    endmethod

    method Action updateRecoveredHistory(Bit#(1) taken);
        updateRecoveredHistoryData.wset(taken);
        //update.send;
    endmethod

    method Bit#(length) history = shift_register[0];
    method Bit#(length) recoveredHistory  = shift_register[1];

    `ifdef DEBUG
    method Action debugInitialise(Bit#(length) newHistory);
        shift_register[0] <= newHistory;
    endmethod
    `endif
endmodule