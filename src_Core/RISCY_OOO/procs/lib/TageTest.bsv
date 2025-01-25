import BrPred::*;
import RegFile::*;
import LFSR::*;
import Vector::*;

import TaggedTable::*;
import ProcTypes::*;
import Types::*;
import Tage::*;
import GlobalBranchHistory::*;
// For debugging
import Cur_Cycle :: *;

export TageTestTrainInfo;
export TageTestSpecInfo;
export Entry;
export PCIndex;
export PCIndexSz;
export mkTageTest;

`define NUM_TABLES 7
typedef TageTrainInfo#(`NUM_TABLES) TageTestTrainInfo;
typedef TageSpecInfo TageTestSpecInfo;

module mkTageTest(DirPredictor#(TageTrainInfo#(`NUM_TABLES), TageSpecInfo));
    Reg#(Bool) starting <- mkReg(True);
    Tage#(7) tage <- mkTage;
    Reg#(UInt#(64)) predCount <- mkReg(0);
    Reg#(UInt#(64)) misPredCount <- mkReg(0);
    Reg#(Addr) currentPc <- mkRegU;


    
    Vector#(SupSize, DirPred#(TageTrainInfo#(`NUM_TABLES), TageSpecInfo)) predIfc;
    for(Integer i=0; i < valueOf(SupSize); i=i+1) begin
        predIfc[i] = (interface DirPred;
        
        method ActionValue#(DirPredResult#(TageTrainInfo#(`NUM_TABLES), TageSpecInfo)) pred;
            //$display("Cycle %0d, TAGETEST, Prediction on %x\n", cur_cycle, currentPc);
            let result <- tage.dirPredInterface.pred[i].pred;
            return result;
        endmethod
        endinterface);
    end
    
    interface pred = predIfc;

    method Action update(Bool taken, TageTrainInfo#(`NUM_TABLES) train, Bool mispred);
        predCount <= predCount+1;
        if(mispred)
            misPredCount <= misPredCount + 1;
        $display("Cycle %0d, TAGETEST, predCount = %d, mispred Count = %d\n", cur_cycle, predCount, misPredCount);
        
        tage.dirPredInterface.update(taken, train, mispred);
    endmethod

    method Action nextPc(Addr pc);
        tage.dirPredInterface.nextPc(pc);
        currentPc <= pc;
    endmethod

    method Action specRecover(TageSpecInfo specInfo, Bool taken);
        tage.dirPredInterface.specRecover(specInfo, taken);
    endmethod

    method flush = noAction;
    method flush_done = True;
endmodule