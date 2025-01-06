import BrPred::*;
import RegFile::*;
import LFSR::*;
import Vector::*;

import TaggedTable::*;
import ProcTypes::*;
import Types::*;
import Tage::*;
import GlobalBranchHistory::*;

export TageTestTrainInfo;
export Entry;
export PCIndex;
export PCIndexSz;
export mkTageTest;

`define NUM_TABLES 7
typedef OOTageTrainInfo#(`NUM_TABLES) TageTestTrainInfo;

module mkTageTest(DirPredictor#(OOTageTrainInfo#(`NUM_TABLES)));
    Reg#(Bool) starting <- mkReg(True);
    Tage#(7) tage <- mkTage;

    
    Vector#(SupSize, DirPred#(OOTageTrainInfo#(`NUM_TABLES))) predIfc;
    for(Integer i=0; i < valueOf(SupSize); i=i+1) begin
        predIfc[i] = (interface DirPred;
        
        method ActionValue#(DirPredResult#(OOTageTrainInfo#(`NUM_TABLES))) pred;
            let result <- tage.dirPredInterface.pred[i].pred;
            return result;
        endmethod
        endinterface);
    end
    
    interface pred = predIfc;

    method Action update(Bool taken, OOTageTrainInfo#(`NUM_TABLES) train, Bool mispred);
        tage.dirPredInterface.update(taken, train, mispred);
    endmethod

    method Action nextPc(Addr pc);
        tage.dirPredInterface.nextPc(pc);
    endmethod

    method flush = noAction;
    method flush_done = True;
endmodule