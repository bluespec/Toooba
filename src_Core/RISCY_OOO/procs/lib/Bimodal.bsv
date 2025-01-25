import Types::*;
import BrPred::*;
import ProcTypes::*;

import RegFile::*;
import Vector::*;


export DirPredTrainInfo(..);
export BimodalTrainInfo(..);
export Entry;
export PCIndex;
export PCIndexSz;
export mkBimodal;
export BimodalSpecInfo;

typedef 12 PCIndexSz;
typedef Bit#(PCIndexSz) PCIndex;
typedef Bit#(2) Entry;

typedef struct {
    Entry counter;
    PCIndex pc;
} BimodalTrainInfo deriving(Bits, Eq, FShow);

typedef BimodalTrainInfo DirPredTrainInfo;
typedef Bit#(1) BimodalSpecInfo;

module mkBimodal(DirPredictor#(BimodalTrainInfo, BimodalSpecInfo));
    RegFile#(PCIndex, Entry) bimodal_table <- mkRegFileWCF(0, maxBound);
    Reg#(Addr) currentPc <- mkReg(?);

    Vector#(SupSize, DirPred#(BimodalTrainInfo, BimodalSpecInfo)) predIfc;
    Reg#(Bit#(1)) dummy <- mkReg(0);

    `ifdef DEBUG_DATA
    for(Integer i=0; i < valueOf(SupSize); i=i+1) begin
        predIfc[i] = (interface DirPred;
            method ActionValue#(DirPredResultWithDebugData#(BimodalTrainInfo, BimodalSpecInfo)) pred;
                PCIndex index = truncate(offsetPc(currentPc,i));
                Entry entry = bimodal_table.sub(index);
                $display("BIMODAL\n");

                return DirPredResultWithDebugData {
                    debugData: DebugData {
                        history: Invalid,
                        entryNumber: Valid({0,(unpack(index))}),
                        entryValues: Valid({0,(unpack(entry))})
                    },
                    predResult: DirPredResult {
                        taken: unpack(truncateLSB(entry)),
                        train: BimodalTrainInfo {
                            counter: entry,
                            pc: index
                        }
                    }
                };
            endmethod
        endinterface);
    end
    `else
        for(Integer i=0; i < valueOf(SupSize); i=i+1) begin
            predIfc[i] = (interface DirPred;
                method ActionValue#(DirPredResult#(BimodalTrainInfo, BimodalSpecInfo)) pred;
                    PCIndex index = truncate(offsetPc(currentPc,i));
                    Entry entry = bimodal_table.sub(index);
                    //$display("pc: %d bsv index: %d counter: %d\n", currentPc, index, entry);

                    return DirPredResult {
                        taken: unpack(truncateLSB(entry)),
                        train: BimodalTrainInfo {
                            counter: entry,
                            pc: index
                        },
                        spec: 0
                    };
                endmethod
            endinterface);
        end
    `endif

    
    interface pred = predIfc;

    method Action update(Bool taken, BimodalTrainInfo train, Bool mispred);
        Entry newEntry = train.counter;
        if(taken) begin
            newEntry = (newEntry == maxBound ? maxBound : newEntry + 1);
           end
        else begin
            newEntry = (newEntry == 0 ? 0 : newEntry - 1);
        end
        bimodal_table.upd(train.pc, newEntry);
    endmethod

    method Action nextPc(Addr pc);
        currentPc <= pc;
    endmethod

    method Action specRecover(BimodalSpecInfo dummy, Bool taken) = noAction;

    method flush = noAction;
    method flush_done = True;
endmodule