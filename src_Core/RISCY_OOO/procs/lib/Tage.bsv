import RegFile::*;
import LFSR::*;
import Vector::*;
import List::*;
import HList::*;

import ProcTypes::*;
import Types::*;
import BrPred::*;
import BranchParams::*;
import BimodalTable::*;
import TaggedTable::*;
import GlobalBranchHistory::*;
import Ehr::*;
import Util::*;
import CircBuff::*;

// For debugging
import Cur_Cycle :: *;
/*
export DirPredTrainInfo(..);
export TageTrainInfo(..);
export Addr;
export Entry;
export PCIndex;
export PCIndexSz;
export Tage;
export mkTage;
*/

// Not a reasonable solution


`define WRAP_CODE(body) \
    action /* body */ endaction

`define WRAP_CODE_NON_ACTION(body) \
    /* body */

`define CASE_ALL_TABLES(varName, body) \
    case (varName) matches \
    tagged T_9_9_5 .t : begin  \
        `WRAP_CODE_NON_ACTION(body) \
    end \
    tagged T_9_9_9 .t : begin \
        `WRAP_CODE_NON_ACTION(body) \
    end \
    tagged T_9_10_15 .t : begin \
        `WRAP_CODE_NON_ACTION(body) \
    end \
    tagged T_9_10_25 .t : begin \
        `WRAP_CODE_NON_ACTION(body) \
    end \
    tagged T_9_11_44 .t : begin \
        `WRAP_CODE_NON_ACTION(body) \
    end \
    tagged T_9_11_76 .t : begin \
        `WRAP_CODE_NON_ACTION(body) \
    end \
    tagged T_9_12_130 .t : begin \
        `WRAP_CODE_NON_ACTION(body) \
    end \
    endcase \

// Macro or type definition?
`define MAX_TAGGED 12
`define MAX_INDEX_SIZE 10
`define METAPREDICTOR_CTR_SIZE 4 //ALT_ON_NA

typedef 12 PCIndexSz;
typedef Bit#(PCIndexSz) PCIndex;
typedef Bit#(2) Entry;
typedef Bit#(TLog#(numTables)) TableIndex#(numeric type numTables);

typedef Tuple2#(Maybe#(Tuple3#(Bit#(TLog#(num)), TaggedTableEntry#(`MAX_TAGGED), Bit#(`MAX_INDEX_SIZE))), Maybe#(Tuple2#(Bit#(TLog#(num)), TaggedTableEntry#(`MAX_TAGGED)))) PredictionTableInfo#(numeric type num);


typedef struct {
    Bit#(TLog#(numTables)) provider_table;
    Bit#(`MAX_INDEX_SIZE) index;
    TaggedTableEntry#(`MAX_TAGGED) provider_entry;
} ProviderTrainInfo#(numeric type numTables) deriving (Eq, FShow, Bits);

typedef struct{
    Bool use_alt;
    Bool provider_prediction; // prediction of the provider
    Bool alt_prediction; // prediction of the alternative table
    Bool taken; // Outcome

    Maybe#(ProviderTrainInfo#(numTables)) provider_info;
    Maybe#(Bit#(TLog#(numTables))) alt_table;
    // Redundancy, can easily be checked with taken and provider but not efficient?

    Bit#(numTables) replaceableEntries;
    Addr pc;
} TageTrainInfo#(numeric type numTables) deriving(Bits, Eq, FShow);

typedef struct {
    TageTrainInfo#(numTables) tageInfo;
    CircBuffIndex#(MaxSpecSize) ooIndex;
} OOTageTrainInfo#(numeric type numTables) deriving(Bits, Eq, FShow);

typedef struct {
    TageTrainInfo#(numTables) tageInfo;
    Bool mispred;
    Bool taken;
} UpdateInfo#(numeric type numTables) deriving(Bits, Eq, FShow);

typedef OOTageTrainInfo DirPredTrainInfo;

// Abolutely terrible, is there an easier way to parametrise this?

typedef union tagged {
    TaggedTable#(9,9,5) T_9_9_5;
    TaggedTable#(9,9,9) T_9_9_9;
    TaggedTable#(9,10,15) T_9_10_15;
    TaggedTable#(9,10,25) T_9_10_25;
    TaggedTable#(9,11,44) T_9_11_44;
    TaggedTable#(9,11,76) T_9_11_76;
    TaggedTable#(9,12, 130) T_9_12_130;
} ChosenTaggedTables deriving(Bits);

typedef union tagged {
    TaggedTableEntry#(9) Tag9;
    TaggedTableEntry#(10) Tag10;
    TaggedTableEntry#(11) Tag11;
    TaggedTableEntry#(12) Tag12;
} TaggedEntrySizes deriving(Bits);

interface Tage#(numeric type numTables);
    interface DirPredictor#(OOTageTrainInfo#(numTables)) dirPredInterface;
    
    `ifdef DEBUG
        method Action debugTables(Addr pc);
        method ActionValue#(Maybe#(TableIndex#(numTables))) debugMispredictAllocation(TageTrainInfo#(numTables) train, Bool taken);
        method Action debugAllocate(Addr pc, Bit#(TLog#(numTables)) tableNum);
        method Action debugResetEntry(Addr pc, Bit#(TLog#(numTables)) tableNum);
        method TaggedTableEntry#(`MAX_TAGGED) debugGetEntry(Addr pc, Bit#(TLog#(numTables)) tableNum);
        method PredictionTableInfo#(numTables) debugPredAltpred;
    `endif
endinterface


module mkTage(Tage#(numTables)) provisos(
    Bits#(OOTageTrainInfo#(numTables), a__),
    Add#(1, b__, TLog#(TAdd#(1, numTables))),
    Add#(c__, numTables, 20)
);
    GlobalBranchHistory#(GlobalHistoryLength) global <- mkGlobalBranchHistory;

    TaggedTable#(9,9,5)     t1 <- mkTaggedTable(global);
    TaggedTable#(9,9,9)     t2 <- mkTaggedTable(global);
    TaggedTable#(9,10,15)   t3 <- mkTaggedTable(global);
    TaggedTable#(9,10,25)   t4 <- mkTaggedTable(global);
    TaggedTable#(9,11,44)   t5 <- mkTaggedTable(global);
    TaggedTable#(9,11,76)   t6 <- mkTaggedTable(global);
    TaggedTable#(9,12,130)  t7 <- mkTaggedTable(global);

    
    BimodalTable#(13, 11) bimodalTable <- mkBimodalTable(regInitFilenameBimodalPred, regInitFilenameBimodalHyst);
    Vector#(7, ChosenTaggedTables) taggedTablesVector = cons(T_9_9_5(t1), cons(T_9_9_9(t2), cons(T_9_10_15(t3), cons(T_9_10_25(t4), cons(T_9_11_44(t5), cons(T_9_11_76(t6), cons(T_9_12_130(t7), nil)))))));
    Reg#(Addr) currentPc <- mkRegU;
    Reg#(UInt#(`METAPREDICTOR_CTR_SIZE)) alt_on_na <- mkReg(1 << (`METAPREDICTOR_CTR_SIZE-1));
    CircBuff#(MaxSpecSize, Bool) ooBuff <- mkCircBuff;

    PulseWire mispredictWire <- mkPulseWire;

    Ehr#(TAdd#(1, SupSize), SupCnt) numPred <- mkEhr(0);
    Ehr#(TAdd#(1, SupSize), Bit#(SupSize)) predResults <- mkEhr(0);

    // For the LFSR
    LFSR#(Bit#(4)) lfsr <- mkLFSR_4;
    Reg#(Bool) starting <- mkReg(True);

    Vector#(SupSize, DirPred#(OOTageTrainInfo#(numTables))) predIfc;
  
    function Bool useAlt;
        return unpack(pack(alt_on_na)[`METAPREDICTOR_CTR_SIZE-1]);
    endfunction

    function Tuple2#(Vector#(numTables,Maybe#(Bit#(TLog#(numTables)))), Vector#(numTables,Maybe#(Bit#(TLog#(numTables))))) treeFindPred(Integer len, Integer depth, Vector#(numTables,Maybe#(Bit#(TLog#(numTables)))) entries_compare, Vector#(numTables,Maybe#(Bit#(TLog#(numTables)))) altpred_compare);
        
        if (depth == 0) return tuple2(entries_compare, altpred_compare);
        else begin
            Vector#(numTables,Maybe#(Bit#(TLog#(numTables)))) pred = replicate(tagged Invalid);
            Vector#(numTables,Maybe#(Bit#(TLog#(numTables)))) altpred = replicate(tagged Invalid);

                // Is this compile time or is it forced to be sequential???
            for (Integer j = 0; j < 2*(len/2); j = j + 2) begin
                if (entries_compare[j+1] matches tagged Valid .x) begin
                    if(altpred_compare[j+1] matches tagged Valid .x)
                        altpred[j / 2] = altpred_compare[j+1];
                    else
                        altpred[j / 2] = entries_compare[j];  
                    pred[j / 2] = entries_compare[j+1];
                    
                end
                else begin
                    pred[j / 2] = entries_compare[j];
                    altpred[j / 2] = altpred_compare[j];
                end
            end
            if (len % 2 == 1) begin
                pred[(len/2)] = entries_compare[len-1];
                altpred[(len/2)] = tagged Invalid;
            end
            Integer len2 = (len / 2) + (len % 2);
        return treeFindPred(len2, depth - 1, pred, altpred);
        end
    endfunction

    function Tuple2#(PredictionTableInfo#(numTables), Bit#(numTables)) find_pred_altpred(Addr pc, Bit#(TLog#(SupSize)) numPred);
        Vector#(numTables,Maybe#(Bit#(TLog#(numTables)))) entries_compare = replicate(tagged Invalid);
        Vector#(numTables,Maybe#(Bit#(TLog#(numTables)))) altpred_compare = replicate(tagged Invalid);
        Bit#(numTables) replaceableEntries = 0;
        
        Vector#(numTables,TaggedTableEntry#(`MAX_TAGGED)) entries = replicate(TaggedTableEntry{tag:0, predictionCounter:0, usefulCounter:0});
        Vector#(numTables,Bit#(`MAX_INDEX_SIZE)) indices = replicate(0);

        
        for(Integer j = 0; j < valueOf(numTables); j=j+1) begin
            ChosenTaggedTables tab = taggedTablesVector[j];    
            `CASE_ALL_TABLES(tab, 
            (*/
                // Could do this in one
                match {.tag, .index}  = t.accessPredInfo[numPred].access(pc);
                let entry = t.access_wrapped_entry(pc, truncate(index));
                replaceableEntries[j] = pack(entry.usefulCounter == 0);
                
                indices[j] = index;
                entries[j] = entry;
                if (tag == entry.tag) begin
                    entries_compare[j] = tagged Valid fromInteger(j);
                end
                /*)
            )
        end
        
        Integer len = valueOf(numTables);
        match{.pred_vec, .altpred_vec} = treeFindPred(len, valueOf(TLog#(numTables)), entries_compare, altpred_compare);

        PredictionTableInfo#(numTables) ret = tuple2(tagged Invalid, tagged Invalid);
        
        // formout output
        if (pred_vec[0] matches tagged Valid .x)
            if (altpred_vec[0] matches tagged Valid .y)
                ret = tuple2(tagged Valid tuple3(x, entries[x], indices[x]), tagged Valid tuple2(y, entries[y]));
            else
                ret = tuple2(tagged Valid tuple3(x, entries[x], indices[x]), tagged Invalid);
        
        return tuple2(ret, replaceableEntries);
    endfunction

    // WARNING - REMOVE ACTIONVALUE AFTER DEBUG
    function Action allocate(TageTrainInfo#(numTables) train, Bool taken);
        action
        Maybe#(TableIndex#(numTables)) ret = tagged Invalid;
        
        if(train.provider_info matches tagged Valid .inf &&& inf.provider_table == fromInteger(valueOf(numTables)-1)) begin
            `ifdef DEBUG_TAGETEST
                $display("Do Nothing\n");
            `endif
        end
        else begin
            // Do this on prediction as we access all tables then anyway (?)
            Bit#(TLog#(numTables)) start = 0;
            if(train.provider_info matches tagged Valid .inf) begin
                // Is this too expensive? Is there a better way to implement the circuit than adding?
                start = inf.provider_table+1; // Could use a mask for start instead?
            end

            // Remove all entries before the starting table we are considering
            Bit#(numTables) tabsReplaceable = (train.replaceableEntries >> start) << start;
            if(tabsReplaceable == 0) begin
                // Decrement all counters as in original TAGE, worried about the circuitry there
                for(Integer i = 0; i < valueOf(numTables); i = i + 1) begin
                    if  (fromInteger(i) >= start) begin
                        let tab = taggedTablesVector[i];
                        `CASE_ALL_TABLES(tab, (*/ t.decrementUsefulCounter(train.pc); /*))
                    end
                end
            end
            else begin
                // overkill?
                Bit#(TAdd#(numTables,1)) num = 1 << countOnes(tabsReplaceable);
                
                Bit#(3) randNum = {lfsr.value[2:1], lfsr.value[0] | lfsr.value[3]};
                Bool found = False;
                TableIndex#(numTables) ind = 0;
                
                `ifdef DEBUG
                $display(fshow(start), " Rand:", fshow(randNum),  " Number to choose:", fshow(num), "\n");
                $display("%b\n",train.replaceableEntries);
                $display("%b\n",tabsReplaceable);
                `endif

                // A better way than sequential?
                for(Integer i = 0; i < valueOf(numTables); i = i + 1) begin
                    if(unpack(tabsReplaceable[i])) begin
                        if(!found && (num == 'b10 || unpack(randNum[2]))) begin
                            ind = fromInteger(i);
                            found = True;
                        end
                        randNum = randNum << 1;
                        num = num >> 1;
                    end
                end

                `ifdef DEBUG_TAGETEST
                    Bit#(20) index = 0;
                    Bit#(`MAX_TAGGED) tag = 0;
                    let tab = taggedTablesVector[ind];
                    `CASE_ALL_TABLES(tab, (*/ index = zeroExtend(tpl_2(t.trainingInfo(train.pc, AFTER_RECOVERY))); tag = zeroExtend(tpl_1(t.trainingInfo(train.pc, AFTER_RECOVERY))); /*))
                    $display("TAGETEST ALLOCATE FOR %d %d %d %d\n",train.pc, ind, index, tag);
                `endif

                `CASE_ALL_TABLES(taggedTablesVector[ind], (*/ t.allocateEntry(train.pc, taken); /*))
            end
        end
    endaction
    endfunction

    function Action updateWithTrain(Bool taken, TageTrainInfo#(numTables) train, Bool mispred);
        action

        `ifdef DEBUG_TAGETEST
            `CASE_ALL_TABLES(taggedTablesVector[0], (*/ $display("TAGETEST UPDATE FOLDING %b\n", t.debugGetHistory(AFTER_RECOVERY, tagged Invalid)) ;/*))
        `endif

        // Update bimodal table either way
        bimodalTable.updateEntry(train.pc, taken);
        Bool mispredict = taken != train.taken;

        // Tagged tables update
        if(train.provider_info matches tagged Valid .info) begin
            let entry = info.provider_entry;
            // Useful counters
            UsefulCtrUpdate u = PRESERVE;
            if(train.provider_prediction == taken && train.alt_prediction != taken)
                u = INCREMENT;
            else if(train.provider_prediction != taken && train.alt_prediction == taken)
                u = DECREMENT;

            ChosenTaggedTables providerTable = taggedTablesVector[info.provider_table];
            `CASE_ALL_TABLES(providerTable, (*/ t.updateEntry(info.index, entry.tag, taken, u); /*))

            // ALT_ON_NA
            `ifdef DEBUG_TAGETEST
                if(train.alt_table matches tagged Valid .alt_t) begin
                $display("TAGETEST UPDATE ALT PRED TABLE %d\n", alt_t);
                end
                else
                    $display("TAGETEST UPDATE ALT PRED BIMODAL\n");
                $display("TAGETEST UPDATE ALT PRED TAKEN %d\n", train.alt_prediction);
                $display("TAGETEST PROVIDR ENTRY COUNTER %d\n", entry.predictionCounter);
                $display("TAGETEST PROVIDR ENTRY USEFUL %d\n", entry.usefulCounter);
            `endif

            if(entry.usefulCounter == 0 && weakCounter(entry.predictionCounter)) begin
                if(train.alt_prediction != train.provider_prediction)
                    alt_on_na <= unpack(boundedUpdate(pack(alt_on_na), train.alt_prediction == taken));
            end
        end

        // Update LSFR
        `ifdef GOLD_STANDARD
            lfsr.next;
        `endif
    endaction
    endfunction

    
    //rule updateHistory(!mispredictWire); NOT SURE ABOUT THIS -------------------------------------
    (* no_implicit_conditions, fire_when_enabled *)
    rule updateHistory;
        //if(!mispredictWire) begin
        // Update history speculatively
            let num = numPred[valueOf(SupSize)];
            let results = predResults[valueOf(SupSize)];

            
            if(num != 0) begin
                `ifdef DEBUG_TAGETEST   
                $display("TAGETEST Update history, cycle %d\n", cur_cycle);
                $display("TAGETEST Global %b\n", global.history);
                `endif

                global.addHistoryBits(results, num);
                for(Integer i = 0; i < valueOf(numTables); i = i+1) begin
                    let tab = taggedTablesVector[i];
                    `CASE_ALL_TABLES(tab, (*/ t.updateHistory(results, num); /*))
                end
            end
      //  end
    endrule

    (* no_implicit_conditions, fire_when_enabled *)
    rule canonUpdate;
        //Update LFSR
        if(starting) begin
            lfsr.seed('d9);
            starting <= False;
        end
        else begin
            `ifndef GOLD_STANDARD
                lfsr.next;
            `endif

            numPred[valueOf(SupSize)] <= 0;
            predResults[valueOf(SupSize)] <= 0;
        end
    endrule

    /*
    (* no_implicit_conditions, fire_when_enabled *)
    rule scheduleUpdate(!starting);
        let updateThisCycle <- ooBuff.retrieveNext;
        (*split*)
        if(updateThisCycle matches tagged Valid .train) (* nosplit *) begin
            updateWithTrain(train.taken, train.tageInfo, train.mispred, train.allocateInfo);
        end
    endrule
    */

    for(Integer i=0; i < valueOf(SupSize); i=i+1) begin
        predIfc[i] = (interface DirPred;
        
        method ActionValue#(DirPredResult#(OOTageTrainInfo#(numTables))) pred;
            
            TageTrainInfo#(numTables) ret = unpack(0);
            Addr pc = offsetPc(currentPc, i);

            `ifdef DEBUG_TAGETEST
                $display("TAGETEST LFSR %d\n", lfsr.value);
                $display("TAGETEST ALT_ON_NA %d\n", alt_on_na);
            `endif

            `ifdef DEBUG_TAGETEST
                `CASE_ALL_TABLES(taggedTablesVector[0], (*/ $display("TAGETEST PRED FOLDING %b\n", t.debugGetHistory(BEFORE_RECOVERY, tagged Valid truncate(numPred[i]))) ;/*))
            `endif


            // Retrieve provider and alternative table
            match {{.pred, .altpred}, .replaceableEntries} = find_pred_altpred(pc, truncate(numPred[i]));
            ret.replaceableEntries = replaceableEntries;
            ret.pc = pc;

            
            if(pred matches tagged Valid {.pred_index, .pred_entry, .pred_table_index}) begin 
                Bool prediction = takenFromCounter(pred_entry.predictionCounter);
                ret.provider_prediction = prediction;

                `ifdef DEBUG_TAGETEST
                    $display("TAGETEST TABLE INDEX %d %d %d\n",pred_index, pred_table_index, pred_entry.tag);
                `endif
                
                // Get the index to avoid recomputing on update (not possible unless mispredict)
                
                ret.provider_info = tagged Valid ProviderTrainInfo{index: pred_table_index, provider_table: pred_index, provider_entry: pred_entry};
                if (altpred matches tagged Valid {.alt_index, .alt_entry}) begin
                    ret.alt_table = tagged Valid alt_index;

                    `ifdef DEBUG_TAGETEST
                    $display("TAGETEST ALT ENTRY %d tag:%d\n", alt_index, alt_entry.tag);
                    `endif
                    
                    Bool alt_prediction = takenFromCounter(alt_entry.predictionCounter);                 
                    ret.alt_prediction = alt_prediction;

                    if(pred_entry.usefulCounter == 0 && weakCounter(pred_entry.predictionCounter) && useAlt) begin
                        ret.taken = alt_prediction;
                        ret.use_alt = True;
                    end
                    else begin
                        ret.use_alt = False;
                        ret.taken = prediction;
                    end
                end
                else begin
                    ret.alt_table = tagged Invalid;
                    Bool bimodal_prediction = unpack(pack(bimodalTable.accessPrediction(pc)));
                    ret.alt_prediction = bimodal_prediction;
                    ret.use_alt = False;
                    ret.taken = prediction;
                end
            end
            else begin

                `ifdef DEBUG_TAGETEST
                    $display("TAGETEST USE BIMODAL\n");
                `endif
                ret.alt_table = tagged Invalid;
                ret.provider_info = tagged Invalid;
                ret.use_alt = False;
                // Maybe automatically trigger a read from bimodal table ever time nextPc is set?
                Bool prediction = unpack(pack(bimodalTable.accessPrediction(pc)));
                ret.provider_prediction = prediction;
                ret.taken = prediction;
            end

            // Update counters and results
            numPred[i] <= numPred[i] + 1;
            predResults[i] <= predResults[i] | (zeroExtend(pack(ret.taken)) << numPred[i]);
            let ooIndex <- ooBuff.specAssign[i].specAssign;

            `ifdef DEBUG_TAGETEST   
                $display("TAGETEST Prediction on: %x,%d, Taken: %d, cycle %d\n", currentPc , i, ret.taken, cur_cycle);
                $display("TAGETEST Id given = %d\n",ooIndex);
            `endif

            // Also update histories
            return DirPredResult {
                taken: ret.taken,
                train: OOTageTrainInfo{
                    tageInfo: ret,
                    ooIndex: ooIndex
                }
            };
        endmethod
        endinterface);
    end

    `ifdef DEBUG
    method Action debugTables(Addr pc);
        for(Integer i = 0; i < 7; i=i+1) begin
            ChosenTaggedTables tab = taggedTablesVector[i];
            `CASE_ALL_TABLES(tab, (*/match {.c, .d} = t.trainingInfo(pc, BEFORE_RECOVERY); $display("%d %d\n", c, d);/*))    
        end
    endmethod


    method PredictionTableInfo#(numTables) debugPredAltpred;
        return tpl_1(find_pred_altpred(currentPc));
    endmethod

    method Action debugAllocate(Addr pc, Bit#(TLog#(numTables)) tableNum);
        ChosenTaggedTables tab = taggedTablesVector[tableNum];
        //`CASE_ALL_TABLES(tab, (*/ t.allocateEntry(pc, True); /*))
    endmethod

    method Action debugResetEntry(Addr pc, Bit#(TLog#(numTables)) tableNum);
        ChosenTaggedTables tab = taggedTablesVector[tableNum];
        `CASE_ALL_TABLES(tab, (*/ t.debugUnsetEntry(pc); /*))
    endmethod

    method ActionValue#(Maybe#(TableIndex#(numTables))) debugMispredictAllocation(TageTrainInfo#(numTables) train, Bool taken);
        let ind <- allocateGivenEntry(train, taken);
        return ind;
    endmethod

    method TaggedTableEntry#(`MAX_TAGGED) debugGetEntry(Addr pc, Bit#(TLog#(numTables)) tableNum);
        ChosenTaggedTables tab = taggedTablesVector[tableNum];
        `CASE_ALL_TABLES(tab, (*/ return t.access_wrapped_entry(pc); /*))
    endmethod
    `endif


    interface  dirPredInterface = interface DirPredictor#(OOTageTrainInfo);
        interface pred = predIfc;
        method Action update(Bool taken, OOTageTrainInfo#(numTables) train, Bool mispred);
            (* split *)
            if(mispred) (* nosplit *) begin
                mispredictWire.send;
                
                let numBits <- ooBuff.handleMispred(train.ooIndex);
                // Recover histories first, then update bit
                let recoverNumber = numBits;
                global.recoverFrom[recoverNumber].undo;
                global.updateRecoveredHistory(pack(taken));

                `ifdef DEBUG_TAGETEST   
                $display("TAGETEST Misprediction on %x, cycle %d\n", train.tageInfo.pc, cur_cycle);
                `endif
                for (Integer i = 0; i < valueOf(numTables); i = i +1) begin
                    let tab = taggedTablesVector[i];
                    /* It is untested if this will work for Toooba */
                    `CASE_ALL_TABLES(tab, (*/ t.recoverHistory(recoverNumber); t.updateRecovered(pack(taken)); /*))
                end 
                // Retrieve allocation information for next update.
                ooBuff.enqueue(True, train.ooIndex);

                let trainInfo = train.tageInfo;
                allocate(trainInfo, taken);
                updateWithTrain(taken, trainInfo, mispred);
            end
            else (* nosplit *) begin
                ooBuff.enqueue(True, train.ooIndex);
                let trainInfo = train.tageInfo;
                updateWithTrain(taken, trainInfo, mispred);
                `ifdef DEBUG_TAGETEST
                $display("TAGETEST correct prediction on %x, cycle %d\n", train.tageInfo.pc, cur_cycle);
                `endif
            end
            
        endmethod
    
        method Action nextPc(Addr pc);
            currentPc <= pc;
        endmethod
    
        method flush = noAction;
        method flush_done = True;
    endinterface;
endmodule

/*
Functionality to check

- Bimodal table operations
- Functions for checking if a counter is taken and for checking if a counter is weak
- Most of the prediction phase


*/