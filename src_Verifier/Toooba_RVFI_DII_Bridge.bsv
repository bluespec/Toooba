/*-
 * Copyright (c) 2018-2019 Peter Rugg
 * Copyright (c) 2020 Jonathan Woodruff
 * All rights reserved.
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory (Department of Computer Science and
 * Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
 * DARPA SSITH research programme.
 *
 * @BERI_LICENSE_HEADER_START@
 *
 * Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
 * license agreements.  See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.  BERI licenses this
 * file to you under the BERI Hardware-Software License, Version 1.0 (the
 * "License"); you may not use this file except in compliance with the
 * License.  You may obtain a copy of the License at:
 *
 *   http://www.beri-open-systems.org/legal/license-1-0.txt
 *
 * Unless required by applicable law or agreed to in writing, Work distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations under the License.
 *
 * @BERI_LICENSE_HEADER_END@
 */

package Toooba_RVFI_DII_Bridge;

// ================================================================
// BSV library imports

import Vector       :: *;
import FIFO         :: *;
import FIFOF        :: *;
import SpecialFIFOs :: *;
import GetPut       :: *;
import ClientServer :: *;
import ConfigReg    :: *;

// ================================================================
// Project imports

import Types::*;
import ProcTypes::*;

//import Verifier  :: *;
import RVFI_DII  :: *;

// ================================================================

interface Toooba_RVFI_DII_Bridge_IFC;
    interface Toooba_RVFI_DII_Server rvfi_dii_server;
    interface Server#(Dii_Id, InstsAndIDs) dii;
    interface Put#(Rvfi_Traces) rvfi;
endinterface

module mkTooobaRVFIDIIBridge(Toooba_RVFI_DII_Bridge_IFC);
    // DII state
    FIFOF#(Tuple2#(Bit#(32), Dii_Id)) dii_in <- mkUGFIFOF;
    Reg#(InstsAndIDs) buff <- mkConfigRegU;
    FIFOF#(InstsAndIDs) instrs <- mkSizedFIFOF(128);
    // RVFI state
    FIFO#(Rvfi_Traces) report_vectors <- mkSizedFIFO(128);
    FIFO#(RVFI_DII_Execution#(DataSz,DataSz)) reports <- mkSizedFIFO(256);
    // Request ID
    FIFO#(Dii_Id) seq_req <- mkFIFO;

    Bit#(32) nop = 'h01FFF033;
    
    Bool verbose = True;
    
    function Bool validReport(RVFI_DII_Execution#(DataSz,DataSz) trace);
        return (trace.rvfi_order != -1 && trace.rvfi_insn != nop);
    endfunction
    
    Reg#(SupWaySel) report_select <- mkReg(0);
    rule split_report_vectors;
        RVFI_DII_Execution#(DataSz,DataSz) report = report_vectors.first[report_select];
        if (verbose)
                    $display("%t RVFI response: ", $time,
                        fshow(report_vectors.first[report_select])
                    );
        if (validReport(report)) reports.enq(report);
        if (report_select == -1) report_vectors.deq();
        report_select <= report_select + 1;
    endrule
    
    Reg#(SupWaySel) buffLvl <- mkConfigReg(0);
    rule bufferInsts(buffLvl != 0 || dii_in.notEmpty);
        InstsAndIDs cb = buff;
        Bit#(32) ins = nop;
        Dii_Id id = ?;
        if (dii_in.notEmpty) {ins, id} = dii_in.first;
        cb.insts[buffLvl] = tagged Valid ins;
        cb.ids[buffLvl] = id;
        if (buffLvl == -1) begin
            instrs.enq(cb);
            cb.insts = replicate(tagged Invalid);
        end
        buff <= cb;
        buffLvl <= buffLvl + 1;
    endrule

    interface Toooba_RVFI_DII_Server rvfi_dii_server;
        interface Get seqReq = toGet(seq_req);
        interface Put inst = toPut(dii_in);
        interface Get trace_report = toGet(reports);
    endinterface

    interface Server dii;
        interface Put request = toPut(seq_req);
        interface Get response;
            method ActionValue#(InstsAndIDs) get;
                InstsAndIDs insts = instrs.first();
                instrs.deq();
                if (verbose)
                    $display("%t DII injection: ", $time,
                        fshow(insts)
                    );
                return insts;
            endmethod
        endinterface
    endinterface

    interface Put rvfi = toPut(report_vectors);
endmodule

endpackage
