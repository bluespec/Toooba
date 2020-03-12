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

`include "ProcConfig.bsv"

import Types::*;
import ProcTypes::*; 

//import Verifier  :: *;
import RVFI_DII  :: *;

// ================================================================

interface Toooba_RVFI_DII_Bridge_IFC;
    interface Toooba_RVFI_DII_Server rvfi_dii_server;
    interface Server#(Dii_Ids, InstsAndIDs) dii;
    interface Put#(Rvfi_Traces) rvfi;
    method Dii_Id lastId;
endinterface

module mkTooobaRVFIDIIBridge(Toooba_RVFI_DII_Bridge_IFC);
    // DII state
    FIFOF#(InstsAndIDs) instrs <- mkSizedFIFOF(2048);
    // RVFI state
    FIFO#(Rvfi_Traces) report_vectors <- mkSizedFIFO(2048);
    // Request ID
    FIFO#(Dii_Ids) seq_req <- mkFIFO;
    Reg#(Dii_Id) last_id <- mkReg(0);
    
    Bool verbose = False;
    
    function Bool validReport(RVFI_DII_Execution#(DataSz,DataSz) trace);
        return (trace.rvfi_insn != dii_nop);
    endfunction
    
    // These two functions convert beteween "Invalid" instructions and "nops".
    // This is because the pipeline currently isn't able to handle Invalid injections,
    // so we replace them with special nops in the bridge that we can filter out in the rvfi trace stream.
    function Maybe#(Bit#(32)) maybeToNop(Maybe#(Bit#(32)) in);
        return tagged Valid fromMaybe(dii_nop, in);
    endfunction
    
    function Maybe#(RVFI_DII_Execution #(DataSz,DataSz)) nopToMaybe(Maybe#(RVFI_DII_Execution #(DataSz,DataSz)) in);
        Maybe#(RVFI_DII_Execution #(DataSz,DataSz)) ret = in;
        if (ret matches tagged Valid .val &&& !validReport(val)) ret = tagged Invalid;
        return (ret);
    endfunction

    interface Toooba_RVFI_DII_Server rvfi_dii_server;
        interface Get seqReq = toGet(seq_req);
        interface Put inst = toPut(instrs);
        interface Get trace_report = toGet(report_vectors);
    endinterface

    interface Server dii;
        interface Put request = toPut(seq_req);
        interface Get response;
            method ActionValue#(InstsAndIDs) get;
                InstsAndIDs insts = instrs.first();
                insts.insts = map(maybeToNop, insts.insts);
                instrs.deq();
                if (verbose)
                    $display("%t DII injection: ", $time,
                        fshow(insts)
                    );
                return insts;
            endmethod
        endinterface
    endinterface

    interface Put rvfi;
        method Action put(Rvfi_Traces in);
            Rvfi_Traces out = map(nopToMaybe,in);
            report_vectors.enq(out);
            Dii_Id next_id = last_id;
            for (Integer i = 0; i < `sizeSup; i = i + 1) begin
                if (out[i] matches tagged Valid .rpt) begin
                  Dii_Id this_id = unpack(truncate(rpt.rvfi_order));
                  if (this_id > next_id) next_id = this_id;
                end
            end
            last_id <= next_id;
        endmethod
    endinterface
    method Dii_Id lastId = last_id;
endmodule

endpackage
