/*-
 * Copyright (c) 2021 Jonathan Woodruff
 * Copyright (c) 2022 Franz Fuchs
 * All rights reserved.
 *
 * This software was developed by the University of  Cambridge
 * Department of Computer Science and Technology under the
 * SIPP (Secure IoT Processor Platform with Remote Attestation)
 * project funded by EPSRC: EP/S030868/1
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

`include "ProcConfig.bsv"
import Map::*;

typedef Bit#(10) StlPredKey;
typedef Bit#(6) StlPredIndex;
typedef Int#(3) StlPredValue;
typedef 2 StlPredAssociativity;

interface STLPred;
    method Action update(Bit#(16) pc_hash, Bool waited, Bool killedLd);
    method Bool pred(Bit#(16) pc_hash);
endinterface

module mkSTLPred(STLPred);
    Map#(StlPredKey,StlPredIndex,StlPredValue,StlPredAssociativity) ldKillMap <- mkMapLossy(minBound);
    Reg#(Bit#(16)) rand_count <- mkReg(0);

    rule inc_rand_count;
        rand_count <= rand_count + 1;
    endrule

    method Action update(Bit#(16) pc_hash, Bool waited, Bool killedLd);
        Bool rand_inv = (rand_count & (512-1)) == 0;
        Int#(3) inc = -1; // Subtract one by default.
        if (waited) inc = 0; // Don't train if we waited for stores.
        else if (killedLd) inc = 2;  // Double train if we flushed the pipe.
        ldKillMap.updateWithFunc(unpack(pc_hash), // Key
            inc,                      // value; don't train if we waited.
            boundedPlus, // function to combine this value with existing
            killedLd || rand_inv      // insert if doesn't exist
        );
    endmethod

    method Bool pred(Bit#(16) pc_hash);
`ifdef NO_SPEC_STL
        return True;
`else
        return fromMaybe(minBound, ldKillMap.lookup(unpack(pc_hash))) == maxBound;
`endif
    endmethod

endmodule
