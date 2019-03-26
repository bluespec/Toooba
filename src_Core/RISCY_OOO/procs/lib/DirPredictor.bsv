
// Copyright (c) 2017 Massachusetts Institute of Technology
// 
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

`include "ProcConfig.bsv"

import Assert::*;
import Types::*;
import ProcTypes::*;
import Vector::*;
import BrPred::*;
import Bht::*;
import GSelectPred::*;
import GSharePred::*;
import TourPred::*;
import TourPredSecure::*;

export DirPredTrainInfo(..);
export mkDirPredictor;

`ifdef DIR_PRED_BHT
typedef BhtTrainInfo DirPredTrainInfo;
`endif
`ifdef DIR_PRED_GSELECT
typedef GSelectTrainInfo DirPredTrainInfo;
`endif
`ifdef DIR_PRED_GSHARE
typedef GShareTrainInfo DirPredTrainInfo;
`endif
`ifdef DIR_PRED_TOUR
typedef TourTrainInfo DirPredTrainInfo;
`endif

(* synthesize *)
module mkDirPredictor(DirPredictor#(DirPredTrainInfo));
`ifdef DIR_PRED_BHT
`ifdef SECURITY
    staticAssert(False, "BHT with flush methods is not implemented");
`endif
    let m <- mkBht;
`endif

`ifdef DIR_PRED_GSELECT
`ifdef SECURITY
    staticAssert(False, "GSelect with flush methods is not implemented");
`endif
    let m <- mkGSelectPred;
`endif

`ifdef DIR_PRED_GSHARE
`ifdef SECURITY
    staticAssert(False, "GShare with flush methods is not implemented");
`endif
    let m <- mkGSharePred;
`endif

`ifdef DIR_PRED_TOUR
`ifdef SECURITY
    let m <- mkTourPredSecure;
`else
    let m <- mkTourPred;
`endif
`endif

    return m;
endmodule
