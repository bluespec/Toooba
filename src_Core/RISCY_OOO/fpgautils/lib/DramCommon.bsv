
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

import Assert::*;

// User interface:
// all DRAM use 64B data block
// present user with 64-bit address space, but addr should be in terms of 64B
typedef 512 DramUserDataSz;
typedef TDiv#(DramUserDataSz, 8) DramUserBESz;
typedef 58 DramUserAddrSz;

typedef Bit#(DramUserDataSz) DramUserData;
typedef Bit#(DramUserBESz) DramUserBE;
typedef Bit#(DramUserAddrSz) DramUserAddr;

typedef struct { // read/write req
    DramUserAddr addr;
    DramUserData data;
    DramUserBE wrBE; // all 0 means read,
    // otherwise wrBE[i]=1 means to write byte i
} DramUserReq deriving(Bits, Eq, FShow);

interface DramUser#(
    // maximum number of in-flight requests (not always used)
    numeric type maxReadNum,
    numeric type maxWriteNum,
    // simulation delay (fully pipelined)
    numeric type simDelay,
    // error of dram controller
    // XXX we should not check address overflow because of wrong-path loads
    type errT
);
    method Action req(DramUserReq r);
    method ActionValue#(DramUserData) rdResp; // only read has resp
    method ActionValue#(errT) err;
endinterface

// Full interface
interface DramFull#(
    numeric type maxReadNum,
    numeric type maxWriteNum,
    numeric type simDelay,
    type errT,
    // pin type
    type pinT
);
    interface DramUser#(maxReadNum, maxWriteNum, simDelay, errT) user;
    interface pinT pins;
endinterface

`ifdef BSIM
function Action doAssert(Bool b, String s) = action if(!b) $fdisplay(stderr, "\n%m: ASSERT FAIL!!"); dynamicAssert(b, s); endaction;
`else
function Action doAssert(Bool b, String s) = noAction;
`endif
