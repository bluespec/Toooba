
// Copyright (c) 2017 Massachusetts Institute of Technology
//
//-
// RVFI_DII + CHERI modifications:
//     Copyright (c) 2020 Jonathan Woodruff
//     All rights reserved.
//
//     This software was developed by SRI International and the University of
//     Cambridge Computer Laboratory (Department of Computer Science and
//     Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
//     DARPA SSITH research programme.
//
//     This work was supported by NCSC programme grant 4212611/RFA 15971 ("SafeBet").
//-
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
`ifdef SELF_INV_CACHE

import Vector::*;
import FShow::*;
import Types::*;
import CCTypes::*;
import CCPipe::*;
import RWBramCore::*;
import RandomReplace::*;

// type param ordering: bank < child < way < index < tag < cRq

// input types
typedef struct {
    Addr addr;
    cRqIdxT mshrIdx;
} SelfInvLLPipeCRqIn#(type cRqIdxT) deriving(Bits, Eq, FShow);

typedef struct {
    Addr addr;
    Msi toState; // come from req in MSHR (E or M)
    Line data; // come from memory must be valid
    wayT way; // come from MSHR
} SelfInvLLPipeMRsIn#(type wayT) deriving(Bits, Eq, FShow);

typedef union tagged {
    SelfInvLLPipeCRqIn#(cRqIdxT) CRq;
    CRsMsg#(childT) CRs;
    SelfInvLLPipeMRsIn#(wayT) MRs;
} SelfInvLLPipeIn#(
    type childT,
    type wayT,
    type cRqIdxT
) deriving (Bits, Eq, FShow);

// output cmd to the processing rule in LLC
typedef union tagged {
    cRqIdxT LLCRq; // mshr idx of the cRq
    childT LLCRs; // which child is downgrading
    void LLMRs;
} SelfInvLLCmd#(type childT, type cRqIdxT) deriving (Bits, Eq, FShow);

// Track only exclusive child in directory
typedef struct {
    childT exChild;
    Msi state; // I/E/M, we don't track S
} SelfInvDir#(type childT) deriving(Bits, Eq, FShow);

interface SelfInvLLPipe#(
    numeric type lgBankNum,
    numeric type childNum,
    numeric type wayNum,
    type indexT,
    type tagT,
    type cRqIdxT
);
    method Action send(SelfInvLLPipeIn#(Bit#(TLog#(childNum)), Bit#(TLog#(wayNum)), cRqIdxT) r);
    method Bool notEmpty;
    method PipeOut#(
        Bit#(TLog#(wayNum)),
        tagT, Msi, SelfInvDir#(Bit#(TLog#(childNum))),
        Maybe#(CRqOwner#(cRqIdxT)), void, RandRepInfo, // no other
        Line, SelfInvLLCmd#(Bit#(TLog#(childNum)), cRqIdxT)
    ) first;
    method PipeOut#(
        Bit#(TLog#(wayNum)),
        tagT, Msi, SelfInvDir#(Bit#(TLog#(childNum))),
        Maybe#(CRqOwner#(cRqIdxT)), void, RandRepInfo, // no other
        Line, SelfInvLLCmd#(Bit#(TLog#(childNum)), cRqIdxT)
    ) unguard_first;
    method Action deqWrite(
        Maybe#(cRqIdxT) swapRq,
        RamData#(tagT, Msi, SelfInvDir#(Bit#(TLog#(childNum))), Maybe#(CRqOwner#(cRqIdxT)), void, Line) wrRam, // always write BRAM
        Bool updateRep
    );
endinterface

// real cmd used in pipeline
typedef struct {
    Addr addr;
    childT child;
} SelfInvLLPipeCRsCmd#(type childT) deriving(Bits, Eq, FShow);

typedef struct {
    Addr addr;
    wayT way;
} SelfInvLLPipeMRsCmd#(type wayT) deriving(Bits, Eq, FShow);

typedef union tagged {
    SelfInvLLPipeCRqIn#(cRqIdxT) CRq;
    SelfInvLLPipeCRsCmd#(childT) CRs;
    SelfInvLLPipeMRsCmd#(wayT) MRs;
} SelfInvLLPipeCmd#(
    type childT,
    type wayT,
    type cRqIdxT
) deriving (Bits, Eq, FShow);

module mkSelfInvLLPipe(
    SelfInvLLPipe#(lgBankNum, childNum, wayNum, indexT, tagT, cRqIdxT)
) provisos(
    Alias#(childT, Bit#(TLog#(childNum))),
    Alias#(wayT, Bit#(TLog#(wayNum))),
    Alias#(dirT, SelfInvDir#(childT)),
    Alias#(ownerT, Maybe#(CRqOwner#(cRqIdxT))),
    Alias#(otherT, void), // no other cache info
    Alias#(repT, RandRepInfo), // use random replace
    Alias#(pipeInT, SelfInvLLPipeIn#(childT, wayT, cRqIdxT)),
    Alias#(pipeCmdT, SelfInvLLPipeCmd#(childT, wayT, cRqIdxT)),
    Alias#(llCmdT, SelfInvLLCmd#(childT, cRqIdxT)),
    Alias#(pipeOutT, PipeOut#(wayT, tagT, Msi, dirT, ownerT, otherT, repT, Line, llCmdT)), // output type
    Alias#(infoT, CacheInfo#(tagT, Msi, dirT, ownerT, otherT)),
    Alias#(ramDataT, RamData#(tagT, Msi, dirT, ownerT, otherT, Line)),
    Alias#(respStateT, RespState#(Msi)),
    Alias#(tagMatchResT, TagMatchResult#(wayT)),
    Alias#(updateByUpCsT, UpdateByUpCs#(Msi)),
    Alias#(updateByDownDirT, UpdateByDownDir#(Msi, dirT)),
    Alias#(dataIndexT, Bit#(TAdd#(TLog#(wayNum), indexSz))),
    // requirement 
    Alias#(indexT, Bit#(indexSz)),
    Alias#(tagT, Bit#(tagSz)),
    Alias#(cRqIdxT, Bit#(_cRqIdxSz)),
    Add#(indexSz, a__, AddrSz),
    Add#(tagSz, b__, AddrSz)
);

   Bool verbose = False;

    // RAMs
    Vector#(wayNum, RWBramCore#(indexT, infoT)) infoRam <- replicateM(mkRWBramCore);
    RWBramCore#(indexT, repT) repRam <- mkRandRepRam;
    RWBramCore#(dataIndexT, Line) dataRam <- mkRWBramCore;
    
    // initialize RAM
    Reg#(Bool) initDone <- mkReg(False);
    Reg#(indexT) initIndex <- mkReg(0);

    rule doInit(!initDone);
        for(Integer i = 0; i < valueOf(wayNum); i = i+1) begin
            infoRam[i].wrReq(initIndex, CacheInfo {
                tag: 0,
                cs: I,
                dir: SelfInvDir {exChild: ?, state: I},
                owner: Invalid,
                other: ?
            });
        end
        repRam.wrReq(initIndex, randRepInitInfo); // useless for random replace
        initIndex <= initIndex + 1;
        if(initIndex == maxBound) begin
            initDone <= True;
        end
    endrule

    // random replacement
    RandomReplace#(wayNum) randRep <- mkRandomReplace;

    // functions
    function Addr getAddrFromCmd(pipeCmdT cmd);
        return (case(cmd) matches
            tagged CRq .r: r.addr;
            tagged CRs .r: r.addr;
            tagged MRs .r: r.addr;
            default: ?;
        endcase);
    endfunction

    function indexT getIndex(pipeCmdT cmd);
        Addr a = getAddrFromCmd(cmd);
        return truncate(a >> (valueOf(LgLineSzBytes) + valueOf(lgBankNum)));
    endfunction

    function ActionValue#(tagMatchResT) tagMatch(
        pipeCmdT cmd,
        Vector#(wayNum, tagT) tagVec, 
        Vector#(wayNum, Msi) csVec, 
        Vector#(wayNum, ownerT) ownerVec,
        repT repInfo
    );
        return actionvalue
            function tagT getTag(Addr a) = truncateLSB(a);

            if (verbose)
            $display("%t LL %m tagMatch: ", $time, 
                fshow(cmd), " ; ", 
                fshow(getTag(getAddrFromCmd(cmd))), " ; ",
                fshow(tagVec), " ; ", 
                fshow(csVec), " ; ", 
                fshow(ownerVec)
            );
            if(cmd matches tagged MRs .rs) begin
                // MRs directly read from cmd
                return TagMatchResult {
                    way: rs.way,
                    pRqMiss: False
                };
            end
            else begin
                // CRq/CRs: need tag matching
                Addr addr = getAddrFromCmd(cmd);
                tagT tag = getTag(addr);
                // find hit way (we do not check replacing bit in LLC)
                // this makes <cRq a> blocked by other <cRq b> which is replacing addr a
                function Bool isMatch(Tuple2#(Msi, tagT) csTag);
                    match {.cs, .t} = csTag;
                    return cs > I && t == tag;
                endfunction
                Maybe#(wayT) hitWay = searchIndex(isMatch, zip(csVec, tagVec));
                if(hitWay matches tagged Valid .w) begin
                    return TagMatchResult {
                        way: w,
                        pRqMiss: False
                    };
                end
                else begin
                    // cRs must hit, so only cRq cannot enter here
                    doAssert(cmd matches tagged CRq ._rq ? True : False,
                        "only cRq can tag match miss"
                    );
                    // find a unlocked way to replace for cRq
                    Vector#(wayNum, Bool) unlocked = ?;
                    Vector#(wayNum, Bool) invalid = ?;
                    for(Integer i = 0; i < valueOf(wayNum); i = i+1) begin
                        invalid[i] = csVec[i] == I;
                        unlocked[i] = !isValid(ownerVec[i]);
                    end
                    Maybe#(wayT) repWay = randRep.getReplaceWay(unlocked, invalid);
                    // sanity check: repWay must be valid
                    doAssert(isValid(repWay), "should always find a way to replace");
                    return TagMatchResult {
                        way: fromMaybe(?, repWay),
                        pRqMiss: False
                    };
                end
            end
        endactionvalue;
    endfunction

    function ActionValue#(updateByUpCsT) updateByUpCs(
        pipeCmdT cmd, Msi toState, Bool dataV, Msi oldCs
    );
    actionvalue
        doAssert(toState > oldCs, "should truly upgrade cs");
        doAssert((oldCs == I) && dataV, "LLC mRs always has data");
        return UpdateByUpCs {cs: toState};
    endactionvalue
    endfunction

    function ActionValue#(updateByDownDirT) updateByDownDir(
        pipeCmdT cmd, Msi toState, Bool dataV, Msi oldCs, dirT oldDir
    );
    actionvalue
        // update dir
        // The exclusive child is downgraded. Since we don't track S, just make
        // dir invalid, child field is useless for I
        dirT newDir = SelfInvDir {exChild: ?, state: I};
        doAssert(oldDir.state >= E && toState <= S, "no more exclusive child");
        doAssert(cmd matches tagged CRs .cRs &&& oldDir.exChild == cRs.child ? True : False,
                 "cRs child should match");
        if(!dataV) begin
            doAssert(oldDir.state < M, "cRs without data, dir must < M");
        end
        if(oldDir.state == M) begin
            doAssert(dataV, "downgrade from M must have data");
            doAssert(oldCs == M, "cs must be M");
        end
        // update cs
        // XXX since child can upgrade from E to M silently, use data valid
        // to determine if we need to upgrade to M. Note that the data
        // valid field has not been overwritten by bypass in CCPipe.
        Msi newCs = oldCs;
        if(dataV) begin
            doAssert(oldCs >= E, "cRs has data, cs must >= E");
            newCs = M;
        end
        return UpdateByDownDir {cs: newCs, dir: newDir};
    endactionvalue
    endfunction

    function ActionValue#(repT) updateRepInfo(repT r, wayT w);
    actionvalue
        return ?; // random replace does not have bookkeeping
    endactionvalue
    endfunction

    CCPipe#(wayNum, indexT, tagT, Msi, dirT, ownerT, otherT, repT, Line, pipeCmdT) pipe <- mkCCPipe(
        regToReadOnly(initDone), getIndex, tagMatch,
        updateByUpCs, updateByDownDir, updateRepInfo,
        infoRam, repRam, dataRam
    );

    // get first output from CCPipe output
    function pipeOutT getFirst(PipeOut#(wayT, tagT, Msi, dirT, ownerT, otherT, repT, Line, pipeCmdT) pout);
        return PipeOut {
            cmd: (case(pout.cmd) matches
                tagged CRq .rq: LLCRq (rq.mshrIdx);
                tagged CRs .rs: LLCRs (rs.child);
                tagged MRs .rs: LLMRs;
                default: ?;
            endcase),
            way: pout.way,
            pRqMiss: pout.pRqMiss,
            ram: pout.ram,
            repInfo: pout.repInfo
        };
    endfunction

    method Action send(pipeInT req);
        case(req) matches
            tagged CRq .rq: begin
                pipe.enq(CRq (rq), Invalid, Invalid);
            end
            tagged CRs .rs: begin
                pipe.enq(CRs (SelfInvLLPipeCRsCmd {
                    addr: rs.addr,
                    child: rs.child
                }), rs.data, DownDir (rs.toState));
            end
            tagged MRs .rs: begin
                pipe.enq(MRs (SelfInvLLPipeMRsCmd {
                    addr: rs.addr,
                    way: rs.way
                }), Valid (rs.data), UpCs (rs.toState));
            end
        endcase
    endmethod

    // need to adapt pipeline output to real output format
    method pipeOutT first;
        return getFirst(pipe.first); // guarded version
    endmethod

    method pipeOutT unguard_first;
        return getFirst(pipe.unguard_first); // unguarded version
    endmethod

    method notEmpty = pipe.notEmpty;

    method Action deqWrite(Maybe#(cRqIdxT) swapRq, ramDataT wrRam, Bool updateRep);
        // get new cmd
        Addr addr = getAddrFromCmd(pipe.first.cmd); // inherit addr
        Maybe#(pipeCmdT) newCmd = Invalid;
        if(swapRq matches tagged Valid .idx) begin
            newCmd = Valid (CRq (SelfInvLLPipeCRqIn {addr: addr, mshrIdx: idx}));
        end
        // call pipe
        pipe.deqWrite(newCmd, wrRam, updateRep);
    endmethod
endmodule
`endif // SELF_INV_CACHE
