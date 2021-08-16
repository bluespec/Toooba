
// Copyright (c) 2019 Massachusetts Institute of Technology
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

import Assert::*;
import ConfigReg::*;
import Vector::*;
import FShow::*;
import Fifos::*;
import Types::*;
import CCTypes::*;
import CCPipe::*;
import RWBramCore::*;
import RandomReplace::*;

export SelfInvL1PipeRqIn(..);
export SelfInvL1PipePRsIn(..);
export SelfInvL1PipeIn(..);
export SelfInvL1Cmd(..);
export SelfInvL1Hits(..);
export SelfInvL1Pipe(..);
export mkSelfInvL1Pipe;

// type param ordering: bank < way < index < tag < cRq < pRq

// in L1 cache, only cRq can occupy cache line (pRq handled immediately)
// replacement is always done immediately (never have replacing line)
// so cache owner type is simply Maybe#(cRqIdxT)

// input types
typedef struct {
    Addr addr;
    rqIdxT mshrIdx;
} SelfInvL1PipeRqIn#(type rqIdxT) deriving(Bits, Eq, FShow);

typedef struct {
    Addr addr;
    Msi toState;
    Maybe#(Line) data;
    wayT way;
} SelfInvL1PipePRsIn#(type wayT) deriving(Bits, Eq, FShow);

typedef union tagged {
    SelfInvL1PipeRqIn#(cRqIdxT) CRq;
    SelfInvL1PipeRqIn#(pRqIdxT) PRq;
    SelfInvL1PipePRsIn#(wayT) PRs;
} SelfInvL1PipeIn#(
    type wayT,
    type cRqIdxT, 
    type pRqIdxT
) deriving (Bits, Eq, FShow);

// output cmd to the processing rule in L1$
typedef union tagged {
    cRqIdxT L1CRq;
    pRqIdxT L1PRq;
    void L1PRs;
} SelfInvL1Cmd#(
    type cRqIdxT, 
    type pRqIdxT
) deriving (Bits, Eq, FShow);

// The "other" field in CacheInfo tracks the number of hits on the cache line
// before self inv
typedef struct {
    Bit#(TLog#(maxHitNum)) hits;
} SelfInvL1Hits#(numeric type maxHitNum) deriving(Bits, Eq, FShow);

interface SelfInvL1Pipe#(
    numeric type lgBankNum,
    numeric type wayNum,
    numeric type maxHitNum,
    type indexT,
    type tagT,
    type cRqIdxT,
    type pRqIdxT
);
    method Action send(SelfInvL1PipeIn#(Bit#(TLog#(wayNum)), cRqIdxT, pRqIdxT) r);
    method PipeOut#(
        Bit#(TLog#(wayNum)),
        tagT, Msi, void, // no dir
        Maybe#(cRqIdxT), SelfInvL1Hits#(maxHitNum), RandRepInfo,
        Line, SelfInvL1Cmd#(cRqIdxT, pRqIdxT)
    ) first;
    method Action deqWrite(
        Maybe#(cRqIdxT) swapRq,
        RamData#(tagT, Msi, void, Maybe#(cRqIdxT), SelfInvL1Hits#(maxHitNum), Line) wrRam, // always write BRAM
        Bool updateRep
    );
    // drop stale clean cache lines
    method Action reconcile;
    method Bool reconcile_done;
endinterface

// real cmd used in pipeline
typedef struct {
    Addr addr;
    wayT way;
} SelfInvL1PipePRsCmd#(type wayT) deriving(Bits, Eq, FShow);

typedef union tagged {
    SelfInvL1PipeRqIn#(cRqIdxT) CRq;
    SelfInvL1PipeRqIn#(pRqIdxT) PRq;
    SelfInvL1PipePRsCmd#(wayT) PRs;
} SelfInvL1PipeCmd#(
    type wayT,
    type cRqIdxT, 
    type pRqIdxT
) deriving (Bits, Eq, FShow);

// cache state array with reconcile port
// ram sub interface has same scheduling as RWBramCore:
// - wrReq CF rdReq (read cannot see write)
// - deqRdResp < rdReq
interface CacheStateArray#(type indexT);
    interface RWBramCore#(indexT, Msi) ram;
    method Action reconcile;
endinterface

module mkCacheStateArray(CacheStateArray#(indexT)) provisos(
    Alias#(indexT, Bit#(indexSz)),
    NumAlias#(size, TExp#(indexSz))
);
    Vector#(size, Reg#(Msi)) state <- replicateM(mkConfigReg(I));
    Fifo#(1, Msi) rdRespQ <- mkPipelineFifo;

    interface RWBramCore ram;
        method Action wrReq(indexT idx, Msi s);
            state[idx] <= s;
        endmethod
        method Action rdReq(indexT idx);
            rdRespQ.enq(state[idx]);
        endmethod
        method rdResp = rdRespQ.first;
        method rdRespValid = rdRespQ.notEmpty;
        method deqRdResp = rdRespQ.deq;
    endinterface

    method Action reconcile;
        function Action resetS(Reg#(Msi) s);
        action
            if(s == S) begin
                s <= I;
            end
        endaction
        endfunction
        joinActions(map(resetS, state));
    endmethod
endmodule

// Put cache state array and tag+owner ram together to form an array
typedef struct {
    tagT tag;
    ownerT owner;
    otherT other;
} TagOwnerOther#(type tagT, type ownerT, type otherT) deriving(Bits, Eq, FShow);

interface CacheInfoArray#(type indexT, type tagT, type ownerT, type otherT);
    interface RWBramCore#(indexT, CacheInfo#(tagT, Msi, void, ownerT, otherT)) ram;
    method Action reconcile;
endinterface

module mkCacheInfoArray(CacheInfoArray#(indexT, tagT, ownerT, otherT)) provisos(
    Alias#(indexT, Bit#(indexSz)),
    NumAlias#(size, TExp#(indexSz)),
    Alias#(tagOwnerOtherT, TagOwnerOther#(tagT, ownerT, otherT)),
    Alias#(infoT, CacheInfo#(tagT, Msi, void, ownerT, otherT)),
    Bits#(tagT, _tagSz),
    Bits#(ownerT, _ownerSz),
    Bits#(otherT, _otherSz)
);
    RWBramCore#(indexT, tagOwnerOtherT) tagOwnerOtherRam <- mkRWBramCore;
    CacheStateArray#(indexT) csArray <- mkCacheStateArray;

    interface RWBramCore ram;
        method Action wrReq(indexT idx, infoT x);
            tagOwnerOtherRam.wrReq(idx, TagOwnerOther {
                tag: x.tag,
                owner: x.owner,
                other: x.other
            });
            csArray.ram.wrReq(idx, x.cs);
        endmethod
        method Action rdReq(indexT idx);
            tagOwnerOtherRam.rdReq(idx);
            csArray.ram.rdReq(idx);
        endmethod
        method infoT rdResp;
            tagOwnerOtherT tagOwnerOther = tagOwnerOtherRam.rdResp;
            Msi cs = csArray.ram.rdResp;
            return CacheInfo {
                tag: tagOwnerOther.tag,
                cs: cs,
                dir: ?,
                owner: tagOwnerOther.owner,
                other: tagOwnerOther.other
            };
        endmethod
        method Bool rdRespValid;
            return tagOwnerOtherRam.rdRespValid && csArray.ram.rdRespValid;
        endmethod
        method Action deqRdResp;
            tagOwnerOtherRam.deqRdResp;
            csArray.ram.deqRdResp;
        endmethod
    endinterface

    method reconcile = csArray.reconcile;
endmodule

module mkSelfInvL1Pipe(
    SelfInvL1Pipe#(lgBankNum, wayNum, maxHitNum, indexT, tagT, cRqIdxT, pRqIdxT)
) provisos(
    Alias#(wayT, Bit#(TLog#(wayNum))),
    Alias#(dirT, void), // no directory
    Alias#(ownerT, Maybe#(cRqIdxT)),
    Alias#(otherT, SelfInvL1Hits#(maxHitNum)),
    Alias#(repT, RandRepInfo),
    Alias#(pipeInT, SelfInvL1PipeIn#(wayT, cRqIdxT, pRqIdxT)),
    Alias#(pipeCmdT, SelfInvL1PipeCmd#(wayT, cRqIdxT, pRqIdxT)),
    Alias#(l1CmdT, SelfInvL1Cmd#(cRqIdxT, pRqIdxT)),
    Alias#(pipeOutT, PipeOut#(wayT, tagT, Msi, dirT, ownerT, otherT, repT, Line, l1CmdT)), // output type
    Alias#(infoT, CacheInfo#(tagT, Msi, dirT, ownerT, otherT)),
    Alias#(ramDataT, RamData#(tagT, Msi, dirT, ownerT, otherT, Line)),
    Alias#(respStateT, RespState#(Msi)),
    Alias#(tagMatchResT, TagMatchResult#(wayT)),
    Alias#(dataIndexT, Bit#(TAdd#(TLog#(wayNum), indexSz))),
    // requirement
    Alias#(indexT, Bit#(indexSz)),
    Alias#(tagT, Bit#(tagSz)),
    Alias#(cRqIdxT, Bit#(cRqIdxSz)),
    Alias#(pRqIdxT, Bit#(pRqIdxSz)),
    Add#(indexSz, a__, AddrSz),
    Add#(tagSz, b__, AddrSz)
);

   Bool verbose = False;

    // info RAM
    Vector#(wayNum, CacheInfoArray#(indexT, tagT, ownerT, otherT)) infoArray <- replicateM(mkCacheInfoArray);
    function RWBramCore#(indexT, infoT) getInfoRam(Integer i) = infoArray[i].ram;
    Vector#(wayNum, RWBramCore#(indexT, infoT)) infoRam = map(getInfoRam, genVector);
    // rep RAM (dummy)
    RWBramCore#(indexT, repT) repRam <- mkRandRepRam;
    // data RAM
    RWBramCore#(dataIndexT, Line) dataRam <- mkRWBramCore;

    // initialize RAM
    Reg#(Bool) initDone <- mkReg(False);
    Reg#(indexT) initIndex <- mkReg(0);

    rule doInit(!initDone);
        for(Integer i = 0; i < valueOf(wayNum); i = i+1) begin
            infoRam[i].wrReq(initIndex, CacheInfo {
                tag: 0,
                cs: I,
                dir: ?,
                owner: Invalid,
                other: SelfInvL1Hits {hits: 0}
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
            tagged PRq .r: r.addr;
            tagged PRs .r: r.addr;
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
            $display("%t L1 %m tagMatch: ", $time, 
                fshow(cmd), " ; ", 
                fshow(getTag(getAddrFromCmd(cmd))),
                fshow(tagVec), " ; ", 
                fshow(csVec), " ; ", 
                fshow(ownerVec), " ; " 
            );
            if(cmd matches tagged PRs .rs) begin
                // PRs directly read from cmd
                return TagMatchResult {
                    way: rs.way,
                    pRqMiss: False
                };
            end
            else begin
                // CRq/PRq: need tag matching
                Addr addr = getAddrFromCmd(cmd);
                tagT tag = getTag(addr);
                // find hit way (nothing is being replaced)
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
                else if(cmd matches tagged PRq .rq) begin
                    // pRq miss
                    return TagMatchResult {
                        way: 0, // default to 0
                        pRqMiss: True
                    };
                end
                else begin
                    // find a unlocked way to replace for cRq
                    Vector#(wayNum, Bool) unlocked = ?;
                    Vector#(wayNum, Bool) invalid = ?;
                    for(Integer i = 0; i < valueOf(wayNum); i = i+1) begin
                        invalid[i] = csVec[i] == I;
                        unlocked[i] = !isValid(ownerVec[i]);
                    end
                    Maybe#(wayT) repWay = randRep.getReplaceWay(unlocked, invalid);
                    // sanity check: repWay must be valid
                    if(!isValid(repWay)) begin
                        $fwrite(stderr, "[L1Pipe] ERROR: ", fshow(cmd), " cannot find way to replace\n");
                        $finish;
                    end
                    return TagMatchResult {
                        way: fromMaybe(?, repWay),
                        pRqMiss: False
                    };
                end
            end
        endactionvalue;
    endfunction

    function ActionValue#(UpdateByUpCs#(Msi)) updateByUpCs(
        pipeCmdT cmd, Msi toState, Bool dataV, Msi oldCs
    );
    actionvalue
        doAssert(toState > oldCs, "should truly upgrade cs");
        doAssert(dataV, "self inv L1 always needs data resp");
        return UpdateByUpCs {cs: toState};
    endactionvalue
    endfunction

    function ActionValue#(UpdateByDownDir#(Msi, dirT)) updateByDownDir(
        pipeCmdT cmd, Msi toState, Bool dataV, Msi oldCs, dirT oldDir
    );
    actionvalue
        doAssert(False, "L1 should not have cRs");
        return UpdateByDownDir {cs: oldCs, dir: oldDir};
    endactionvalue
    endfunction

    function ActionValue#(repT) updateRepInfo(repT r, wayT w);
    actionvalue
        return ?; // random replace does not have bookkeeping
    endactionvalue
    endfunction

    CCPipe#(
        wayNum, indexT, tagT, Msi, dirT, ownerT, otherT, repT, Line, pipeCmdT
    ) pipe <- mkCCPipe(
        regToReadOnly(initDone), getIndex, tagMatch,
        updateByUpCs, updateByDownDir, updateRepInfo,
        infoRam, repRam, dataRam
    );

    // reconcile: wait until pipeline empty and drop all S states. Stall
    // pipeline enq while we are waiting. Make the reconcile rule conflict with
    // pipeline deq to remove any possible race (the guard of reconcile rule
    // actually should have done the job). 
    // Since send method will not fire when needReconcile, we can use a wire to
    // catch pipeline empty signal to avoid scheduling issue
    Reg#(Bool) needReconcile <- mkReg(False);

    RWire#(void) conflict_reconcile_deq <- mkRWire;

    PulseWire pipeEmpty <- mkPulseWire;

    (* fire_when_enabled, no_implicit_conditions *)
    rule setPipeEmpty(pipe.emptyForFlush);
        pipeEmpty.send;
    endrule
    
    rule doReconcile(initDone && needReconcile && pipeEmpty);
        function Action flush(CacheInfoArray#(indexT, tagT, ownerT, otherT) ifc);
        action
            ifc.reconcile;
        endaction
        endfunction
        joinActions(map(flush, infoArray));
        // reconcile is done
        needReconcile <= False;
        // conflict with deq
        conflict_reconcile_deq.wset(?);
       if (verbose)
        $display("%t L1 %m doReconcile", $time);
    endrule

    // stall enq for reconcile
    method Action send(pipeInT req) if(!needReconcile);
        case(req) matches
            tagged CRq .rq: begin
                pipe.enq(CRq (rq), Invalid, Invalid);
            end
            tagged PRq .rq: begin
                pipe.enq(PRq (rq), Invalid, Invalid);
            end
            tagged PRs .rs: begin
                pipe.enq(PRs (SelfInvL1PipePRsCmd {
                    addr: rs.addr,
                    way: rs.way
                }), rs.data, UpCs (rs.toState));
            end
        endcase
    endmethod

    // need to adapt pipeline output to real output format
    method pipeOutT first;
        let pout = pipe.first;
        return PipeOut {
            cmd: (case(pout.cmd) matches
                tagged CRq .rq: L1CRq (rq.mshrIdx);
                tagged PRq .rq: L1PRq (rq.mshrIdx);
                tagged PRs .rs: L1PRs;
                default: ?;
            endcase),
            way: pout.way,
            pRqMiss: pout.pRqMiss,
            ram: pout.ram,
            repInfo: pout.repInfo
        };
    endmethod

    method Action deqWrite(Maybe#(cRqIdxT) swapRq, ramDataT wrRam, Bool updateRep);
        // get new cmd
        Maybe#(pipeCmdT) newCmd = Invalid;
        if(swapRq matches tagged Valid .idx) begin // swap in cRq
            Addr addr = getAddrFromCmd(pipe.first.cmd); // inherit addr
            newCmd = Valid (CRq (SelfInvL1PipeRqIn {addr: addr, mshrIdx: idx}));
        end
        // call pipe
        pipe.deqWrite(newCmd, wrRam, updateRep);
        // conflict with reconcile
        conflict_reconcile_deq.wset(?);
    endmethod

    method Action reconcile if(!needReconcile);
        needReconcile <= True;
    endmethod

    method Bool reconcile_done;
        return !needReconcile;
    endmethod
endmodule
`endif //  SELF_INV_CACHE
