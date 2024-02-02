
// Copyright (c) 2017 Massachusetts Institute of Technology
// Copyright (c) 2020 Jonathan Woodruff
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

import Vector::*;
import FShow::*;
import Types::*;
import CCTypes::*;
import CCPipe::*;
import RWBramCore::*;
import RandomReplace::*;

export L1PipeRqIn(..);
export L1PipePRsIn(..);
export L1PipeFlushIn(..);
export L1PipeIn(..);
export L1FlushCmd(..);
export L1Cmd(..);
export L1Pipe(..);
export mkL1Pipe;

// type param ordering: bank < way < index < tag < cRq < pRq

// in L1 cache, only cRq can occupy cache line (pRq handled immediately)
// replacement is always done immediately (never have replacing line)
// so cache owner type is simply Maybe#(cRqIdxT)

// input types
typedef struct {
    Addr addr;
    rqIdxT mshrIdx;
} L1PipeRqIn#(type rqIdxT) deriving(Bits, Eq, FShow);

typedef struct {
    Addr addr;
    Msi toState;
    Maybe#(Line) data;
    wayT way;
} L1PipePRsIn#(type wayT) deriving(Bits, Eq, FShow);

typedef struct {
    indexT index;
    wayT way;
    pRqIdxT mshrIdx;
} L1PipeFlushIn#(
    type wayT,
    type indexT,
    type pRqIdxT
) deriving(Bits, Eq, FShow);

typedef union tagged {
    L1PipeRqIn#(cRqIdxT) CRq;
    L1PipeRqIn#(pRqIdxT) PRq;
    L1PipePRsIn#(wayT) PRs;
`ifdef SECURITY_CACHES
    L1PipeFlushIn#(wayT, indexT, pRqIdxT) Flush;
`endif
} L1PipeIn#(
    type wayT,
    type indexT,
    type cRqIdxT,
    type pRqIdxT
) deriving (Bits, Eq, FShow);

// output cmd to the processing rule in L1$
typedef struct {
    indexT index;
    pRqIdxT mshrIdx;
} L1FlushCmd#(type indexT, type pRqIdxT) deriving(Bits, Eq, FShow);

typedef union tagged {
    cRqIdxT L1CRq;
    pRqIdxT L1PRq;
    void L1PRs;
`ifdef SECURITY_CACHES
    L1FlushCmd#(indexT, pRqIdxT) L1Flush;
`endif
} L1Cmd#(
    type indexT,
    type cRqIdxT,
    type pRqIdxT
) deriving (Bits, Eq, FShow);

interface L1Pipe#(
    numeric type lgBankNum,
    numeric type wayNum,
    type indexT,
    type tagT,
    type cRqIdxT,
    type pRqIdxT
);
    method Action send(L1PipeIn#(Bit#(TLog#(wayNum)), indexT, cRqIdxT, pRqIdxT) r);
    method PipeOut#(
        Bit#(TLog#(wayNum)),
        tagT, Msi, void, // no dir
        Maybe#(cRqIdxT), void, RandRepInfo, // no other
        Line, L1Cmd#(indexT, cRqIdxT, pRqIdxT)
    ) first;
    method Action deqWrite(
        Maybe#(cRqIdxT) swapRq,
        RamData#(tagT, Msi, void, Maybe#(cRqIdxT), void, Line) wrRam, // always write BRAM
        Bool updateRep
    );
endinterface

// real cmd used in pipeline
typedef struct {
    Addr addr;
    wayT way;
} L1PipePRsCmd#(type wayT) deriving(Bits, Eq, FShow);

typedef union tagged {
    L1PipeRqIn#(cRqIdxT) CRq;
    L1PipeRqIn#(pRqIdxT) PRq;
    L1PipePRsCmd#(wayT) PRs;
`ifdef SECURITY_CACHES
    L1PipeFlushIn#(wayT, indexT, pRqIdxT) Flush;
`endif
} L1PipeCmd#(
    type wayT,
    type indexT,
    type cRqIdxT,
    type pRqIdxT
) deriving (Bits, Eq, FShow);

module mkL1Pipe(
    L1Pipe#(lgBankNum, wayNum, indexT, tagT, cRqIdxT, pRqIdxT)
) provisos(
    Alias#(wayT, Bit#(TLog#(wayNum))),
    Alias#(dirT, void), // no directory
    Alias#(ownerT, Maybe#(cRqIdxT)),
    Alias#(otherT, void), // no other cache info
    Alias#(repT, RandRepInfo), // use random replace
    Alias#(pipeInT, L1PipeIn#(wayT, indexT, cRqIdxT, pRqIdxT)),
    Alias#(pipeCmdT, L1PipeCmd#(wayT, indexT, cRqIdxT, pRqIdxT)),
    Alias#(l1CmdT, L1Cmd#(indexT, cRqIdxT, pRqIdxT)),
    Alias#(pipeOutT, PipeOut#(wayT, tagT, Msi, dirT, ownerT, otherT, repT, Line, l1CmdT)), // output type
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
    Alias#(cRqIdxT, Bit#(cRqIdxSz)),
    Alias#(pRqIdxT, Bit#(pRqIdxSz)),
    Add#(indexSz, a__, AddrSz),
    Add#(tagSz, b__, AddrSz)
);

   Bool verbose = False;

    // RAMs
    Vector#(wayNum, RWBramCore#(indexT, infoT)) infoRam <- replicateM(mkRWBramCoreForwarded);
    RWBramCore#(indexT, repT) repRam <- mkRandRepRam;
    Vector#(wayNum, RWBramCore#(indexT, Line)) dataRam <- replicateM(mkRWBramCoreForwarded);

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
            tagged PRq .r: r.addr;
            tagged PRs .r: r.addr;
`ifdef SECURITY_CACHES
            // fake an address for flush req that has the same index
            tagged Flush .r: (zeroExtend(r.index) << (valueOf(LgLineSzBytes) + valueOf(lgBankNum)));
`endif
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
`ifdef SECURITY_CACHES
            else if(cmd matches tagged Flush .flush) begin
                // flush directly read from cmd
                return TagMatchResult {
                    way: flush.way,
                    pRqMiss: False
                };
            end
`endif
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

    function ActionValue#(updateByUpCsT) updateByUpCs(
        pipeCmdT cmd, Msi toState, Bool dataV, Msi oldCs
    );
    actionvalue
        doAssert(toState > oldCs, "should truly upgrade cs");
        doAssert((oldCs == I) == dataV, "valid resp data for upgrade from I");
        return UpdateByUpCs {cs: toState};
    endactionvalue
    endfunction

    function ActionValue#(updateByDownDirT) updateByDownDir(
        pipeCmdT cmd, Msi toState, Bool dataV, Msi oldCs, dirT oldDir
    );
    actionvalue
        doAssert(False, "L1 does not have dir");
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
    ) pipe <- mkCCPipeSingleCycle(
        regToReadOnly(initDone), getIndex, tagMatch,
        updateByUpCs, updateByDownDir, updateRepInfo,
        infoRam, repRam, dataRam
    );

    method Action send(pipeInT req);
        case(req) matches
            tagged CRq .rq: begin
                pipe.enq(CRq (rq), Invalid, Invalid);
            end
            tagged PRq .rq: begin
                pipe.enq(PRq (rq), Invalid, Invalid);
            end
            tagged PRs .rs: begin
                pipe.enq(PRs (L1PipePRsCmd {
                    addr: rs.addr,
                    way: rs.way
                }), rs.data, UpCs (rs.toState));
            end
`ifdef SECURITY_CACHES
            tagged Flush .flush: begin
                pipe.enq(Flush (flush), Invalid, Invalid);
            end
`endif
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
`ifdef SECURITY_CACHES
                tagged Flush .flush: L1Flush (L1FlushCmd {
                    index: flush.index,
                    mshrIdx: flush.mshrIdx
                });
`endif
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
            newCmd = Valid (CRq (L1PipeRqIn {addr: addr, mshrIdx: idx}));
`ifdef SECURITY_CACHES
            doAssert(pipe.first.cmd matches tagged Flush .f ? False : True,
                     "Cannot swap after a flush req");
`endif
        end
        // call pipe
        pipe.deqWrite(newCmd, wrRam, updateRep);
    endmethod
endmodule
