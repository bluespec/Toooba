import CCTypes::*;
import Types::*;
import Vector::*;
import CacheUtils::*;
// imported C function to handle monolithic memory
import "BDPI" function ActionValue#(Bit#(64)) c_createMem(Bit#(32) lgSzBytes);
import "BDPI" function ActionValue#(Bit#(64)) c_readMem(Bit#(64) memPtr, Bit#(64) addr);
import "BDPI" function Action c_writeMem(Bit#(64) memPtr, Bit#(64) addr, Bit#(64) d);

interface AtomicMem#(numeric type lgMemSzBytes);
    method ActionValue#(Data) readData(Addr a);
    method Action writeData(Addr a, Data d);
    method ActionValue#(Line) readLine(Addr a);
    method Action writeLine(Addr a, Line d);
endinterface

module mkAtomicMem(AtomicMem#(lgMemSzBytes));
    Reg#(Addr) memPtr <- mkReg(0);
    Reg#(Bool) initDone <- mkReg(False);

    function Action checkAddrOverflow(Addr a);
        return action
            if(a >= fromInteger(valueOf(TExp#(lgMemSzBytes)))) begin
                $fwrite(stderr, "[AtomicMem] ERROR: time %t, addr %x overflow\n", $time, a);
                $finish;
            end
        endaction;
    endfunction

    rule doInit(!initDone);
        let ptr <- c_createMem(fromInteger(valueOf(lgMemSzBytes)));
        memPtr <= ptr;
        initDone <= True;
    endrule

    method ActionValue#(Data) readData(Addr a) if(initDone);
        checkAddrOverflow(a);
        let d <- c_readMem(memPtr, a);
        $display("%t AtomicMem readData Addr %x, Data %x", $time, a, d);

        return d;
    endmethod

    method Action writeData(Addr a, Data d) if(initDone);
        checkAddrOverflow(a);
        $display("%t AtomicMem writeData Addr %x, Data %x", $time, a, d);
        c_writeMem(memPtr, a, d);
    endmethod

    method ActionValue#(Line) readLine(Addr addr) if(initDone);
        checkAddrOverflow(addr);
		Line line = ?;
        LineAddr la = truncateLSB(addr);
        DataBytesOffset off = 0;
		for(Integer i = 0; i < valueOf(LineSzData); i = i+1) begin
			LineDataOffset sel = fromInteger(i);
			line.data[i] <- c_readMem(memPtr, {la, sel, off});
		end
		return line;
    endmethod

    method Action writeLine(Addr addr, Line line) if(initDone);
        checkAddrOverflow(addr);
        LineAddr la = truncateLSB(addr);
        DataBytesOffset off = 0;
		for(Integer i = 0; i < valueOf(LineSzData); i = i+1) begin
			LineDataOffset sel = fromInteger(i);
			c_writeMem(memPtr, {la, sel, off}, line[i]);
		end
    endmethod
endmodule

