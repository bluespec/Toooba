import CCTypes::*;
import Types::*;

// interface to testbench
interface DelayMemTest;
    method Action initLine(Addr a, Line d); // for init
    method Action initDone;
    method Line getLine(Addr a); // for checking
endinterface
