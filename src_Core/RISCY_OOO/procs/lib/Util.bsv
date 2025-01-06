function Bit#(n) boundedUpdate(Bit#(n) counter, Bool increment);
    if(increment) begin
        return counter == maxBound ? maxBound : counter + 1;
    end
    else begin
        return counter == 0 ? 0 : counter - 1;
    end
endfunction