
// Copyright (c) 2014 Quanta Research Cambridge, Inc.

// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

import Vector::*;

function Bool booland(Bool x1, Bool x2); return x1 && x2; endfunction
function Bool boolor(Bool x1, Bool x2); return x1 || x2; endfunction

function Bool eq(a x1, a x2) provisos (Eq#(a)); return x1 == x2; endfunction

function a add(a x1, a x2) provisos (Arith#(a)); return x1 + x2; endfunction
function a mul(a x1, a x2) provisos (Arith#(a)); return x1 * x2; endfunction
function Bit#(b) rshift(Bit#(b) x1, Integer i); return x1 >> i; endfunction
function Vector#(n, a) vadd(Vector#(n, a) x1, Vector#(n, a) x2) provisos (Arith#(a));
   return map(uncurry(add), zip(x1, x2));
endfunction
function Vector#(n, a) vmul(Vector#(n, a) x1, Vector#(n, a) x2) provisos (Arith#(a));
   return map(uncurry(mul), zip(x1, x2));
endfunction
function Vector#(n, Bit#(b)) vrshift(Vector#(n, Bit#(b)) x1, Integer i);
   return map(flip(rshift)(i), x1);
endfunction
