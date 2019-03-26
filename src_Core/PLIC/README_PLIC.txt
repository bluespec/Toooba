The (relative) Memory map for this PLIC follows the one given in:

    SiFive U54-MC Core Complex Manual, v1p0, Oct 4 2017
    Chapter 8 Platform Level Interrupt Controller, pp.32-38.

>----------------
Priority registers

0x0000    Reserved
0x0004    Source 1 priority
0x0008    Source 2 priority
...
0x0800    Source 511 priority

>----------------
Reserved

0x0804 ... 0x0FFF

>----------------
IP (Interrupt Pending) array: 32 sources per 32b word

0x1000
...
0x103C

>----------------
Reserved

0x1018 ... 0x1FFF

>----------------
IE (interrupt enables) array for Hart0 M mode (1 bit per source)

0x2000
...
0x2014

>----------------
Reserved

0x2018 ... 0x207F

>----------------
IE (interrupt enables) array for Hart1 M mode (1 bit per source)

0x2080
...
0x2094

>----------------
IE (interrupt enables) array for Hart1 S mode (1 bit per source)

0x2100
...
0x2114

>----------------
IE (interrupt enables) array for Hart2 M mode (1 bit per source)

0x2180
...
0x2194

>----------------
IE (interrupt enables) array for Hart2 S mode (1 bit per source)

0x2200
...
0x2214

>----------------
IE (interrupt enables) array for Hart3 M mode (1 bit per source)

0x2280
...
0x2294

>----------------
IE (interrupt enables) array for Hart3 S mode (1 bit per source)

0x2300
...
0x2314

>----------------
IE (interrupt enables) array for Hart4 M mode (1 bit per source)

0x2380
...
0x2394

>----------------
IE (interrupt enables) array for Hart4 S mode (1 bit per source)

0x2400
...
0x2414

>----------------
Reserved

0x0C00 2480 ... 0x0C1F FFFF

>----------------
Threshold (of priority) and Claim/Complete registers

0x0C20 0000    Hart 0 M-mode
0x0C20 0004    Hart 0 M-mode claim/complete
>----------------
0x0C20 1000    Hart 1 M-mode
0x0C20 1004    Hart 1 M-mode claim/complete
>----------------
0x0C20 2000    Hart 1 S-mode
0x0C20 2004    Hart 1 S-mode claim/complete
>----------------
0x0C20 3000    Hart 2 M-mode
0x0C20 3004    Hart 2 M-mode claim/complete
>----------------
0x0C20 4000    Hart 2 S-mode
0x0C20 4004    Hart 2 S-mode claim/complete
>----------------
0x0C20 5000    Hart 3 M-mode
0x0C20 5004    Hart 3 M-mode claim/complete
>----------------
0x0C20 6000    Hart 3 S-mode
0x0C20 6004    Hart 3 S-mode claim/complete
>----------------
0x0C20 7000    Hart 4 M-mode
0x0C20 7004    Hart 4 M-mode claim/complete
>----------------
0x0C20 8000    Hart 4 S-mode
0x0C20 8004    Hart 4 S-mode claim/complete
>----------------
