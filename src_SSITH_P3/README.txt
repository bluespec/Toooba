Copyright (c) 2019 Bluespec, Inc.  All Rights Reserved.

This directory is intended for DARPA SSITH users; others may safely ignore it.

This directory contains a wrapper and other resources that package up
MIT's RISCY-OOO to fit into the "standard" core socket in the SSITH GFE
("Government Furnished Equipment").

>================================================================
Context:

The SSITH system is an SoC with a "socket" (placeholder) for a "Core"
module (a RISC-V CPU).  Various implementations are/will be plugged
into this socket:

 - "P1"
    - Baseline Piccolo (BSV) based core
    - Baseline Rocket (Chisel) based core
    - Variations/alternatives by various SSITH project teams

 - "P2"
    - Baseline Flute (BSV) based core
    - Baseline Rocket (Chisel) based core
    - Variations/alternatives by various SSITH project teams

 - "P3"
    - Baseline Tooba (BSV) based core
    - Baseline BOOM (Chisel) based core
    - Variations/alternatives by various SSITH project teams

>================================================================
Extra dependencies:

On top of the dependencies in the top project, you must have a build of bsc-contrib,
which you can find here: https://github.com/B-Lang-org/bsc-contrib
The Makefile expects the folder that is generated in bsc-contrib/inst/lib/Libraries/Bus
to be located in the default bsc build at bsc/inst/lib/Libraries.

>================================================================
Whenever there are changes to the Toooba core, rerun:

  $ make compile
      (which generates RTL and then $ cp Verilog_RTL/* xilinx_ip/hdl/)

The synthesis version (Verilog_RTL) uses Xilinx IP for the integer divider,
while the simulation version (Verilog_RTL_sim) uses a model.

>================================================================
