# Open-source RISC-V CPUs from Bluespec, Inc.

***** UNDER CONSTRUCTION *****

***** PLEASE COME BACK LATER (EARLY APRIL 2019) *****

This is one of a family of free, open-source RISC-V CPUs created by Bluespec, Inc.

- [Piccolo](https://github.com/bluespec/Piccolo): 3-stage, in-order pipeline

  Piccolo is intended for low-end applications (Embedded Systems, IoT, microcontrollers, etc.).

- [Flute](https://github.com/bluespec/Flute): 5-stage, in-order pipeline

  Flute is intended for low-end to medium applications that require
  64-bit operation, an MMU (Virtual Memory) and more performance than
  Piccolo-class processors.

- [Tooba](https://github.com/bluespec/Tooba): superscalar, out-of-order
  pipeline, slight variation on MIT's RISCY-OOO [In progress!]

----------------------------------------------------------------
### Note re. distribution of MIT RISCY-OOO sources.

The directory `src_Core/RISCY_OOO` contains sources copied from MIT's
`riscy-OOO` repository.  See `LICENSE_RISCY-OOO` for MIT's license.

[Note: MIT's repository is on an MIT git server, which can only be
 accessed with credentials; hence the local copy in of these files.]

----------------------------------------------------------------
### Building and running Tooba

You will need:

- A Bluespec tools installation (so you can run 'bsc', the Bluespec
  compiler for BSV).  We recommend version 2018.10.beta1 or later.

- A Verilator installation.  We recommend version 3.922 or later.

Then:

    $ cd  builds/RV64ADFIMSU_Tuba_verilator
    $ make all

This will compile BSV sources using the 'bsc' compiler into Verilog in
the directory `Verilog_RTL`, then compile and link into a verilator
executable `exe_HW_sim`.

Then:

    $ make test                                   (1)
    $ make TEST=<isa_test_name> test              (2)
    $ make isa_tests                              (3)

(1) Will run a single ISA test, `rv64ui-p-add`.
(2) Will do the same, but with the ISA test whose name you supply.
(3) Will run ISA tests for RV64G.

----------------------------------------------------------------
