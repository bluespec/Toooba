# CHERI-Enabled Out-of-Order RISC-V Core

This is a prototype of an out-of-order core that implements hardware capabilities (see [CHERI](https://www.cl.cam.ac.uk/research/security/ctsrd/cheri/) for details).
It is based off of [Bluespec's Toooba](https://github.com/bluespec/Toooba), which is a slight variation of [MIT's RisyOO core](https://github.com/csail-csg/riscy-OOO).

## Note re. distribution of MIT RISCY-OOO sources.

The directory `src_Core/RISCY_OOO` contains sources copied from MIT's
`riscy-OOO` repository.  See `LICENSE_RISCY-OOO` for MIT's license.

Bluespec's modifications to files in src_Core/RISCY_OOO are relatively
small and mostly additive:

- To add the RISC-V 'C' extension (compressed instructions)
- To add support for Bluespec's Tandem Verification
- To add support for Bluespec's Debug Module.
- To fix about bugs leading to about half a dozen failures of standard RISC-V ISA tests

The University of Cambridge made changes to RiscyOO to add support for [CHERI capabilities](https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-941.pdf). For details on what CHERI instructions do, please see the [Instruction Set Architecture document](https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-951.pdf).

## About the source codes (in BSV and Verilog)

The BSV source code in this repository, from which the synthesizable
Verilog RTL in this repository is generated, is highly parameterized
to allow generating many possible configurations, some of which are
adequate to boot a Linux kernel.

The pre-generated synthesizable Verilog RTL source files in this
repository are for one specific configuration:

1. RV64ACDFIMSUxCHERI    (a.k.a. RV64GCxCHERI)
    - RV64I: base RV64 integer instructions
    - 'A' extension: atomic memory ops
    - 'C' extension: compressed instructions
    - 'D' extension: double-precision floating point instructions
    - 'F' extension: single-precision floating point instructions
    - 'M' extension: integer multiply/divide instructions
    - 'xCHERI' extension: capability-based security extensions
    - Privilege levels M (machine), S (Supervisor) and U (user)
    - Supports external, timer, software and non-maskable interrupts
    - Passes all riscv-isa tests for RV64ACDFIMSU
    - Boots the Linux kernel

If you want to generate other Verilog variants, you'll need a Bluespec
`bsc` compiler, which is open source and can be found in [this repository](https://github.com/B-Lang-org/bsc).

## Testbench included

This repository contains a simple testbench (a small SoC) with which
one can run RISC-V binaries in simulation by loading standard mem hex
files.  The testbench contains an AXI4 interconnect
fabric that connects the CPU to models of a boot ROM, a memory, a
timer and a UART for console I/O.

[Note: **iverilog functionality is currently limited** because we are
still working out robust mechanisms to import C code, which is used in
parts of the testbench.]

This repository contains four sample build directories, to build
an RV64ACDFIMSUxCHERI simulator, using Bluesim and Verilog simulation.
There are also RVFI-DII variants of these to be used with [TestRIG](https://github.com/CTSRD-CHERI/TestRIG).
The generated Verilog is synthesizable.

## Simulation

We currently only support Bluesim and Verilator simulation. There is also some code related to simulation on iVerilog, but this is currently not working and not being maintained.

## Source codes

This repository contains two levels of source code: Verilog and BSV.

**Verilog RTL** can be found in directories with names suffixed in
'_verilator' in the 'builds' directory:

        builds/..._verilator/Verilog_RTL/

The Verilog RTL is _synthesizable_ (and hence acceptable to
Verilator).  It can be simulated in any Verilog simulator.

The RTL represents RISC-V CPU RTL, plus a rudimentary surrounding SoC
enabling immediate simulation here, and which is rich enough to enable
booting a Linux kernel.  Users are free to use the CPU RTL in their
own Verilog system designs.  The top-level module for the CPU RTL is
`Verilog_RTL/mkProc.v`.  The top-level module for the surrounding
SoC was originally `Verilog_RTL/mkTop_HW_Side.v`, but is now `Verilog_RTL/mkTop_HW_Side_edited.v`.  The SoC has an AXI4
fabric, a timer, a software-interrupt device, and a UART.  Additional
library RTL can be found in the directory `src_bsc_lib_RTL`.

**Bluespec BSV** source code (which was used to generate the Verilog RTL) can be found in:

- `src_Core/`, for the CPU core, with sub-directories:
   - `Core/`: the top-level of the CPU Core (specifically, CoreW.bsv)
   - `CPU/`: more CPU core sources
   - `RISCY_OOO/`: the bulk of the code, taken from MIT's riscy-ooo design, with local modifications.
   - `ISA/`:  generic types/constants/functions for the RISC-V ISA (not CPU-implementation-specific)
   - `PLIC/`: Platform-Level Interrupt Controller (standard RISC-V spec)
   - `BSV_Additional_Libs/`: generic utilities (not CPU-specific)
   - `Debug_Module/`: RISC-V Debug Module to debug the CPU from GDB or other debuggers

- `src_Testbench/`, for the surrounding testbench, with sub-directories:

   - `Top/`: The system top-level (`Top_HW_Side.bsv`), a memory model
       that loads from a memory hex file, and some imported C
       functions for polled reads from the console tty (not currently
       available for Icarus Verilog).

   - `SoC/`: An interconnect, a boot ROM, a memory controller, a timer
       and software-interrupt device, and a UART for console tty I/O.

The BSV source code has a rich set of parameters. The provided RTL
source has been generated from the BSV source automatically using
Bluespec's `bsc` compiler, with certain particular sets of choices for
the various parameters.  The generated RTL is not parameterized.

To generate Verilog variants with other parameter choices, the user
will need Bluespec's `bsc` compiler.  See the next section for
examples of how the build is configured for different ISA features.

In fact the CPU also supports a "Tandem Verifier" that produces an
instruction-by-instruction trace that can be checked for correctness
against a RISC-V Golden Reference Model.  Please contact Bluespec,
Inc. for more information.

## Build Instructions

First clone this repository and then inside the repository initialize the submodules:

       $ git clone git@github.com:CTSRD-CHERI/Toooba.git
       $ cd Toooba
       $ git submodule update --init --recursive

### Dependencies

Build the Bluespec Compiler `bsc` from [this repository](https://github.com/B-Lang-org/bsc). You will also need set the `$BLUESPECDIR` to the `lib` folder of your `bsc` install. By default this is located in `inst/lib` directory inside your bsc repo. Also, make sure to add the `inst/bin` directory to your `$PATH` environment variable.

If you wish to use Verilator, the build has been tested on versions 3.922 and 3.926.
You can build any version of Verilator from [this repository](https://github.com/verilator/verilator/releases) and follow the build instructions [on the official website](https://www.veripool.org/projects/verilator/wiki/Installing).
Using version 4.002 or later is currently not supported.

        $ verilator --version
        Verilator 3.922 2018-03-17 rev verilator_3_920-32-gdf3d1a4

### Building a simulator using Bluespec's Bluesim

To build a Bluesim-based simulator, use the following commands to generate the elaboration files and compile them:

        $ cd builds/RV64ACDFIMSUxCHERI_Toooba_bluesim
        $ make compile
        $ make simulator

### Building a simulator using Verilator

To build a Verilator-based simulator, use the following commands to generate the Verilog RTL and compile it using Verilator:

        $ cd builds/RV64ACDFIMSUxCHERI_Toooba_verilator
        $ make compile
        $ make simulator

### Running a simulator

You must have followed one of the steps above to build a simulator (with either Bluesim or Verilator).
In the corresponding build directory:

  - `$ make test` will run the executable on the standard RISC-V ISA
        test `rv32ui-p-add` or `rv64ui-p-add`, which is one of the
        tests in the `Tests/isa/` directory.  Examining the `test:`
        target in `Makefile`, we see that it first runs the program
        `Tests/elf_to_hex/elf_to_hex` on the `rv32ui-p-add` or
        `rv64ui-p-add` ELF file to create a `Mem.hex` file, and then
        runs the simulation executable which loads this `Mem.hex` file
        into its memory.

  - `$ make TEST=<isa_test_name> test` will run the executable on the
        standard RISC-V ISA test whose name is supplied.
        The full set of standard isa tests are in the `Tests/isa/` directory.

  - `$ make isa_tests` will run the executable on
      all the standard RISC-V ISA tests relevant for RV64ACDFIMSU (regression testing).
      This uses the Python script `Tests/Run_regression.py`.
      Please see the documentation at the top of that program for details.
