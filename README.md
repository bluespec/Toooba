# Open-source RISC-V CPUs from Bluespec, Inc.

This is one of a family of free, open-source RISC-V CPUs created by Bluespec, Inc.

- [Piccolo](https://github.com/bluespec/Piccolo): 3-stage, in-order pipeline

  Piccolo is intended for low-end applications (Embedded Systems, IoT, microcontrollers, etc.).

- [Flute](https://github.com/bluespec/Flute): 5-stage, in-order pipeline

  Flute is intended for low-end to medium applications that require
  64-bit operation, an MMU (Virtual Memory) and more performance than
  Piccolo-class processors.

- [Toooba](https://github.com/bluespec/Toooba): superscalar, out-of-order
  pipeline, slight variation on MIT's RISCY-OOO

  Toooba is intended as a high-end application processor.

The three repo structures are nearly identical, and the ways to build
and run are identical.

----------------------------------------------------------------
### Note re. distribution of MIT RISCY-OOO sources.

The directory `src_Core/RISCY_OOO` contains sources copied from MIT's
`riscy-OOO` repository.  See `LICENSE_RISCY-OOO` for MIT's license.

[Note: MIT's repository is on an MIT git server, which can only be
 accessed with credentials; hence the local copy in of these files.]

Bluespec's modifications to files in src_Core/RISCY_OOO are relatively
small and mostly additive:

- To add the RISC-V 'C' extension (compressed instructions)
- To add support for Bluespec's Tandem Verification
- To add support for Bluespec's Debug Module.
- To fix about bugs leading to about half a dozen failures of standard RISC-V ISA tests

----------------------------------------------------------------
### About the source codes (in BSV and Verilog)

The BSV source code in this repository, from which the synthesizable
Verilog RTL in this repository is generated, is highly parameterized
to allow generating many possible configurations, some of which are
adequate to boot a Linux kernel.

The pre-generated synthesizable Verilog RTL source files in this
repository are for one specific configuration:

1. RV64ACDFIMSU    (a.k.a. RV64GC)
    - RV64I: base RV64 integer instructions
    - 'A' extension: atomic memory ops
    - 'C' extension: compressed instructions
    - 'D' extension: double-precision floating point instructions
    - 'F' extension: single-precision floating point instructions
    - 'M' extension: integer multiply/divide instructions
    - Privilege levels M (machine), S (Supervisor) and U (user)
    - Supports external, timer, software and non-maskable interrupts
    - Passes all riscv-isa tests for RV64ACDFIMSU
    - Boots the Linux kernel

If you want to generate other Verilog variants, you'll need a Bluespec
`bsc` compiler [Note: Bluespec, Inc. provides free licenses to
academia and for non-profit research].

### Testbench included

This repository contains a simple testbench (a small SoC) with which
one can run RISC-V binaries in simulation by loading standard mem hex
files and executing in Bluespec's Bluesim, Verilator simulation or
iVerilog simulation.  The testbench contains an AXI4 interconnect
fabric that connects the CPU to models of a boot ROM, a memory, a
timer and a UART for console I/O.

[Note: **iverilog functionality is currently limited** because we are
still working out robust mechanisms to import C code, which is used in
parts of the testbench.]

This repository contains one sample build directory, to build
an RV64ACDFIMSU simulator, using Verilator Verilog simulation.

The generated Verilog is synthesizable. Bluespec tests all this code
on Xilinx FPGAs.

#### Plans

- Ongoing continuous micro-architectural improvements for performance and hardware area.

----------------------------------------------------------------
## Source codes

This repository contains two levels of source code: Verilog and BSV.

**Verilog RTL** can be found in directories with names suffixed in
'_verilator' or '_iverilog' in the 'builds' directory:

        builds/..._<verilator or iverilog>/Verilog_RTL/

[There is no difference between Verilog in a Verilator directory
vs. the corresponding iverilog directory. ]

The Verilog RTL is _synthesizable_ (and hence acceptable to
Verilator).  It can be simulated in any Verilog simulator (we provide
Makefiles to build simulation executables for Verilator and for Icarus
Verilog (iverilog)).

The RTL represents RISC-V CPU RTL, plus a rudimentary surrounding SoC
enabling immediate simulation here, and which is rich enough to enable
booting a Linux kernel.  Users are free to use the CPU RTL in their
own Verilog system designs.  The top-level module for the CPU RTL is
`Verilog_RTL/mkProc.v`.  The top-level module for the surrounding
SoC is `Verilog_RTL/mkTop_HW_Side.v`.  The SoC has an AXI4
fabric, a timer, a software-interrupt device, and a UART.  Additional
library RTL can be found in the directory `src_bsc_lib_RTL`.

**Bluespec BSV** source code (which was used to generate the Verilog RTL) can be found in:

- `src_Core/`, for the CPU core, with sub-directories:
   - `Core/`: the top-level of the CPU Core (specifically, the files CoreW_IFC.bsv and CoreW.bsv)
   - 'CPU/': more CPU core sources
   - 'RISCY_OOO': the bulk of the code, taken from MIT's riscy-ooo design, with local modifications.
   - `ISA/`:  generic types/constants/functions for the RISC-V ISA (not CPU-implementation-specific)
   - 'PLIC/': Platform-Level Interrupt Controller (standard RISC-V spec)
   - `BSV_Additional_Libs/`: generic utilities (not CPU-specific)
   - `Debug_Module/`: RISC-V Debug Module to debug the CPU from GDB or other debuggers

- `src_Testbench/`, for the surrounding testbench, with sub-directories:

   - `Top/`: The system top-level (`Top_HW_Side.bsv`), a memory model
       that loads from a memory hex file, and some imported C
       functions for polled reads from the console tty (not currently
       available for Icarus Verilog).

   - `SoC/`: An interconnect, a boot ROM, a memory controller, a timer
       and software-interrupt device, and a UART for console tty I/O.

   - `Fabrics/`: Generic AXI4 code for the SoC fabric.

The BSV source code has a rich set of parameters. The provided RTL
source has been generated from the BSV source automatically using
Bluespec's `bsc` compiler, with certain particular sets of choices for
the various parameters.  The generated RTL is not parameterized.

To generate Verilog variants with other parameter choices, the user
will need Bluespec's `bsc` compiler.  See the next section for
examples of how the build is configured for different ISA features.

`BSV_Additional_Libs` contains a submodule, `BlueStuff`, which must be checked out using:
```sh
$ git submodule update --init --recursive
```
This command may need to be repeated when this parent repository
is updated to point to newer versions of the `BlueStuff` repository.

In fact the CPU also supports a "Tandem Verifier" that produces an
instruction-by-instruction trace that can be checked for correctness
against a RISC-V Golden Reference Model.  Please contact Bluespec,
Inc. for more information.

----------------------------------------------------------------
### Building and running from the Verilog sources, out of the box

In the Verilog-build directory:

            builds/RV64ACDFIMSU_Toooba_verilator/

  - `$ make simulator` will create a Verilog simulation executable using Verilator

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

#### Tool dependencies:

We test our builds with the following versions
Verilator.  Later versions are probably ok; we have observed some
problems with earlier versions.

        $ verilator --version
        Verilator 3.922 2018-03-17 rev verilator_3_920-32-gdf3d1a4

----------------------------------------------------------------
### What you can build and run if you have Bluespec's `bsc` compiler

[Note: Bluespec, Inc. provides free licenses to academia and for non-profit research].

Note: even without Bluespec's `bsc` compiler, you can use the Verilog
sources in any of the `builds/<ARCH>_<CPU>_verilator/Verilog_RTL`
directories-- build and run Verilog simulations, incorporate the
Verilog CPU into your own SoC, etc.  This section describes additional
things you can do with a `bsc` compiler.

#### Building a Bluesim simulator

In any of the following directories:

        builds/<ARCH>_<CPU>_bluesim

  - `$ make compile simulator`

will compile and link a Bluesim executable.  Then, you can `make test`
or `make isa_tests` as described above to run an individual ISA test
or run regressions on the full suite of relevant ISA tests.

#### Re-generating Verilog RTL

You can regenerate the Verilog RTL in any of the
`build/<ARCH>_<CPU>_verilator/` or `build/<ARCH>_<CPU>_iverilog/`
directories.  Example:

        $ cd  builds/RV32ACIMU_<CPU>_verilator
        $ make compile

#### Creating a new architecture configuration

[This documentation needs to be fleshed out.] The `builds/Resources`
directory contains some "include" files for Makefiles, and illustrate
the compile-time flags that determine the micro-architectural
configuration.

In addition, MIT's riscy-ooo code provides further configuration
controls, which can be found in:

        Toooba/src_Core/RISCY_OOO/procs/RV64G_OOO/ProcConfig.bsv

----------------------------------------------------------------
