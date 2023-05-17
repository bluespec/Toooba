###  -*-Makefile-*-

# ================================================================

.PHONY: help
help:
	@echo '    make  compile      Recompile Core (CPU, caches) into Verilog_RTL'
	@echo '                           NOTE: needs Bluespec bsc compiler'
	@echo '    make  tagsparams   Generates the CHERI tag controller parameters source file'
	@echo ''
	@echo '    make  clean        Remove intermediate build-files'
	@echo '    make  full_clean   Restore this directory to pristine state'

.PHONY: compile
compile: compile_sim compile_synth

# ================================================================

REPO ?= $(CURDIR)/..
ARCH ?= RV64ACDFIMSUxCHERI

# ================================================================
# RISC-V config macros passed into Bluespec 'bsc' compiler

BSC_COMPILATION_FLAGS += \
	-D RV64 \
	-D ISA_PRIV_M  -D ISA_PRIV_S  -D ISA_PRIV_U  \
	-D SV39 \
	-D ISA_I  -D ISA_M  -D ISA_A  -D ISA_F  -D ISA_D  -D ISA_FD_DIV  -D ISA_C  \
	-D CheriBusBytes=64 \
	-D CheriMasterIDWidth=1 \
	-D CheriTransactionIDWidth=6 \
	-D PERFORMANCE_MONITORING \
	-D SHIFT_BARREL    \
	-D MULT_SERIAL    \
	-D Near_Mem_Caches    \
	-D FABRIC64    \
	-D CAP128 \
	-D MEM512 \
	-D RISCV \
	-D TSO_MM \
	-D INCLUDE_GDB_CONTROL \
	-D BRVF_TRACE \
	-D XILINX_BSCAN  -D JTAG_TAP

#-D MELTDOWN_CF \
#-D NO_SPEC_TRAINING -D NO_SPEC_REDIRECT -D NO_SPEC_STRAIGHT_PATH -D SPEC_RSB_FIXUP -D NO_SPEC_STL \
#-D NO_SPEC_TRAINING -D NO_SPEC_REDIRECT -D NO_SPEC_STRAIGHT_PATH -D SPEC_RSB_FIXUP -D NO_SPEC_RSB_PUSH -D NO_SPEC_STL \
#-D NO_SPEC_STRAIGHT_PATH -D SPEC_RSB_FIXUP \
# Synth only BSC_COMPILATION_FLAGS
SYNTH_BSC_OPTIONS = -D XILINX_XCVU9P

# Sim only BSC_COMPILATION_FLAGS
SIM_BSC_OPTIONS = -D BSIM

# Set number of cores for RISCY config
CORE_NUM = 2

# Only used if we don't have INCLUDE_GDB_CONTROL
#	-D EXTERNAL_DEBUG_MODULE

include $(REPO)/builds/Resources/Include_RISCY_Config.mk

# ================================================================
# Search path for bsc for .bsv files
COREDIR ?= $(REPO)
COREW_DIRS = $(CURDIR)/src_BSV:$(COREDIR)/src_Core/Core:$(COREDIR)/src_Core/CPU:$(COREDIR)/src_Core/ISA:$(COREDIR)/src_Core/PLIC:$(COREDIR)/src_Core/Debug_Module:$(COREDIR)/src_Core/BSV_Additional_Libs:$(COREDIR)/src_Core/RISCY_OOO/procs/RV64G_OOO:$(COREDIR)/src_Core/RISCY_OOO/procs/lib:$(COREDIR)/src_Core/RISCY_OOO/coherence/src:$(COREDIR)/src_Core/RISCY_OOO/fpgautils/lib
WINDCOREIFCDIR ?= $(COREDIR)/libs/WindCoreInterface
CHERICAPLIBDIR ?= $(COREDIR)/libs/cheri-cap-lib
TAGCONTROLLERDIR ?= $(COREDIR)/libs/TagController
RISCVHPMEVENTSDIR ?= $(COREDIR)/libs/RISCV_HPM_Events
TAGCONTROLLER_DIRS = $(TAGCONTROLLERDIR)/TagController:$(TAGCONTROLLERDIR)/TagController/CacheCore
BLUESTUFFDIR ?= $(COREDIR)/libs/BlueStuff
BLUEAXI4DIR = $(BLUESTUFFDIR)/BlueAXI4
BLUEAXI4DIRS = $(BLUEAXI4DIR):$(BLUEAXI4DIR)/AXI4:$(BLUEAXI4DIR)/AXI4Lite:$(BLUEAXI4DIR)/AXI4Stream:$(BLUEAXI4DIR)/BlueUnixBridges
BLUEUNIXBRIDGESDIR = $(BLUEAXI4DIR)/BlueUnixBridges
BLUEBASICSDIR = $(BLUESTUFFDIR)/BlueBasics
BLUEUTILSDIR = $(BLUESTUFFDIR)/BlueUtils
BLUESTUFF_DIRS = $(BLUESTUFFDIR):$(BLUEAXI4DIRS):$(BLUEBASICSDIR):$(BLUEUTILSDIR):$(BLUESTUFFDIR)/Stratix10ChipID

# search path for bsc imports
ifdef BSC_CONTRIB_DIR
BSC_CONTRIB_LIB_DIR = $(BSC_CONTRIB_DIR)/lib/Libraries
else
BSC_CONTRIB_LIB_DIR = %/Libraries
endif
BSC_CONTRIB_DIRS = $(BSC_CONTRIB_LIB_DIR)/Bus
BSVPATH = +:$(BSC_CONTRIB_DIRS):$(WINDCOREIFCDIR):$(RISCVHPMEVENTSDIR):$(CHERICAPLIBDIR):$(TAGCONTROLLER_DIRS):$(COREW_DIRS):$(BLUESTUFF_DIRS)
BSC_PATH = -p $(BSVPATH)

# ----------------
# Top-level file and module

TOPFILE   = src_BSV/P3_Core.bsv
TOPMODULE = mkP3_Core

# ================================================================
# More bsc compilation flags
BSC_COMPILATION_FLAGS += \
	-keep-fires -aggressive-conditions \
	-no-warn-action-shadowing \
	-suppress-warnings G0020    \
	+RTS -K128M -RTS  -show-range-conflict \
	-unspecified-to X -opt-undetermined-vals \
	-steps-max-intervals 10000000 \
	-steps-warn-interval 1000000

# ================================================================
# Generate Bluespec CHERI tag controller source file
CAPSIZE = 128
TAGS_STRUCT = 0 64
TAGS_ALIGN = 32
.PHONY: tagsparams
tagsparams: src_BSV/TagTableStructure.bsv
src_BSV/TagTableStructure.bsv: $(REPO)/libs/TagController/tagsparams.py
	@echo "INFO: Re-generating CHERI tag controller parameters"
	$^ -v -c $(CAPSIZE) -s $(TAGS_STRUCT:"%"=%) -a $(TAGS_ALIGN) --data-store-base-addr 0xc0000000 -b $@ 0xbfff8000 0x17ffff000
	@echo "INFO: Re-generated CHERI tag controller parameters"


.PHONY: generate_hpm_vector
generate_hpm_vector: GenerateHPMVector.bsv
GenerateHPMVector.bsv: $(RISCVHPMEVENTSDIR)/parse_counters.py
	@echo "INFO: Re-generating GenerateHPMVector bluespec file"
	$^ $(RISCVHPMEVENTSDIR)/counters.yaml -m ProcTypes -b $@
	@echo "INFO: Re-generated GenerateHPMVector bluespec file"


.PHONY: stat_counters
stat_counters: StatCounters.bsv
StatCounters.bsv: $(RISCVHPMEVENTSDIR)/parse_counters.py
	@echo "INFO: Re-generating HPM events struct bluepsec file"
	$^ $(RISCVHPMEVENTSDIR)/counters.yaml -m ProcTypes -s $@
	@echo "INFO: Re-generated HPM events struct bluespec file"
compile_sim: tagsparams stat_counters generate_hpm_vector
compile_synth: tagsparams stat_counters generate_hpm_vector

# ================================================================
# Generate Verilog RTL from BSV sources (needs Bluespec 'bsc' compiler)

BUILD_DIRS_SYNTH = -bdir build_dir_synth  -info-dir build_dir_synth
BUILD_DIRS_SIM = -bdir build_dir_sim  -info-dir build_dir_sim

build_dir_synth:
	mkdir -p $@

build_dir_sim:
	mkdir -p $@

Verilog_RTL:
	mkdir -p $@

Verilog_RTL_sim:
	mkdir -p $@

.PHONY: compile_synth
compile_synth: | build_dir_synth Verilog_RTL
	@echo  "INFO: Generating RTL into Verilog_RTL for synthesis ..."
	bsc -u -elab -verilog  -vdir Verilog_RTL $(BUILD_DIRS_SYNTH)  $(BSC_COMPILATION_FLAGS)  $(SYNTH_BSC_OPTIONS) $(BSC_PATH)  $(TOPFILE)
	@echo  "INFO: Generated Synth RTL into Verilog_RTL"

.PHONY: compile_sim
compile_sim: | build_dir_sim Verilog_RTL_sim
	@echo  "INFO: Generating RTL into Verilog_RTL_sim for simulation ..."
	bsc -u -elab -verilog  -vdir Verilog_RTL_sim $(BUILD_DIRS_SIM)  $(BSC_COMPILATION_FLAGS) $(SIM_BSC_OPTIONS) $(BSC_PATH)  $(TOPFILE)

# ================================================================

.PHONY: clean
clean:
	rm -r -f  *~  Makefile_*  build_dir_sim build_dir_synth src_BSV/TagTableStructure.bsv

.PHONY: full_clean
full_clean: clean
	rm -r -f  *.log  Verilog_RTL Verilog_RTL_sim

# ================================================================
