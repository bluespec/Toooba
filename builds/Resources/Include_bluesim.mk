###  -*-Makefile-*-

# Copyright (c) 2018-2019 Bluespec, Inc. All Rights Reserved

# This file is not a standalone Makefile, but 'include'd by other Makefiles

# ================================================================
# Compile Bluesim intermediate files from BSV sources (needs Bluespec 'bsc' compiler)

TMP_DIRS  = -bdir build_dir  -simdir build_dir  -info-dir build_dir

build_dir:
	mkdir -p $@

ifeq (,$(filter clean full_clean,$(MAKECMDGOALS)))
include .depends.mk

.depends.mk: TagTableStructure.bsv StatCounters.bsv GenerateHPMVector.bsv | build_dir
	if ! bluetcl -exec makedepend -elab -sim  $(TMP_DIRS)  $(RTL_GEN_DIRS)  $(BSC_COMPILATION_FLAGS)  -p $(BSC_PATH) -o $@ $(TOPFILE); then rm -f $@ && false; fi
endif

%.bo:
	$(info building $@)
	bsc -elab -sim  $(TMP_DIRS)  $(RTL_GEN_DIRS)  $(BSC_COMPILATION_FLAGS)  -p $(BSC_PATH) $<

.PHONY: compile
compile: build_dir/Top_HW_Side.bo | build_dir
#	@echo "INFO: Re-compiling Core (CPU, Caches)"
#	bsc -u -elab -sim  $(TMP_DIRS)  $(BSC_COMPILATION_FLAGS)  -p $(BSC_PATH)  $(TOPFILE)
#	@echo "INFO: Re-compiled  Core (CPU, Caches)"

# ================================================================
# Compile and link Bluesim intermediate files into a Bluesim executable

SIM_EXE_FILE = exe_HW_sim

BSC_C_FLAGS += \
	-Xl -v \
	-Xc -O1 -Xc++ -O1 \


# For Bluespec_2019.05.beta2-debian9stretch-amd64
# you may have to remove the line: -Xc++ -D_GLIBCXX_USE_CXX11_ABI=0

.PHONY: simulator
simulator: build_dir/Top_HW_Side.bo
	@echo "INFO: linking bsc-compiled objects into Bluesim executable"
	bsc -sim -parallel-sim-link 8 +RTS -K128M -RTS \
		$(TMP_DIRS) \
		-e $(TOPMODULE) -o ./$(SIM_EXE_FILE) \
		$(BSC_C_FLAGS) \
		$(REPO)/src_Verifier/BSV-RVFI-DII/SocketPacketUtils/socket_packet_utils.c \
		$(REPO)/src_Testbench/Top/C_Imported_Functions.c
	@echo "INFO: linked bsc-compiled objects into Bluesim executable"

# ================================================================
