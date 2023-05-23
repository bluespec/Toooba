# This Makefile copies source BSV files from the MIT riscy-OOO repo
# for use in the Bluespec environment

RISCY_HOME = ~/Projects/RISCV/MIT-riscy/riscy-OOO

copyfiles:  copy_RV64_OOO_files  copy_procs_lib_files  copy_coherence_src_files  copy_fpgautils_files  copy_connectal_files

PROCS_RV64G_OOO      = procs/RV64G_OOO
.PHONY: copy_RV64_OOO_files
copy_RV64_OOO_files:
	mkdir -p  $(PROCS_RV64G_OOO)
	cp -p $(RISCY_HOME)/$(PROCS_RV64G_OOO)/AluExePipeline.bsv                 ./$(PROCS_RV64G_OOO)/
	cp -p $(RISCY_HOME)/$(PROCS_RV64G_OOO)/CommitStage.bsv                    ./$(PROCS_RV64G_OOO)/
	cp -p $(RISCY_HOME)/$(PROCS_RV64G_OOO)/FetchStage.bsv                     ./$(PROCS_RV64G_OOO)/
	cp -p $(RISCY_HOME)/$(PROCS_RV64G_OOO)/FpuMulDivExePipeline.bsv           ./$(PROCS_RV64G_OOO)/
	cp -p $(RISCY_HOME)/$(PROCS_RV64G_OOO)/MemExePipeline.bsv                 ./$(PROCS_RV64G_OOO)/
	cp -p $(RISCY_HOME)/$(PROCS_RV64G_OOO)/ProcConfig.bsv                     ./$(PROCS_RV64G_OOO)/
	cp -p $(RISCY_HOME)/$(PROCS_RV64G_OOO)/RenameStage.bsv                    ./$(PROCS_RV64G_OOO)/
	cp -p $(RISCY_HOME)/$(PROCS_RV64G_OOO)/ReorderBufferSynth.bsv             ./$(PROCS_RV64G_OOO)/
	cp -p $(RISCY_HOME)/$(PROCS_RV64G_OOO)/ReservationStationAlu.bsv          ./$(PROCS_RV64G_OOO)/
	cp -p $(RISCY_HOME)/$(PROCS_RV64G_OOO)/ReservationStationFpuMulDiv.bsv    ./$(PROCS_RV64G_OOO)/
	cp -p $(RISCY_HOME)/$(PROCS_RV64G_OOO)/ReservationStationMem.bsv          ./$(PROCS_RV64G_OOO)/
	cp -p $(RISCY_HOME)/$(PROCS_RV64G_OOO)/RFileSynth.bsv                     ./$(PROCS_RV64G_OOO)/
	cp -p $(RISCY_HOME)/$(PROCS_RV64G_OOO)/ScoreboardSynth.bsv                ./$(PROCS_RV64G_OOO)/
	cp -p $(RISCY_HOME)/$(PROCS_RV64G_OOO)/SynthParam.bsv                     ./$(PROCS_RV64G_OOO)/

PROCS_LIB            = procs/lib
.PHONY: copy_procs_lib_files
copy_procs_lib_files:
	mkdir -p  $(PROCS_LIB)
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/Amo.bsv                    ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/Bht.bsv                    ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/BrPred.bsv                 ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/Btb.bsv                    ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/Bypass.bsv                 ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/CacheUtils.bsv             ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/ConcatReg.bsv              ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/Decode.bsv                 ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/DirPredictor.bsv           ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/DTlb.bsv                   ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/Ehr.bsv                    ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/EpochManager.bsv           ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/Exec.bsv                   ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/Fifo.bsv                   ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/Fpu.bsv                    ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/FullAssocTlb.bsv           ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/GlobalBrHistReg.bsv        ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/GlobalSpecUpdate.bsv       ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/GSelectPred.bsv            ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/GSharePred.bsv             ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/HasSpecBits.bsv            ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/ITlb.bsv                   ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/L1CoCache.bsv              ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/L1LLConnect.bsv            ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/L2SetAssocTlb.bsv          ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/L2Tlb.bsv                  ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/LatencyTimer.bsv           ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/LLCache.bsv                ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/LLCDmaConnect.bsv          ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/LLCRqMshrSecureModel.bsv   ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/MemLoader.bsv              ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/MemLoaderIF.bsv            ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/MemoryTypes.bsv            ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/MMIOAddrs.bsv              ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/MMIOCore.bsv               ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/MMIOInst.bsv               ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/MsgFifo.bsv                ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/MulDiv.bsv                 ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/Performance.bsv            ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/PhysRFile.bsv              ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/ProcTypes.bsv              ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/Ras.bsv                    ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/RenameDebugIF.bsv          ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/RenamingTable.bsv          ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/ReorderBuffer.bsv          ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/ReservationStationEhr.bsv  ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/SafeCounter.bsv            ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/Scoreboard.bsv             ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/SetAssocTlb.bsv            ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/SpecFifo.bsv               ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/SpecPoisonFifo.bsv         ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/SpecTagManager.bsv         ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/SplitLSQ.bsv               ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/StoreBuffer.bsv            ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/TlbConnect.bsv             ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/TlbTypes.bsv               ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/TourPred.bsv               ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/TourPredSecure.bsv         ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/TranslationCache.bsv       ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/Types.bsv                  ./$(PROCS_LIB)/
	cp -p $(RISCY_HOME)/$(PROCS_LIB)/VerificationPacket.bsv     ./$(PROCS_LIB)/

COHERENCE_SRC = coherence/src
.PHONY: copy_coherence_src_files
copy_coherence_src_files:
	mkdir -p  $(COHERENCE_SRC)
	cp -p $(RISCY_HOME)/$(COHERENCE_SRC)/CCPipe.bsv               ./$(COHERENCE_SRC)/
	cp -p $(RISCY_HOME)/$(COHERENCE_SRC)/CCTypes.bsv              ./$(COHERENCE_SRC)/
	cp -p $(RISCY_HOME)/$(COHERENCE_SRC)/CrossBar.bsv             ./$(COHERENCE_SRC)/
	cp -p $(RISCY_HOME)/$(COHERENCE_SRC)/IBank.bsv                ./$(COHERENCE_SRC)/
	cp -p $(RISCY_HOME)/$(COHERENCE_SRC)/ICRqMshr.bsv             ./$(COHERENCE_SRC)/
	cp -p $(RISCY_HOME)/$(COHERENCE_SRC)/IPRqMshr.bsv             ./$(COHERENCE_SRC)/
	cp -p $(RISCY_HOME)/$(COHERENCE_SRC)/L1Bank.bsv               ./$(COHERENCE_SRC)/
	cp -p $(RISCY_HOME)/$(COHERENCE_SRC)/L1CRqMshr.bsv            ./$(COHERENCE_SRC)/
	cp -p $(RISCY_HOME)/$(COHERENCE_SRC)/L1Pipe.bsv               ./$(COHERENCE_SRC)/
	cp -p $(RISCY_HOME)/$(COHERENCE_SRC)/L1PRqMshr.bsv            ./$(COHERENCE_SRC)/
	cp -p $(RISCY_HOME)/$(COHERENCE_SRC)/LLBank.bsv               ./$(COHERENCE_SRC)/
	cp -p $(RISCY_HOME)/$(COHERENCE_SRC)/LLCRqMshr.bsv            ./$(COHERENCE_SRC)/
	cp -p $(RISCY_HOME)/$(COHERENCE_SRC)/LLPipe.bsv               ./$(COHERENCE_SRC)/
	cp -p $(RISCY_HOME)/$(COHERENCE_SRC)/MshrDeadlockChecker.bsv  ./$(COHERENCE_SRC)/
	cp -p $(RISCY_HOME)/$(COHERENCE_SRC)/Prefetcher.bsv  		  ./$(COHERENCE_SRC)/
	cp -p $(RISCY_HOME)/$(COHERENCE_SRC)/RandomReplace.bsv        ./$(COHERENCE_SRC)/
	cp -p $(RISCY_HOME)/$(COHERENCE_SRC)/RWBramCore.bsv           ./$(COHERENCE_SRC)/
	cp -p $(RISCY_HOME)/$(COHERENCE_SRC)/SelfInvIBank.bsv         ./$(COHERENCE_SRC)/
	cp -p $(RISCY_HOME)/$(COHERENCE_SRC)/SelfInvIPipe.bsv         ./$(COHERENCE_SRC)/
	cp -p $(RISCY_HOME)/$(COHERENCE_SRC)/SelfInvL1Bank.bsv        ./$(COHERENCE_SRC)/
	cp -p $(RISCY_HOME)/$(COHERENCE_SRC)/SelfInvL1Pipe.bsv        ./$(COHERENCE_SRC)/
	cp -p $(RISCY_HOME)/$(COHERENCE_SRC)/SelfInvLLBank.bsv        ./$(COHERENCE_SRC)/
	cp -p $(RISCY_HOME)/$(COHERENCE_SRC)/SelfInvLLPipe.bsv        ./$(COHERENCE_SRC)/

FPGAUTILS_LIB               = fpgautils/lib
FPGAUTILS_XILINX_FPU        = fpgautils/xilinx/fpu
FPGAUTILS_XILINX_RESET_REGS = fpgautils/xilinx/reset_regs
.PHONY: copy_fpgautils_files
copy_fpgautils_files:
	mkdir -p  $(FPGAUTILS_LIB)
	mkdir -p  $(FPGAUTILS_XILINX_FPU)
	mkdir -p  $(FPGAUTILS_XILINX_RESET_REGS)
	cp -p $(RISCY_HOME)/$(FPGAUTILS_LIB)/DramCommon.bsv      ./$(FPGAUTILS_LIB)/
	cp -p $(RISCY_HOME)/$(FPGAUTILS_LIB)/ResetGuard.bsv      ./$(FPGAUTILS_LIB)/
	cp -p $(RISCY_HOME)/$(FPGAUTILS_LIB)/SyncFifo.bsv        ./$(FPGAUTILS_LIB)/
	cp -p $(RISCY_HOME)/$(FPGAUTILS_LIB)/XilinxFpu.bsv       ./$(FPGAUTILS_LIB)/
	cp -p $(RISCY_HOME)/$(FPGAUTILS_LIB)/XilinxIntDiv.bsv    ./$(FPGAUTILS_LIB)/
	cp -p $(RISCY_HOME)/$(FPGAUTILS_LIB)/XilinxIntMul.bsv    ./$(FPGAUTILS_LIB)/
	cp -p $(RISCY_HOME)/$(FPGAUTILS_LIB)/XilinxSyncFifo.bsv  ./$(FPGAUTILS_LIB)/
	cp -p $(RISCY_HOME)/$(FPGAUTILS_LIB)/WaitAutoReset.bsv   ./$(FPGAUTILS_LIB)/
	cp -p $(RISCY_HOME)/$(FPGAUTILS_XILINX_FPU)/fp_sqrt_sim.v    ./$(FPGAUTILS_XILINX_FPU)/
	cp -p $(RISCY_HOME)/$(FPGAUTILS_XILINX_FPU)/fp_fma_sim.v     ./$(FPGAUTILS_XILINX_FPU)/
	cp -p $(RISCY_HOME)/$(FPGAUTILS_XILINX_FPU)/fp_div_sim.v     ./$(FPGAUTILS_XILINX_FPU)/
	cp -p $(RISCY_HOME)/$(FPGAUTILS_XILINX_RESET_REGS)/reset_guard.v    ./$(FPGAUTILS_XILINX_RESET_REGS)/

CONNECTAL_BSV        = connectal/bsv
CONNECTAL_LIB_BSV    = connectal/lib/bsv
CONNECTAL_TESTS_SPI  = connectal/tests/spi
.PHONY: copy_connectal_files
copy_connectal_files:
	mkdir -p  $(CONNECTAL_BSV)
	mkdir -p  $(CONNECTAL_LIB_BSV)
	mkdir -p  $(CONNECTAL_TESTS_SPI)
	cp -p $(RISCY_HOME)/$(CONNECTAL_BSV)/ConnectalBramFifo.bsv               ./$(CONNECTAL_BSV)/
	cp -p $(RISCY_HOME)/$(CONNECTAL_BSV)/ConnectalClocks.bsv                 ./$(CONNECTAL_BSV)/
	cp -p $(RISCY_HOME)/$(CONNECTAL_LIB_BSV)/Arith.bsv                       ./$(CONNECTAL_LIB_BSV)/
	cp -p $(RISCY_HOME)/$(CONNECTAL_TESTS_SPI)/ConnectalProjectConfig.bsv    ./$(CONNECTAL_TESTS_SPI)/
	tree .
