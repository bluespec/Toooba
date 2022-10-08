# Makefile to create an elf_to_hex executable.
# The executable creates mem-hex files containing 32-Byte words

CC = gcc

# the LIBERARY path needs to be set for Apple devices, e.g.,
# LIBRARY_PATH="$LIBRARY_PATH:$(brew --prefix)/lib"

uname_S := $(shell uname -s)

CFLAGS = -g  -o elf_to_hex  elf_to_hex.c -lelf

ifeq ($(uname_S), Darwin)
    CC = clang++
	APPLE_FLAGS = -I /opt/homebrew/lib -I /opt/homebrew/Cellar/libelf/0.8.13_1/include/
endif

elf_to_hex: elf_to_hex.c
	$(CC) $(CFLAGS) $(APPLE_FLAGS)

# ================================================================

.PHONY: clean
clean:
	rm  -f  *~

.PHONY: full_clean
full_clean:
	rm  -f  *~  elf_to_hex