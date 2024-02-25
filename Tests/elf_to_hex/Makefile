# Makefile to create an elf_to_hex executable.
# The executable creates mem-hex files containing 32-Byte words

# ----------------
# On Apple MacOS
ifeq ($(shell uname -s),Darwin)

# For include <gelf.h>, after 'brew install libelf'
CFLAGS += -I /opt/homebrew/include
CFLAGS += -I /opt/homebrew/include/libelf/

# For libelf.a on MacOS
LDFLAGS += -L /opt/homebrew/lib

endif
# ----------------

LDLIBS = -lelf -lm

elf_to_hex: elf_to_hex.c
	$(CC) $(CFLAGS) -o  elf_to_hex  elf_to_hex.c  $(LDFLAGS) $(LDLIBS)

# ================================================================

.PHONY: clean
clean:
	rm  -f  *~

.PHONY: full_clean
full_clean:
	rm  -f  *~  elf_to_hex
