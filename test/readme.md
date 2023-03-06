## testing

I was going to just give instructions on testing, but gave up halfway through and decided to manually dump a bunch of rv binaries in the repo. Here's how the tests got here:

1. follow the instructions here: [homebrew-riscv](https://github.com/riscv-software-src/homebrew-riscv)
    - you will get a bunch of command-line tools prefixed with `riscv64-`
2. `brew install md5sha1sum` apple moment
3. follow a subset of the instructions here: [riscv-tests](https://github.com/riscv-software-src/riscv-tests) 
   - cd into the folder containing riscv-hs/
   - `git clone https://github.com/riscv/riscv-tests`
   - `cd-riscv-tests`
   - `git submodule update --init --recursive`
   - `autoupdate`
   - `autoconf`
4. `cd isa`
5. Modify the Makefile; new version below.
   - Uncomment more tests once implemented
6. `make && make bins` 
   - optionally `make run` for asm dump files
7. `cp *.bin ../../riscv-hs/test/isa/` 


todo i might delete this bunch of tests

you may have to:
- Modify rv32ui/Makefrag to comment out `rv32ui_v_tests` (for now. virtual memory tests)
- base address is 0x80000000 (this isn't encoded in the raw bin)

### success/failure (todo)

Results are not compared against a reference implementation; Spike (the official emulator downloaded above) does not have an option to print machine state at the end of execution. It has a per-instruction logging feature, but producing and comparing equal logs would probably be more work than writing emulator itself.

Instead, the included tests are designed to evaluate the emulator they're running on. A bit of code is necessary to deal with this.

Success and failure are defined by the following macros, and the emulator (or test harness) should react appropriately. 
```
//-----------------------------------------------------------------------
// End Macro
//-----------------------------------------------------------------------

#define RVTEST_CODE_END                                                 \
        unimp

//-----------------------------------------------------------------------
// Pass/Fail Macro
//-----------------------------------------------------------------------

#define RVTEST_PASS                                                     \
        fence;                                                          \
        li TESTNUM, 1;                                                  \
        li a7, 93;                                                      \
        li a0, 0;                                                       \
        ecall

#define TESTNUM gp
#define RVTEST_FAIL                                                     \
        fence;                                                          \
1:      beqz TESTNUM, 1b;                                               \
        sll TESTNUM, TESTNUM, 1;                                        \
        or TESTNUM, TESTNUM, 1;                                         \
        li a7, 93;                                                      \
        addi a0, TESTNUM, 0;                                            \
        ecall
```

### makefile
``` Makefile
#=======================================================================
# Makefile for riscv-tests/isa
#-----------------------------------------------------------------------

XLEN ?= 64

src_dir := .

ifeq ($(XLEN),64)
include $(src_dir)/rv64ui/Makefrag
include $(src_dir)/rv64uc/Makefrag
include $(src_dir)/rv64um/Makefrag
include $(src_dir)/rv64ua/Makefrag
include $(src_dir)/rv64uf/Makefrag
include $(src_dir)/rv64ud/Makefrag
include $(src_dir)/rv64uzfh/Makefrag
include $(src_dir)/rv64si/Makefrag
include $(src_dir)/rv64ssvnapot/Makefrag
include $(src_dir)/rv64mi/Makefrag
include $(src_dir)/rv64mzicbo/Makefrag
endif
include $(src_dir)/rv32ui/Makefrag
include $(src_dir)/rv32uc/Makefrag
include $(src_dir)/rv32um/Makefrag
include $(src_dir)/rv32ua/Makefrag
include $(src_dir)/rv32uf/Makefrag
include $(src_dir)/rv32ud/Makefrag
include $(src_dir)/rv32uzfh/Makefrag
include $(src_dir)/rv32si/Makefrag
include $(src_dir)/rv32mi/Makefrag

default: all

#--------------------------------------------------------------------
# Build rules
#--------------------------------------------------------------------

RISCV_PREFIX ?= riscv$(XLEN)-unknown-elf-
RISCV_GCC ?= $(RISCV_PREFIX)gcc
RISCV_GCC_OPTS ?= -static -mcmodel=medany -fvisibility=hidden -nostdlib -nostartfiles
RISCV_OBJDUMP ?= $(RISCV_PREFIX)objdump --disassemble-all --disassemble-zeroes --section=.text --section=.text.startup --section=.text.init --section=.data
RISCV_OBJCOPY ?= $(RISCV_PREFIX)objcopy # -O binary rv32ui-p-add rv32ui-p-add.bin
RISCV_SIM ?= spike # --log-commits

vpath %.S $(src_dir)

#------------------------------------------------------------
# Build assembly tests

%.dump: %
	$(RISCV_OBJDUMP) $< > $@

%.bin: %
	$(RISCV_OBJCOPY) -O binary $< $@

%.out: %
	$(RISCV_SIM) --isa=rv64gc_zfh_zicboz_svnapot --misaligned $< 2> $@

%.out32: %
	$(RISCV_SIM) --isa=rv32gc_zfh_zicboz_svnapot --misaligned $< 2> $@

define compile_template

$$($(1)_p_tests): $(1)-p-%: $(1)/%.S
	$$(RISCV_GCC) $(2) $$(RISCV_GCC_OPTS) -I$(src_dir)/../env/p -I$(src_dir)/macros/scalar -T$(src_dir)/../env/p/link.ld $$< -o $$@
$(1)_tests += $$($(1)_p_tests)

$$($(1)_v_tests): $(1)-v-%: $(1)/%.S
	$$(RISCV_GCC) $(2) $$(RISCV_GCC_OPTS) -DENTROPY=0x$$(shell echo \$$@ | md5sum | cut -c 1-7) -std=gnu99 -O2 -I$(src_dir)/../env/v -I$(src_dir)/macros/scalar -T$(src_dir)/../env/v/link.ld $(src_dir)/../env/v/entry.S $(src_dir)/../env/v/*.c $$< -o $$@
$(1)_tests += $$($(1)_v_tests)

$(1)_tests_dump = $$(addsuffix .dump, $$($(1)_tests))

$(1): $$($(1)_tests_dump)

.PHONY: $(1)

COMPILER_SUPPORTS_$(1) := $$(shell $$(RISCV_GCC) $(2) -c -x c /dev/null -o /dev/null 2> /dev/null; echo $$$$?)

ifeq ($$(COMPILER_SUPPORTS_$(1)),0)
tests += $$($(1)_tests)
endif

endef

$(eval $(call compile_template,rv32ui,-march=rv32g -mabi=ilp32))
# $(eval $(call compile_template,rv32uc,-march=rv32g -mabi=ilp32))
# $(eval $(call compile_template,rv32um,-march=rv32g -mabi=ilp32))
# $(eval $(call compile_template,rv32ua,-march=rv32g -mabi=ilp32))
# $(eval $(call compile_template,rv32uf,-march=rv32g -mabi=ilp32))
# $(eval $(call compile_template,rv32ud,-march=rv32g -mabi=ilp32))
# $(eval $(call compile_template,rv32uzfh,-march=rv32g_zfh -mabi=ilp32))
# $(eval $(call compile_template,rv32si,-march=rv32g -mabi=ilp32))
$(eval $(call compile_template,rv32mi,-march=rv32g -mabi=ilp32))
ifeq ($(XLEN),64)
# $(eval $(call compile_template,rv64ui,-march=rv64g -mabi=lp64))
# $(eval $(call compile_template,rv64uc,-march=rv64g -mabi=lp64))
# $(eval $(call compile_template,rv64um,-march=rv64g -mabi=lp64))
# $(eval $(call compile_template,rv64ua,-march=rv64g -mabi=lp64))
# $(eval $(call compile_template,rv64uf,-march=rv64g -mabi=lp64))
# $(eval $(call compile_template,rv64ud,-march=rv64g -mabi=lp64))
# $(eval $(call compile_template,rv64uzfh,-march=rv64g_zfh -mabi=lp64))
# $(eval $(call compile_template,rv64mzicbo,-march=rv64g_zicboz -mabi=lp64))
# $(eval $(call compile_template,rv64si,-march=rv64g -mabi=lp64))
# $(eval $(call compile_template,rv64ssvnapot,-march=rv64g -mabi=lp64))
# $(eval $(call compile_template,rv64mi,-march=rv64g -mabi=lp64))
endif

tests_dump = $(addsuffix .dump, $(tests))
tests_hex = $(addsuffix .hex, $(tests))
tests_bin= $(addsuffix .bin, $(tests))
tests_out = $(addsuffix .out, $(filter rv64%,$(tests)))
tests32_out = $(addsuffix .out32, $(filter rv32%,$(tests)))

run: $(tests_out) $(tests32_out)

bins: $(tests_bin)

junk += $(tests) $(tests_dump) $(tests_bin) $(tests_hex) $(tests_out) $(tests32_out)

#------------------------------------------------------------
# Default

all: $(tests_dump)

#------------------------------------------------------------
# Clean up

clean:
	rm -rf $(junk)

```

### SimpleTest.hs
I've dumped a couple of simple raw binaries into the test/bins directory to exercise the emulator. The SimpleTest test harness iterates through the test programs, executes them and compares the state of the registers to an expected value.

The tests depend on the QuickCheck library  - `cabal install QuickCheck --lib`

The tests can be run with `cabal test`
