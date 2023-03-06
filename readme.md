# riscv-hs

A RISC-V emulator written in Haskell.

Currently supports the RV32I unprivileged ISA.
todo: Probably adding M extension too since we're not hardware engineers and have no excuse


## Run the simulator

`cabal build && cabal run` will run a small example program. 

Alternatively, provide an ELF with `cabal run riscv-hs -- <filepath>`. Not all ELF features are supported; I've done my best to raise an error if something unsupported is detected.


## Compiling input programs

```riscv64-unknown-elf-gcc -O2 -march=rv32i -mabi=ilp32 <program>.c -o <program>```

You will need the riscv toolchain installed. For mac, this can be found here: [homebrew-riscv](https://github.com/riscv-software-src/homebrew-riscv)

