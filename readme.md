# riscv-hs

A RISC-V emulator written in Haskell. Currently supports the [RV32I ](https://riscv.org/wp-content/uploads/2017/05/riscv-spec-v2.2.pdf) (32-bit integer) unprivileged ISA.

Common extensions such as M (multiplication) and C (compressed two-byte instructions) may be added later.

## Run the simulator

`cabal build && cabal run` will run a small example program. 

Alternatively, provide an ELF with `cabal run riscv-hs -- <filepath>`. Not all ELF features are supported; I've done my best to raise an error if something unsupported is detected.


## Compiling input programs

```riscv64-unknown-elf-gcc -O2 -march=rv32i -mabi=ilp32 <program>.c -o <program>```

You will need the riscv toolchain installed. For mac, this can be found here: [homebrew-riscv](https://github.com/riscv-software-src/homebrew-riscv)

Some test programs (compiled and not) are found in [test/c](./test/c). You can find out what the compiler does with no multiplication instruction in [test/c/mul](./test/c/mul): `cabal run riscv-hs -- /test/c/mul`

See the [disaster in /test](./test) for other things we tried and other possible program inputs.

## Current implementation quirks

- The program counter starts at 0x0 unless an ELF (with entry point) is provided
- The stack pointer starts at 0x0 (the first item pushed will appear at the highest available address)
- ECALL and any undefined instructions according to the RV32I spec are treated as halt instructions 
  - By their design, any instruction with all bits set to zero is undefined. Accessing uninitialized memory will always produce zero (by our design).
