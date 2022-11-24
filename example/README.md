# Example: riscv-ift

This example implements ISA-level information flow tracking.
That is, it tracks an initially tainted value across memory and registers and at the end prints all registers which depend on this initially tainted value.

## Usage

Compile the example RISC-V program using either `clang` or `gcc`:

     $ clang --target=-riscv32-unknown-elf -march=rv32i -mabi=ilp32 -nostdlib -o example example.S

Afterwards, execute the program using `riscv-ift`:

    $ cabal run riscv-ift -- --taint-register A0 -r example | grep tainted
    A0      = 0xfffffffb (tainted)
    A1      = 0xfffffffb (tainted)
    A2      = 0x12 (tainted)

In the invocation above, we initially taint register `A0`.
After execution has finished, register `A1` and `A2` are tainted as well which tells us that they depend (directly or indirectly) on register `A1`.

## Limitations

It is currently not possible to taint memory addresses via the command line interface.
Furthermore, the program counter cannot be tainted presently.
