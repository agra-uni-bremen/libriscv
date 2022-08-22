# riscv-tiny

A simulator for the RISC-V ISA which implements a subset of RV32I in Haskell.

# Status

This is just a partial toy implementation of the RISC-V ISA to improve my rusty Haskell knowledge.
The implementation supports a subset of RV32I, the roadmap includes the following instructions:

* [x] ADD
* [x] ADDI
* [x] LW
* [x] SW
* [x] JAL
* [ ] JALR
* [x] BLT
* [x] LUI
* [x] AUIPC

## Installation

This RISC-V simulator can be installed using [Cabal][cabal web] with the following commands:

	$ git clone https://github.com/nmeum/riscv-tiny
	$ cd riscv-tiny
	$ cabal install

This should install the `riscv-tiny` executable to `~/.cabal/bin`.

## Usage:

The `riscv-tiny` executable runs 32-bit RISC-V software until the first invalid instruction.
There are also various command-line options for dumping the register file afterwards and tracing instructions.
Refer to the `--help` output for details.

Example usage:

	$ cat example.S
	.globl _start
	.myword:
		.word 0xffffffff
	_start:
		lw t0, .myword
		addi a0, a0, 1
	$ riscv-none-elf-gcc -o example example.S -march=rv32i -mabi=ilp32 -nostdlib
	$ riscv-tiny --trace --registers example | head
	10078: Auipc T0 0
	1007c: Lw (-4) T0 T0
	10080: Addi 24 T0 T0
	10084: InvalidInstruction
	Zero    = 0
	RA      = 0
	SP      = 0
	GP      = 0
	TP      = 0
	T0      = 23

## Tests

There are currently some minor [doctest][doctest github] tests.
These can be run using the following command:

	$ cabal test

Furthermore, some minimal integration tests are available in `./tests`.
These tests require a GCC cross compiler toolchain and can be invoked using:

	$ ./tests/run.sh

## License

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
Public License for more details.

You should have received a copy of the GNU General Public License along
with this program. If not, see <https://www.gnu.org/licenses/>.

[doctest github]: https://github.com/sol/doctest-haskell
[cabal web]: https://www.haskell.org/cabal/
