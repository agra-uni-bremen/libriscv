# libriscv

An extensible implementation of the RISC-V ISA for rapid prototyping of software verification techniques and hardware security mechanisms.

# Status

The implementation presently supports a subset of RV32I.
The following instructions are supported at the moment, more instructions will be added later:

* [x] ADD
* [x] ADDI
* [x] LW
* [x] SW
* [x] JAL
* [x] JALR
* [x] BLT
* [x] LUI
* [x] AUIPC

## Installation

This RISC-V simulator can be installed using [Cabal][cabal web] with the following commands:

	$ git clone https://github.com/nmeum/libriscv
	$ cd libriscv
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

There are currently some minor unit tests.

	$ cabal test

Furthermore, some minimal integration tests are available in `./golden`.
These tests require `riscv-tiny` in `$PATH` and the following software:

* [GNU Make][make web]
* [Clang][clang web] with rv32i support

If these dependencies are installed, run the golden tests using:

	$ ./golden/run.sh

## Related Work

There are several "formal models" of the RISC-V ISA many of them are
written in Haskell and also allow interpretation of RISC-V machine code:

* https://github.com/GaloisInc/grift
* https://github.com/mit-plv/riscv-semantics/
* https://github.com/rsnikhil/Forvis_RISCV-ISA-Spec
* https://github.com/sifive/RiscvSpecFormal

A survey of different "formal models" was conducted by RISC-V working
group in 2019. As the result of this survey, the formal model provided
by [SAIL](https://github.com/rems-project/sail/) was established
as the "golden formal model" for the RISC-V architecture.

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

[cabal web]: https://www.haskell.org/cabal/
[make web]: https://www.gnu.org/software/make
[clang web]: https://clang.llvm.org/
