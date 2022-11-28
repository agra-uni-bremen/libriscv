# libriscv

An extensible implementation of the RISC-V ISA for rapid prototyping of software verification techniques and hardware security mechanisms.

# Status

Currently, the RV32 base instruction set (i.e. `rv32i`) is implemented
and passes the `riscv-tests`.  In terms of extensibility, both the
interpretation of arithmetic expression and abstractions for
architectural state (registers, memory, …) can be replaced presently.

## Installation

This RISC-V simulator can be installed using [Cabal][cabal web] with the following commands:

	$ git clone https://github.com/nmeum/libriscv
	$ cd libriscv
	$ cabal install

This should install the `riscv-tiny` executable to `~/.cabal/bin`.

## Usage

The libriscv cabal package provides an executable (`riscv-tiny`) for
executing rv32i machine code via standard concrete interpretation. Refer
to the `--help` output for more information on this interpreter.
Regarding the provided library, see the Haskell example code provided in
`example/`. The example application provided there demonstrates how
information flow tracking (IFT) can be implemented on top of libriscv.

## Tests

A small unit test suite is available which can be invoked using:

	$ cabal test

Furthermore, a version of [riscv-tests][riscv-tests github] is included
in this repository for performing minimal compliance tests. These tests
require a [riscv-gnu-toolchain][riscv-gnu-toolchain github] as well as
[GNU Make][make web]. If these dependencies are installed, run the tests
using:

	$ ./riscv-tests/run.sh

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
[riscv-tests github]: https://github.com/riscv-software-src/riscv-tests
[riscv-gnu-toolchain github]: https://github.com/riscv-collab/riscv-gnu-toolchain
