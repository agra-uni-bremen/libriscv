# riscv-tiny

A simulator for RISC-V implementing a subset of rv32i in Haskell.

# Status

This is just a toy project for partially re-activating my Haskell knowledge.
It implements a subset of RV32I, the roadmap includes the following instructions:

* [x] ADD
* [x] ADDI
* [x] LW
* [ ] SW
* [ ] JAL
* [ ] JALR
* [ ] BLT
* [ ] LUI
* [ ] AUIPC

## Installation

This RISC-V simulator can be installed using [Cabal][cabal web] with the following commands:

	$ git clone https://github.com/nmeum/riscv-tiny
	$ cd riscv-tiny
	$ cabal install

This should install the `riscv-tiny` executable to `~/.cabal/bin`.

## Tests

There are currently some minor [doctest][doctest github] tests.
These can be run using the following command:

	$ cabal test

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
