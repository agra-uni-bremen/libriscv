# A Versatile and Flexibel RISC-V Model

This is an extensible [RISC-V][riscv web] implementation which is based on a formal model of the architecture.
The formal model is implemented using [free monads][free monads] and a custom expression language for arithmetic operations.
Based on this formal model, different interpretation of RISC-V instructions can be implemented.
As such, `libriscv` is a versatile tool for prototyping software analysis tools (e.g. [symbolic execution][symex wp]) or [hardware-based security mechanisms][hardbound paper].

## Status

Currently, the RV32 base instruction set (i.e. `rv32i`) is implemented and passes the [riscv-tests][riscv-tests github].
Apart from the formal model for `rv32i`, a minimal concrete interpreter is also implemented.

## Features

* Abstract description of RISC-V instructions based on free monads
* Custom expression language to perform operations on arbitrary types
    * Instruction operands must not be concrete fixed-width integers
    * Can also be SMT expressions for example (useful for symbolic execution)
* Polymorphic implementations of byte-addressable memory and register file
* Algebraic effects for instruction tracing etc

## Installation

This software can be installed either using [Cabal][cabal web] or [Docker][docker web].
The latter installation method may be preferable if you don't have a RISC-V cross compiler installed on your system.
Both methods are described further below under the assumption that the repository has been cloned already.

### Cabal

Assuming cabal has already been setup, run the following command within the source directory to install `libriscv`:

    $ cabal install

This should place the concrete example interpreter (`riscv-tiny`) in your `$PATH`.

### Docker

To ease installation of a RISC-V cross compiler toolchain a Dockerfile is provided.
To build and run a Docker container using this file invoke the following commands:

    $ docker build -t libriscv .
    $ docker run -it libriscv

This will drop you in an interactive shell within the Docker container.
See the section below for more information on which commands can be run within the container.
Within the container, the `libriscv` source code is available in the `libriscv` subdirectory.

## Usage

This section provides more information on using the provided library and the concrete interpreter.

### The Concrete Interpreter

The concrete interpreter, which is build upon the formal RISC-V model, can execute any `rv32i` RISC-V machine code.
Assuming you have a RISC-V compiler toolchain available, run the following commands to execute a sample assembly program:

    $ cat sample.S
    .globl _start
    .myword:
        .word 0xffffffff
    _start:
        lw t0, .myword
        addi a0, t0, -1

        # Invalid instruction to cause riscv-tiny exit
        .word 0xffffffff
    $ clang --target=-riscv32-unknown-elf -march=rv32i -mabi=ilp32 -nostdlib -o sample sample.S
    $ riscv-tiny --trace --registers sample

The sample program loads a value from memory and then decrements this value by 1.
The `riscv-tiny` invocation will then print all instructions executed for this program and exits after encountering the first invalid instruction.
Afterwards, it will dump all register values.

### The Library Interface

The concreter interpreter implements just one possible interpretation of RISC-V instructions.
As the name suggests, `libriscv` is specifically intended to implement custom interpreters on top of the abstract RISC-V model.
For this purpose, the Cabal file provides a Haskell library component.
Based on this library, the `example/` subdirectory contains a custom example interpreter which performs [dynamic information flow tracking][dift paper] on RISC-V machine code.
Refer to `example/README.md` for more information.

## Tests

A unit test suite is available which can be invoked using:

	$ cabal test

Furthermore, a version of [riscv-tests][riscv-tests github] is included in this repository for performing minimal compliance tests.
These tests require a [riscv-gnu-toolchain][riscv-gnu-toolchain github] as well as [GNU Make][make web].
If these dependencies are installed (e.g. if you are using the provided Docker image), run the tests using:

	$ ./riscv-tests/run.sh

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

[riscv web]: https://riscv.org
[cabal web]: https://www.haskell.org/cabal/
[make web]: https://www.gnu.org/software/make
[riscv-tests github]: https://github.com/riscv-software-src/riscv-tests
[riscv-gnu-toolchain github]: https://github.com/riscv-collab/riscv-gnu-toolchain
[docker web]: https://www.docker.io
[free monads]: https://doi.org/10.1145/2887747.2804319
[symex wp]: https://en.wikipedia.org/wiki/Symbolic_execution
[hardbound paper]: https://doi.org/10.1145/1353535.1346295
[dift paper]: https://doi.org/10.1145/1024393.1024404
