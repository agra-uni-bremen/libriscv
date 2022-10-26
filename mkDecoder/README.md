# Decoder Generation

Tool for generating a Haskell RISC-V instruction decoder from [riscv-opcodes](https://github.com/riscv/riscv-opcodes).

## Usage

This tool (partially) generates a RISC-V instruction decoder in Haskell from the YAML specification provided by [riscv-opcodes](https://github.com/riscv/riscv-opcodes).
The utilized YAML file is tracked in this directory as `instr_dict.yaml`.
Furthermore, this directory contains a [Go](https://golang.org) tool which generates Haskell code using the [Go template language](https://golang.org/pkg/text/template).

In order to utilize the tool run the following commands:

    $ cd mkDecoder/
    $ go build
    $ ./mkDecoder instr_dict.yaml ../src/Decoder.hs

These commands will write the Haskell decoder implementation to `../src/Decoder.hs`.

## Limitations

The tool implementation is a bit hacky and currently uses string concatenation and the Go template language to generate Haskell code.
Ideally one would create an [unparser](https://en.wikipedia.org/wiki/Unparser) and create a Haskell AST from the riscv-opcodes YAML.
Furthermore, the immediate and register parsing functions are not generated from the YAML.
This could be changed in the future, an issue in this regard is though that the YAML is rather low-level and for instance distinguishes the low and high bits for certain immediates.
