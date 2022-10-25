package main

import (
	"fmt"
	"os"
	"path/filepath"
)

func main() {
	if len(os.Args) <= 2 {
		fmt.Fprintf(os.Stderr, "USAGE: %s INSTR_YAML PATH\n", filepath.Base(os.Args[0]))
		os.Exit(1)
	}

	ymlFp := os.Args[1]
	outFp := os.Args[2]

	err := GenDecoder(ymlFp, outFp)
	if err != nil {
		panic(err)
	}
}
