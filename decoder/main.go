package main

import (
)

func main() {
	err := GenDecoder("instr_dict.yaml", "decoder.hs")
	if err != nil {
		panic(err)
	}
}
