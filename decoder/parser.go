package main

import (
	"os"
	"fmt"
	"gopkg.in/yaml.v3"
)

type Instruction struct {
	Encoding  string   `yaml:"encoding"`
	Extension []string `yaml:"extension"`
	RawMask   string   `yaml:"mask"`
	RawMatch  string   `yaml:"match"`
	Fields    []string `yaml:"variable_fields"`
}

type Instructions map[string]Instruction

func (i Instruction) Mask() (uint, error) {
	var n uint
	_, err := fmt.Sscanf(i.RawMask, "0x%x", &n)
	if err != nil {
		return 0, err
	}

	return n, nil
}

func (i Instruction) Match() (uint, error) {
	var n uint
	_, err := fmt.Sscanf(i.RawMatch, "0x%x", &n)
	if err != nil {
		return 0, err
	}

	return n, nil
}

func ParseInstrs(fp string) (Instructions, error) {
	var m map[string]Instruction

	file, err := os.Open(fp)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	dec := yaml.NewDecoder(file)
	err = dec.Decode(&m)
	if err != nil {
		return nil, err
	}

	return m, nil
}
