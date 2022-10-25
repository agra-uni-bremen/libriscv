package main

import (
	"fmt"
)

type FieldType int8

const (
	Register FieldType = iota
	Immediate
)

type Field struct {
	Type       FieldType
	RawName    string
	RecordName string
	ParserFunc string
}

func fieldType(field string) FieldType {
	switch field {
	case "rs1", "rs2", "rd":
		return Register
	case "imm12", "imm20", "imm12lo", "bimm12lo", "jimm20":
		return Immediate
	default:
		panic(fmt.Sprintf("could not determine type for field: %q", field))
	}
}

func toRecord(field string, t FieldType) string {
	switch t {
	case Register:
		return field
	case Immediate:
		return "imm"
	}

	panic("unreachable")
}

func getParser(field string) string {
	switch field {
	case "rs1":
		return "mkRs1"
	case "rs2":
		return "mkRs2"
	case "rd":
		return "mkRd"
	case "imm12":
		return "immI"
	case "imm20":
		return "immU"
	case "imm12lo":
		return "immS"
	case "bimm12lo":
		return "immB"
	case "jimm20":
		return "immJ"
	default:
		panic(fmt.Sprintf("could not determine parser for field: %q", field))
	}
}

func MakeField(field string) Field {
	t := fieldType(field)

	return Field{
		Type: t,
		RawName: field,
		RecordName: toRecord(field, t),
		ParserFunc: getParser(field),
	}
}
