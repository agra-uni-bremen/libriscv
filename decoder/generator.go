package main

import (
	"os"
	"fmt"
	"strings"
	"text/template"
)

type GeneratorInput struct {
	InstType string
	Instrs   Instructions
}

func fieldType(field string) string {
	// TODO
	if field == "rs1" || field == "rs2" || field == "rd" {
		return "RegIdx"
	} else {
		return "Immediate"
	}
}

func formatFields(fields []string) string {
	var recordFields []string
	for _, field := range fields {
		// Ignore high immediates and only handle the low ones.
		//
		// Assumption: For every high immediate field there is an
		// equally sized low immediate field in `variable_fields`.
		if strings.HasSuffix(field, "hi") {
			continue
		}

		// Use the same record name for all immediate types.
		if strings.Contains(field, "imm") {
			field = "imm"
		}

		field := fmt.Sprintf("%s :: %s", field, fieldType(field))
		recordFields = append(recordFields, field)
	}

	return strings.Join(recordFields, ", ")
}

func makeRecord(name string, inst Instruction) string {
	identifier := strings.ToUpper(name)
	if len(inst.Fields) == 0 {
		return identifier
	} else {
		return fmt.Sprintf("%s { %s }", identifier, formatFields(inst.Fields))
	}
}

func getTmpl(name string) (*template.Template, error) {
	tmpl := template.New(name)
	funcMap := template.FuncMap{
		"makeRecord": makeRecord,
	}
	tmpl = tmpl.Funcs(funcMap)

	tmpl, err := tmpl.ParseFiles(name)
	if err != nil {
		return nil, err
	}

	return tmpl, nil
}

func GenDecoder(instYaml string, outFp string) error {
	const tmplName = "decoder.tmpl"
	tmpl, err := getTmpl(tmplName)
	if err != nil {
		return err
	}

	instrs, err := ParseInstrs(instYaml)
	if err != nil {
		return err
	}

	input := GeneratorInput{
		InstType: "InstructionType",
		Instrs:   instrs,
	}

	out, err := os.Create(outFp)
	if err != nil {
		return err
	}
	defer out.Close()

	return tmpl.Execute(out, input)
}
