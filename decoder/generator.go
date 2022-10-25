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

func toIdentifier(name string) string {
	return strings.ToUpper(name)
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
	identifier := toIdentifier(name)
	if len(inst.Fields) == 0 {
		return identifier
	} else {
		return fmt.Sprintf("%s { %s }", identifier, formatFields(inst.Fields))
	}
}

func makeConstructor(name string, inst Instruction) string {
	var assigns []string
	for _, field := range inst.Fields {
		// Ignore high immediates and only handle the low ones.
		//
		// Assumption: For every high immediate field there is an
		// equally sized low immediate field in `variable_fields`.
		if strings.HasSuffix(field, "hi") {
			continue
		}

		var assign string
		switch field {
		case "rs1":
			assign = "rs1=mkRs1"
		case "rs2":
			assign = "rs2=mkRs2"
		case "rd":
			assign = "rd=mkRd"
		case "imm12":
			assign = "imm=immI"
		case "imm20":
			assign = "imm=immU"
		case "imm12lo":
			assign = "imm=immS"
		case "bimm12lo":
			assign = "imm=immB"
		case "jimm20":
			assign = "imm=immJ"
		default:
			panic(fmt.Sprintf("unknown field name: %q", field))
		}

		assign += " instrWord"
		assigns = append(assigns, assign)
	}

	return fmt.Sprintf("%s { %s }", toIdentifier(name), strings.Join(assigns, ", "))
}

func getTmpl(name string) (*template.Template, error) {
	tmpl := template.New(name)
	funcMap := template.FuncMap{
		"makeRecord": makeRecord,
		"makeConstructor": makeConstructor,
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
