package main

import (
	"os"
	"fmt"
	"strings"
	"path/filepath"
	"text/template"
)

type GeneratorInput struct {
	InstType string
	Instrs   Instructions
}

// Creates Haskell type constructor identifier from given string.
func makeId(name string) string {
	return strings.ToUpper(name)
}

func formatFields(fields []Field) string {
	var recFields []string
	for _, field := range fields {
		var t string
		switch field.Type {
		case Register:
			t = "RegIdx"
		case Immediate:
			t = "Immediate"
		}

		recField := fmt.Sprintf("%s :: %s", field.RecordName, t)
		recFields = append(recFields, recField)
	}

	return strings.Join(recFields, ", ")
}

func makeRecord(name string, inst Instruction) (string, error) {
	instFields, err := inst.Fields()
	if err != nil {
		return "", err
	}

	id := makeId(name)
	if len(instFields) == 0 {
		return id, nil
	} else {
		return fmt.Sprintf("%s { %s }", id, formatFields(instFields)), nil
	}
}

func makeConstructor(name string, inst Instruction) (string, error) {
	const instParam = "instrWord"

	instFields, err := inst.Fields()
	if err != nil {
		return "", err
	}

	var assigns []string
	for _, field := range instFields {
		fnCall := fmt.Sprintf("%s %s", field.ParserFunc, instParam)
		assign := fmt.Sprintf("%s=%s", field.RecordName, fnCall)
		assigns = append(assigns, assign)
	}

	constructor := fmt.Sprintf("%s { %s }", makeId(name), strings.Join(assigns, ", "))
	return constructor, nil
}

func getTmpl(name string) (*template.Template, error) {
	tmpl := template.New(name)
	funcMap := template.FuncMap{
		"makeRecord": makeRecord,
		"makeConstructor": makeConstructor,
	}
	tmpl = tmpl.Funcs(funcMap)

	fp := filepath.Join(filepath.Dir(os.Args[0]), name)
	tmpl, err := tmpl.ParseFiles(fp)
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
