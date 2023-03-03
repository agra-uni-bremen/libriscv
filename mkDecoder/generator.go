package main

import (
	"os"
	"path/filepath"
	"strings"
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

func makeRecord(name string, inst Instruction) string {
	return makeId(name)
}

func makeConstructor(name string, inst Instruction) string {
	return makeRecord(name, inst)
}

func getTmpl(name string) (*template.Template, error) {
	tmpl := template.New(name)
	funcMap := template.FuncMap{
		"makeRecord":      makeRecord,
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
