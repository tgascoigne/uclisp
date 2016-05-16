package uclisp

import (
	"fmt"
	"io/ioutil"
	"log"
)

const (
	SymLoadFileName = Symbol("*load-file-path*")
)

func init() {
	Builtin.Define("load-file", Procedure(loadfileForm))
	Builtin.Define("message", Procedure(messageForm))
}

func messageForm(env Env, args []Elem) Elem {
	if len(args) < 1 {
		Raise(ErrArgCount, len(args))
	}

	format, err := AssertString(args[0])
	if err != nil {
		Raise(err, args[0])
	}

	args = args[1:]
	argsIface := make([]interface{}, len(args))
	for i := range args {
		// arguments are evaluated in the caller's environment
		argsIface[i] = args[i].Eval(env)
	}

	out := fmt.Sprintf(string(format), argsIface...)
	log.Println(out)
	return String(out)
}

func loadfileForm(parentEnv Env, args []Elem) Elem {
	if len(args) != 1 {
		Raise(ErrArgCount, len(args))
	}

	pathElem := args[0].Eval(parentEnv)
	path, err := AssertString(pathElem)
	if err != nil {
		Raise(err, pathElem)
	}

	//todo: better logging
	fmt.Printf("load-file: %v\n", path)

	data, err := ioutil.ReadFile(string(path))
	if err != nil {
		Raise(err, path)
	}

	env := NewBasicEnv(parentEnv)
	env.Define(SymLoadFileName, path)
	prog := Parse(string(path), string(data))
	return prog.Eval(env)
}
