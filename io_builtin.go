package uclisp

import (
	"fmt"
	"io/ioutil"
	"log"
)

func init() {
	Builtin.Define("load-file", Procedure(loadfileForm))
	Builtin.Define("printf", Procedure(printfForm))
}

func printfForm(env Env, args []Elem) Elem {
	if len(args) < 1 {
		Raise(ErrArgCount, len(args))
	}

	format, err := AssertString(args[0])
	if err != nil {
		Raise(err)
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

	path, err := AssertString(args[0].Eval(parentEnv))
	if err != nil {
		Raise(err)
	}

	//todo: better logging
	fmt.Printf("load-file: %v\n", path)

	data, err := ioutil.ReadFile(string(path))
	if err != nil {
		Raise(err)
	}

	env := NewBasicEnv(parentEnv)
	prog := Parse(string(path), string(data))
	return prog.Eval(env)
}
