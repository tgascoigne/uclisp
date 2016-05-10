package uclisp

import (
	"fmt"
	"log"
)

func init() {
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
