package uclisp

import (
	"fmt"
	"strings"
)

func init() {
	Builtin.Define("concat", Procedure(concatForm))
}

func concatForm(env Env, args []Elem) Elem {
	list := make([]string, len(args))
	for i := range args {
		list[i] = fmt.Sprintf("%v", args[i].Eval(env))
	}

	return String(strings.Join(list, ""))
}
