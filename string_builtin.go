package uclisp

import (
	"fmt"
	"strings"
)

func init() {
	Builtin.Define("concat", NewProcedure(concatForm))
}

func concatForm(ctx *Context, env Env, args []Elem) Elem {
	list := make([]string, len(args))
	for i := range args {
		list[i] = fmt.Sprintf("%v", ctx.Eval(args[i], env))
	}

	return String(strings.Join(list, ""))
}
