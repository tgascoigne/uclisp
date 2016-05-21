package uclisp

import "log"

func init() {
	Builtin.Define("backtrace", NewProcedure(backtraceForm))
	Builtin.Define("eval", NewProcedure(evalForm))
}

func backtraceForm(ctx *Context, env Env, args []Elem) Elem {
	dump := ctx.CallStack.Dump(BacktraceDepth)
	log.Print(dump)
	return Nil
}

func evalForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) != 1 {
		ctx.Raise(ErrArgCount, len(args), 1)
	}

	return ctx.Eval(ctx.Eval(args[0], env), env)
}
