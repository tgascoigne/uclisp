package uclisp

import "log"

func init() {
	Builtin.Define("backtrace", Procedure(backtraceForm))
	Builtin.Define("eval", Procedure(evalForm))
}

func backtraceForm(env Env, args []Elem) Elem {
	dump := callStack.Dump(BacktraceDepth)
	log.Print(dump)
	return Nil
}

func evalForm(env Env, args []Elem) Elem {
	if len(args) != 1 {
		Raise(ErrArgCount, len(args), 1)
	}

	return Eval(Eval(args[0], env), env)
}