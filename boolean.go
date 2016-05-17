package uclisp

var Nil = List{}

var True = trueSymbol("t")

type trueSymbol Symbol

func (t trueSymbol) Equals(env Env, other Elem) bool {
	return other == True
}

func (t trueSymbol) Eval(ctx *Context, env Env) Elem {
	return True
}
