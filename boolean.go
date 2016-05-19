package uclisp

var Nil = List{}

var True = trueSymbol("t")

type trueSymbol Symbol

func (t trueSymbol) Equals(ctx *Context, env Env, other Elem) bool {
	return !IsNil(other)
}

func (t trueSymbol) Eval(ctx *Context, env Env) Elem {
	return True
}
