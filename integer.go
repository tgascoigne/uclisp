package uclisp

// Integer is an int.
type Integer int

func (i Integer) Equals(ctx *Context, env Env, o Elem) bool {
	other, err := AssertInteger(o)
	if err != nil {
		ctx.Raise(err, other)
	}

	return i == other
}

func (i Integer) Eval(ctx *Context, env Env) Elem {
	return i
}
