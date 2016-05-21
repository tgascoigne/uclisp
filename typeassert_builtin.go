package uclisp

func init() {
	Builtin.Define("stringp", NewProcedure(stringpForm))
	Builtin.Define("symbolp", NewProcedure(symbolpForm))
	Builtin.Define("listp", NewProcedure(listpForm))
	Builtin.Define("integerp", NewProcedure(integerpForm))
	Builtin.Define("procedurep", NewProcedure(procedurepForm))
}

func stringpForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) != 1 {
		ctx.Raise(ErrArgCount, len(args))
	}

	if _, err := AssertString(ctx.Eval(args[0], env)); err != nil {
		return Nil
	}

	return True
}

func symbolpForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) != 1 {
		ctx.Raise(ErrArgCount, len(args))
	}

	if _, err := AssertSymbol(ctx.Eval(args[0], env)); err != nil {
		return Nil
	}

	return True
}

func listpForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) != 1 {
		ctx.Raise(ErrArgCount, len(args))
	}

	if _, err := AssertList(ctx.Eval(args[0], env)); err != nil {
		return Nil
	}

	return True
}

func integerpForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) != 1 {
		ctx.Raise(ErrArgCount, len(args))
	}

	if _, err := AssertInteger(ctx.Eval(args[0], env)); err != nil {
		return Nil
	}

	return True
}

func procedurepForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) != 1 {
		ctx.Raise(ErrArgCount, len(args))
	}

	if _, err := AssertProcedure(ctx.Eval(args[0], env)); err != nil {
		return Nil
	}

	return True
}
