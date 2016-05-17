package uclisp

func init() {
	Builtin.Define("stringp", Procedure(stringpForm))
	Builtin.Define("symbolp", Procedure(symbolpForm))
	Builtin.Define("listp", Procedure(listpForm))
	Builtin.Define("integerp", Procedure(integerpForm))
	Builtin.Define("procedurep", Procedure(procedurepForm))
}

func stringpForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) != 1 {
		Raise(ErrArgCount, len(args))
	}

	if _, err := AssertString(ctx.Eval(args[0], env)); err != nil {
		return Nil
	}

	return True
}

func symbolpForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) != 1 {
		Raise(ErrArgCount, len(args))
	}

	if _, err := AssertSymbol(ctx.Eval(args[0], env)); err != nil {
		return Nil
	}

	return True
}

func listpForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) != 1 {
		Raise(ErrArgCount, len(args))
	}

	if _, err := AssertList(ctx.Eval(args[0], env)); err != nil {
		return Nil
	}

	return True
}

func integerpForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) != 1 {
		Raise(ErrArgCount, len(args))
	}

	if _, err := AssertInteger(ctx.Eval(args[0], env)); err != nil {
		return Nil
	}

	return True
}

func procedurepForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) != 1 {
		Raise(ErrArgCount, len(args))
	}

	if _, err := AssertProcedure(ctx.Eval(args[0], env)); err != nil {
		return Nil
	}

	return True
}
