package uclisp

func init() {
	Builtin.Define("stringp", Procedure(stringpForm))
	Builtin.Define("symbolp", Procedure(symbolpForm))
	Builtin.Define("listp", Procedure(listpForm))
	Builtin.Define("integerp", Procedure(integerpForm))
	Builtin.Define("procedurep", Procedure(procedurepForm))
}

func stringpForm(env Env, args []Elem) Elem {
	if len(args) != 1 {
		Raise(ErrArgCount, len(args))
	}

	if _, err := AssertString(Eval(args[0], env)); err != nil {
		return Nil
	}

	return True
}

func symbolpForm(env Env, args []Elem) Elem {
	if len(args) != 1 {
		Raise(ErrArgCount, len(args))
	}

	if _, err := AssertSymbol(Eval(args[0], env)); err != nil {
		return Nil
	}

	return True
}

func listpForm(env Env, args []Elem) Elem {
	if len(args) != 1 {
		Raise(ErrArgCount, len(args))
	}

	if _, err := AssertList(Eval(args[0], env)); err != nil {
		return Nil
	}

	return True
}

func integerpForm(env Env, args []Elem) Elem {
	if len(args) != 1 {
		Raise(ErrArgCount, len(args))
	}

	if _, err := AssertInteger(Eval(args[0], env)); err != nil {
		return Nil
	}

	return True
}

func procedurepForm(env Env, args []Elem) Elem {
	if len(args) != 1 {
		Raise(ErrArgCount, len(args))
	}

	if _, err := AssertProcedure(Eval(args[0], env)); err != nil {
		return Nil
	}

	return True
}
