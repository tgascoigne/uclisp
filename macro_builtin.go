package uclisp

func init() {
	Builtin.Define("macro", Procedure(macroForm))
}

func macroForm(env Env, args []Elem) Elem {
	if len(args) < 1 {
		Raise(ErrArgCount, len(args))
	}

	bindSpec, err := AssertList(args[0])
	if err != nil {
		Raise(err)
	}

	bindings := make(argumentBinding, 0)
	for _, el := range bindSpec {
		sym, err := AssertSymbol(el)
		if err != nil {
			Raise(err)
		}

		bindings = append(bindings, sym)
	}

	body := append(List{Symbol("progn")}, args[1:]...)

	return Procedure(func(callerEnv Env, args []Elem) Elem {
		args = args[:] // copy original list

		bound := bindings.Bind(env, args)
		expanded := body.Eval(bound)
		return expanded.Eval(callerEnv)
	})
}
