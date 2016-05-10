package uclisp

func init() {
	Builtin.Set("lambda", Procedure(lambdaForm))
}

type argumentBinding []Symbol

func (symbols argumentBinding) Bind(env Env, args []Elem) Env {
	bound := NewBasicEnv(env)
	if len(args) != len(symbols) {
		Raise(ErrArgCount, len(args), len(symbols))
	}

	for i, sym := range symbols {
		bound.Set(sym, args[i])
	}

	return bound
}

func lambdaForm(env Env, args []Elem) Elem {
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
		for i := range args {
			// arguments are evaluated in the caller's environment
			args[i] = args[i].Eval(callerEnv)
		}

		// ... but the body of the function is evaluated in the environment at the point
		// of declaration.
		// Roughly emulates lexical/dynamic binding
		bound := bindings.Bind(env, args)
		return body.Eval(bound)
	})
}
