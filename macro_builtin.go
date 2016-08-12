package uclisp

import "log"

func init() {
	Builtin.Define("macro", NewProcedure(macroForm))
}

// todo: macroexpand

func macroForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) < 2 {
		ctx.Raise(ErrArgCount, len(args))
	}

	argSpec, err := AssertList(args[0])
	if err != nil {
		ctx.Raise(err, args[0])
	}

	argSpecSymbols := make([]Symbol, len(argSpec))
	for i := range argSpec {
		argSpecSymbols[i], err = AssertSymbol(argSpec[i])
		if err != nil {
			ctx.Raise(err, argSpec[i])
		}
	}

	bindings := parseArgSpec(ctx, argSpecSymbols)

	body := append(List{Symbol("progn")}, args[1:]...)

	return NewProcedure(func(ctx *Context, callerEnv Env, args []Elem) Elem {
		// Macros are implemented similarly to lambda.
		// however, instead of evaluating arguments and then binding, we just
		// bind exactly what's passed in.
		//
		// We then evaluate the body within this binding, with the expectation that
		// the body returns a s-expr which represents the expanded macro
		//
		// Finally, this expanded macro is evaluated in the caller's environment.

		bound := bindings.Bind(ctx, callerEnv, args)
		expanded := ctx.Eval(body, bound)

		if Global.Defined(ctx, Symbol("*macro-debug*")) {
			log.Printf("expanded to %v\n", expanded)
		}

		return ctx.Eval(expanded, callerEnv)
	})
}
