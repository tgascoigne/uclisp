package uclisp

import "fmt"

func init() {
	Builtin.Define("macro", Procedure(macroForm))
}

// todo: macroexpand

func macroForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) < 1 {
		Raise(ErrArgCount, len(args))
	}

	argSpec, err := AssertList(args[0])
	if err != nil {
		Raise(err, args[0])
	}

	argSpecSymbols := make([]Symbol, len(argSpec))
	for i := range argSpec {
		argSpecSymbols[i], err = AssertSymbol(argSpec[i])
		if err != nil {
			Raise(err, argSpec[i])
		}
	}

	bindings := parseArgSpec(argSpecSymbols)

	body := append(List{Symbol("progn")}, args[1:]...)

	return Procedure(func(ctx *Context, callerEnv Env, args []Elem) Elem {
		// Macros are implemented similarly to lambda.
		// however, instead of evaluating arguments and then binding, we just
		// bind exactly what's passed in.
		//
		// We then evaluate the body within this binding, with the expectation that
		// the body returns a s-expr which represents the expanded macro
		//
		// Finally, this expanded macro is evaluated in the caller's environment.

		bound := bindings.Bind(callerEnv, args)
		expanded := ctx.Eval(body, bound)

		if Global.Defined(Symbol("*macro-debug*")) {
			fmt.Printf("expanded to %v\n", expanded)
		}

		return ctx.Eval(expanded, callerEnv)
	})
}
