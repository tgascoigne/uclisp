package uclisp

import "errors"

var ErrInvalidBindSpec = errors.New("invalid bind specification: %v")

func init() {
	Builtin.Define("define", Procedure(defineForm))
	Builtin.Define("defined", Procedure(definedForm))
	Builtin.Define("let", genLetForms(false))
	Builtin.Define("let*", genLetForms(true))
	Builtin.Define("set", Procedure(setForm))
}

func genLetForms(starform bool) Procedure {
	return Procedure(func(env Env, args []Elem) Elem {
		if len(args) < 1 {
			Raise(ErrArgCount, len(args))
		}

		bound := NewBasicEnv(env)

		bindSpec, err := AssertList(args[0])
		if err != nil {
			Raise(err)
		}

		for _, el := range bindSpec {
			if bindList, err := AssertList(el); err == nil {
				if len(bindList) != 2 {
					Raise(ErrInvalidBindSpec, bindSpec)
				}

				sym, err := AssertSymbol(bindList[0])
				if err != nil {
					Raise(err)
				}

				initial := bindList[1]

				if starform {
					// let* can reference earlier bindings
					bound.Define(sym, initial.Eval(bound))
				} else {
					bound.Define(sym, initial.Eval(env))
				}

				continue
			}

			if sym, err := AssertSymbol(el); err == nil {
				bound.Define(sym, Nil)
			} else {
				Raise(err)
			}
		}

		body := append(List{Symbol("progn")}, args[1:]...)
		return body.Eval(bound)
	})
}

func defineForm(env Env, args []Elem) Elem {
	if len(args) < 2 {
		Raise(ErrArgCount, len(args), 2)
	}

	symbol, err := AssertSymbol(args[0])
	if err != nil {
		Raise(err)
	}

	value := args[1].Eval(env)

	Global.Define(symbol, value)
	return symbol
}

func definedForm(env Env, args []Elem) Elem {
	if len(args) < 1 {
		Raise(ErrArgCount, len(args), 1)
	}

	symbol, err := AssertSymbol(args[0])
	if err != nil {
		Raise(err)
	}

	if Global.Defined(symbol) {
		return True
	}
	return Nil
}

func setForm(env Env, args []Elem) Elem {
	if len(args) < 2 {
		Raise(ErrArgCount, len(args), 2)
	}

	symbol, err := AssertSymbol(args[0].Eval(env))
	if err != nil {
		Raise(err)
	}

	value := args[1].Eval(env)

	env.Set(symbol, value)
	return symbol
}
