package uclisp

import "errors"

var ErrInvalidBindSpec = errors.New("invalid bind specification: %v")

func init() {
	Builtin.Define("with", NewProcedure(withForm))
	Builtin.Define("define", NewProcedure(defineForm))
	Builtin.Define("defined", NewProcedure(definedForm))
	Builtin.Define("let", genLetForms(false))
	Builtin.Define("let*", genLetForms(true))
	Builtin.Define("set", NewProcedure(setForm))
}

func withForm(ctx *Context, parentenv Env, args []Elem) Elem {
	if len(args) < 2 {
		ctx.Raise(ErrArgCount, len(args))
	}

	arg0val := args[0].Eval(ctx, parentenv)

	goValue, err := AssertGoValue(arg0val)
	if err != nil {
		ctx.Raise(err, arg0val)
	}

	env := goValue.With(parentenv)

	var result Elem
	result = Nil
	for _, f := range args[1:] {
		result = f.Eval(ctx, env)
	}

	return result
}

func genLetForms(starform bool) Procedure {
	return NewProcedure(func(ctx *Context, env Env, args []Elem) Elem {
		if len(args) < 1 {
			ctx.Raise(ErrArgCount, len(args))
		}

		bound := NewBasicEnv(env)

		bindSpec, err := AssertList(args[0])
		if err != nil {
			ctx.Raise(err, args[0])
		}

		for _, el := range bindSpec {
			if bindList, err := AssertList(el); err == nil {
				if len(bindList) != 2 {
					ctx.Raise(ErrInvalidBindSpec, bindSpec)
				}

				sym, err := AssertSymbol(bindList[0])
				if err != nil {
					ctx.Raise(err, bindList[0])
				}

				initial := bindList[1]

				if starform {
					// let* can reference earlier bindings
					bound.Define(sym, ctx.Eval(initial, bound))
				} else {
					bound.Define(sym, ctx.Eval(initial, env))
				}

				continue
			}

			if sym, err := AssertSymbol(el); err == nil {
				bound.Define(sym, Nil)
			} else {
				ctx.Raise(err, el)
			}
		}

		body := append(List{Symbol("progn")}, args[1:]...)
		return ctx.Eval(body, bound)
	})
}

func defineForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) != 2 {
		ctx.Raise(ErrArgCount, len(args), 2)
	}

	symbol, err := AssertSymbol(args[0])
	if err != nil {
		ctx.Raise(err, args[0])
	}

	value := ctx.Eval(args[1], env)

	Global.Define(symbol, value)
	return symbol
}

func definedForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) != 1 {
		ctx.Raise(ErrArgCount, len(args), 1)
	}

	symbol, err := AssertSymbol(args[0])
	if err != nil {
		ctx.Raise(err, args[0])
	}

	if env.Defined(ctx, symbol) {
		return True
	}
	return Nil
}

func setForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) != 2 {
		ctx.Raise(ErrArgCount, len(args), 2)
	}

	symbolElem := ctx.Eval(args[0], env)
	symbol, err := AssertSymbol(symbolElem)
	if err != nil {
		ctx.Raise(err, symbolElem)
	}

	if !env.Defined(ctx, symbol) {
		ctx.Raise(ErrSymbolNotDefined, symbol)
	}

	value := ctx.Eval(args[1], env)

	env.Set(ctx, symbol, value)
	return symbol
}
