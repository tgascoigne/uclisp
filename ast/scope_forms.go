package ast

import "errors"

var ErrInvalidVarForm = errors.New("invalid variable binding list")

func init() {
	Global.Set(Symbol("let"), SpecialForm{letForm})
}

func letForm(env *Env, args List) Value {
	if len(args) == 0 {
		exceptionArgCount("if", len(args))
	}

	if bindings, ok := args[0].(List); ok {
		for _, b := range bindings {
			if b, ok := b.(List); ok {
				// ((sym value) ..) syntax
				if len(b) != 2 {
					exception(ErrInvalidVarForm, b)
				}

				if sym, ok := b[0].(Symbol); ok {
					env.Set(sym, b[1].Eval(env))
				} else {
					exception(ErrNotASymbol, b)
				}
				continue
			}

			// (sym ..) syntax
			if sym, ok := b.(Symbol); ok {
				env.Set(sym, Nil)
			} else {
				exception(ErrNotASymbol, b)
			}
		}
	} else {
		exception(ErrInvalidVarForm, args[0])
	}

	var result Value
	result = Nil
	for _, f := range args[1:] {
		result = f.Eval(env)
	}

	return result
}
