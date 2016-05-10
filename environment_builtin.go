package uclisp

import "errors"

var ErrInvalidBindSpec = errors.New("invalid bind specification: %v")

func init() {
	Builtin.Set("let", genLetForms(false))
	Builtin.Set("let*", genLetForms(true))
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
					bound.Set(sym, initial.Eval(bound))
				} else {
					bound.Set(sym, initial.Eval(env))
				}

				continue
			}

			if sym, err := AssertSymbol(el); err == nil {
				bound.Set(sym, Nil)
			} else {
				Raise(err)
			}
		}

		body := append(List{Symbol("progn")}, args[1:]...)
		return body.Eval(bound)
	})
}
