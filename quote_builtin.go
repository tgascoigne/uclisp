package uclisp

import "fmt"

const (
	UnquoteSymbol = Symbol(",")
	SpliceSymbol  = Symbol(",@")
)

func init() {
	Builtin.Set("quote", Procedure(quoteForm))
	Builtin.Set("backquote", Procedure(backquoteForm))
	Builtin.Set("eval", Procedure(evalForm))
}

func quoteForm(env Env, args []Elem) Elem {
	if len(args) != 1 {
		Raise(ErrArgCount, len(args), 1)
	}

	return args[0]
}

func backquoteForm(env Env, args []Elem) Elem {
	// `(1 ,foo ,@bar) expands to (backquote (list 1 (, foo) (,@ bar)))
	if len(args) != 1 {
		Raise(ErrArgCount, len(args), 1)
	}

	backquoted, err := AssertList(args[0])
	if err != nil {
		Raise(err)
	}

	quoted := make(List, 0)
	for _, el := range backquoted {
		if form, err := AssertList(el); err == nil && len(form) > 0 {
			switch {
			case Equal(env, form[0], UnquoteSymbol):
				if len(form) != 2 {
					Raise(ErrArgCount, len(form), 2)
				}

				el = form[1].Eval(env)

				quoted = append(quoted, el)
				continue

			case Equal(env, form[0], SpliceSymbol):
				if len(form) != 2 {
					Raise(ErrArgCount, len(form), 2)
				}

				el = form[1].Eval(env)

				list, err := AssertList(el)
				if err != nil {
					Raise(err)
				}

				quoted = append(quoted, list...)
				continue
			default:
				fmt.Printf("not a quote symbol: %v\n", form[0])
			}
		}

		quoted = append(quoted, el)
	}

	return quoted
}

func evalForm(env Env, args []Elem) Elem {
	if len(args) != 1 {
		Raise(ErrArgCount, len(args), 1)
	}

	return args[0].Eval(env).Eval(env)
}
