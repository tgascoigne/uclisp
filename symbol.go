package uclisp

import "errors"

var ErrSymbolNotDefined = errors.New("symbol not defined: %v")

var EmptySymbol Symbol

// Symbol is a string which maps to an Elem within a given Env
type Symbol string

func (s Symbol) Equals(env Env, o Elem) bool {
	other, err := AssertSymbol(o)
	if err != nil {
		Raise(err, o)
	}

	return s == other
}

func (s Symbol) Eval(ctx *Context, env Env) Elem {
	e := env.Get(s)
	if e == nil {
		Raise(ErrSymbolNotDefined, s)
	}

	return e
}
