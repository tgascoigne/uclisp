package ast

import "errors"

var ErrNoSymbol = errors.New("no such symbol")

type Symbol string

func (s Symbol) Set(v Value) {

}

func (s Symbol) Eval() Value {
	if fn, ok := specialForms[s]; ok {
		return fn
	}

	/* todo: do this better */
	if s == Symbol("nil") {
		return Nil
	}

	if s == Symbol("t") {
		return True
	}

	exception(ErrNoSymbol, string(s))
	return Nil
}
