package ast

import "errors"

var ErrNotASymbol = errors.New("not a valid symbol")

type Symbol string

func (s Symbol) Eval(env *Env) Value {
	return env.Get(s)
}
