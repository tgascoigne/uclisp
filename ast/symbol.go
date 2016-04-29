package ast

import "errors"

var ErrNotASymbol = errors.New("not a valid symbol")

// A Symbol is a string which evaluates to a value within the environment.
type Symbol string

func (s Symbol) Eval(env *Env) Value {
	return env.Get(s)
}
