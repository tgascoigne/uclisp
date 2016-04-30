package ast

import "errors"

var ErrNotASymbol = errors.New("not a valid symbol")

// A Symbol is a value which references another value within the current environment.
type Symbol string

func (s Symbol) Type() Type {
	return SymbolType
}

func (s Symbol) IsAtom() bool {
	return true
}

func (s Symbol) IsNil() bool {
	return false
}

func (s Symbol) Equals(env *Env, v Value) bool {
	return s.Eval(env).Equals(env, v)
}

func (s Symbol) Eval(env *Env) Value {
	return env.Get(s)
}
