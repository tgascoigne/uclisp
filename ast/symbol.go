package ast

import "errors"

var ErrNoSymbol = errors.New("no such symbol")
var ErrInvalidSymbol = errors.New("not a valid symbol")
var ErrImmutable = errors.New("immutable symbol")

// A Symbol is a value which references another value within the current environment.
type Symbol string

func (s Symbol) Type() Type {
	return SymbolType
}

func (s Symbol) IsNil() bool {
	return false
}

func (s Symbol) Equals(env Env, v Value) bool {
	return s.Eval(env).Equals(env, v)
}

func (s Symbol) Eval(env Env) Value {
	val := env.Get(s)
	if form, ok := val.(Form); ok {
		val = form.Eval(env)
	}
	return val
}
