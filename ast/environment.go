package ast

import "errors"

type Env struct {
	Map    map[Symbol]Value
	Parent *Env
}

var ErrNoSymbol = errors.New("no such symbol")

var Global *Env = NewEnv(nil)

func NewEnv(parent *Env) *Env {
	return &Env{
		Map:    make(map[Symbol]Value),
		Parent: parent,
	}
}

func (e *Env) Set(s Symbol, v Value) {
	e.Map[s] = v
}

func (e *Env) Get(s Symbol) Value {
	if v, ok := e.Map[s]; ok {
		return v
	}

	if e.Parent != nil {
		return e.Parent.Get(s)
	}

	exception(ErrNoSymbol, string(s))
	return nil
}

func (e *Env) New() *Env {
	return NewEnv(e)
}
