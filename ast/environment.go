package ast

import "errors"

var ErrNoSymbol = errors.New("no such symbol")

// Env is a mapping of Symbol -> Value.
// An Environment is heirarchical: each Env has a parent Env, and all Envs are rooted at Global.
// Nested Envs are used for scoping of variables.
type Env struct {
	Map    map[Symbol]Value
	Parent *Env
}

// Global is the interpreter's global scope.
var Global *Env = NewEnv(nil)

func NewEnv(parent *Env) *Env {
	return &Env{
		Map:    make(map[Symbol]Value),
		Parent: parent,
	}
}

// Set sets a value within a given scope
func (e *Env) Set(s Symbol, v Value) {
	e.Map[s] = v
}

// Get looks up a value within a given scope.
// If the symbol does not exist in the current scope, it recurses up until it reaches the global scope.
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

// New creates an environment with a parent environment of 'e'
func (e *Env) New() *Env {
	return NewEnv(e)
}
