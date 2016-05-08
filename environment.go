package uclisp

var Builtin = NewBasicEnv(nil)
var Global = NewBasicEnv(Builtin)

// Env is a scope mapping of symbols to elements
type Env interface {
	Get(Symbol) Elem
	Set(Symbol, Elem)
}

// BasicEnv is an environment based on a simple map[Symbol]Elem
type BasicEnv struct {
	m      map[Symbol]Elem
	parent Env
}

func NewBasicEnv(parent Env) Env {
	return &BasicEnv{
		m:      make(map[Symbol]Elem),
		parent: parent,
	}
}

func (e *BasicEnv) Get(s Symbol) Elem {
	if e, ok := e.m[s]; ok {
		return e
	}

	if e.parent != nil {
		return e.parent.Get(s)
	}

	return nil
}

func (e *BasicEnv) Set(s Symbol, v Elem) {
	e.m[s] = v
}
