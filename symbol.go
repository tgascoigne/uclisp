package uclisp

// Symbol is a string which maps to an Elem within a given Env
type Symbol string

func (s Symbol) Equals(env Env, o Elem) bool {
	other := AssertSymbol(o)

	return s == other
}

func (s Symbol) Eval(env Env) Elem {
	return env.Get(s)
}
