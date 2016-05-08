package uclisp

// Procedure is a function which can be called with some arguments
type Procedure func(env Env, args []Elem) Elem

func (p Procedure) Eval(env Env) Elem {
	return p
}

// Call makes the procedure call (proc args...)
func (p Procedure) Call(env Env, args []Elem) Elem {
	return p(env, args)
}
