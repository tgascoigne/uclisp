package uclisp

// Procedure is a function which can be called with some arguments
type Procedure func(ctx *Context, env Env, args []Elem) Elem

func (p Procedure) Equals(env Env, other Elem) bool {
	return false
}

func (p Procedure) Eval(ctx *Context, env Env) Elem {
	return p
}

// Call makes the procedure call (proc args...)
func (p Procedure) Call(ctx *Context, env Env, args []Elem) Elem {
	return p(ctx, env, args)
}
