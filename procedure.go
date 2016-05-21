package uclisp

import "github.com/tgascoigne/uclisp/counter"

var procIdGenerator = counter.New()

type ProcedureFunc func(ctx *Context, env Env, args []Elem) Elem

// Procedure is a function which can be called with some arguments
type Procedure struct {
	id int
	fn ProcedureFunc
}

func NewProcedure(fn ProcedureFunc) Procedure {
	return Procedure{
		id: <-procIdGenerator,
		fn: fn,
	}
}

func (p Procedure) Equals(ctx *Context, env Env, other Elem) bool {
	otherProc, err := AssertProcedure(other)
	if err != nil {
		ctx.Raise(err, other)
	}

	// Comparing procedures by pointer didn't work out too well
	// To be on the safe side, every NewProcedure gets a unique id which we use
	// to test equality.
	return p.id == otherProc.id
}

func (p Procedure) Eval(ctx *Context, env Env) Elem {
	return p
}

// Call makes the procedure call (proc args...)
func (p Procedure) Call(ctx *Context, env Env, args []Elem) Elem {
	return p.fn(ctx, env, args)
}
