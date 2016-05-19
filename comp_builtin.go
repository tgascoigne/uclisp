package uclisp

func init() {
	Builtin.Define("eq", Procedure(eqForm))
}

func eqForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) < 2 {
		ctx.Raise(ErrArgCount, len(args))
	}

	for i := range args {
		if i == 0 {
			continue
		}

		v1 := ctx.Eval(args[i-1], env)
		v2 := ctx.Eval(args[i], env)

		if !Equal(ctx, env, v1, v2) {
			return Nil
		}
	}

	return True
}
