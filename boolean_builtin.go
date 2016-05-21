package uclisp

func init() {
	Builtin.Define("nil", Nil)
	Builtin.Define("t", True)
	Builtin.Define("not", NewProcedure(notForm))
	Builtin.Define("cond", NewProcedure(condForm))
}

func notForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) != 1 {
		ctx.Raise(ErrArgCount, len(args))
	}

	val := ctx.Eval(args[0], env)

	if IsNil(val) {
		return True
	}

	return Nil
}

func condForm(ctx *Context, env Env, args []Elem) Elem {
	for _, clause := range args {
		if clause, err := AssertList(clause); err == nil {
			if len(clause) == 0 {
				continue
			}

			cond := ctx.Eval(clause[0], env)
			if Equal(ctx, env, cond, True) {
				if len(clause) > 1 {
					body := append(List{Symbol("progn")}, clause[1:]...)
					return ctx.Eval(body, env)
				}

				return cond
			}
		} else {
			ctx.Raise(err, clause)
		}
	}

	return Nil
}
