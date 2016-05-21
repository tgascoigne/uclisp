package uclisp

func init() {
	Builtin.Define("while", NewProcedure(whileForm))
}

func whileForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) < 1 {
		ctx.Raise(ErrArgCount, len(args))
	}

	test := args[0]
	body := append(List{Symbol("progn")}, args[1:]...)
	for !IsNil(ctx.Eval(test, env)) {
		ctx.Eval(body, env)
	}

	return Nil
}
