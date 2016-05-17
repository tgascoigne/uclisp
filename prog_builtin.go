package uclisp

func init() {
	Builtin.Define("progn", Procedure(prognForm))
}

func prognForm(ctx *Context, env Env, args []Elem) Elem {
	var result Elem
	result = Nil

	for _, form := range args {
		result = ctx.Eval(form, env)
	}

	return result
}
