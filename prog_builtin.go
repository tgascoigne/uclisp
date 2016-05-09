package uclisp

func init() {
	Builtin.Set("progn", Procedure(prognForm))
}

func prognForm(env Env, args []Elem) Elem {
	var result Elem

	for _, form := range args {
		result = form.Eval(env)
	}

	return result
}
