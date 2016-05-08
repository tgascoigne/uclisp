package uclisp

func init() {
	Builtin.Set("+", Procedure(addForm))
}

func addForm(env Env, args []Elem) Elem {
	accum := Integer(0)

	for i := range args {
		accum += AssertInt(args[i].Eval(env))
	}

	return accum
}
