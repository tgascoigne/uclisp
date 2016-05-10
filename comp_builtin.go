package uclisp

func init() {
	Builtin.Define("eq", Procedure(eqForm))
}

func eqForm(env Env, args []Elem) Elem {
	for i := range args {
		if i == 0 {
			continue
		}

		v1 := args[i-1].Eval(env)
		v2 := args[i].Eval(env)

		if !Equal(env, v1, v2) {
			return Nil
		}
	}

	return True
}
