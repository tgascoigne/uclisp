package uclisp

func init() {
	Builtin.Define("eq", Procedure(eqForm))
}

func eqForm(env Env, args []Elem) Elem {
	for i := range args {
		if i == 0 {
			continue
		}

		v1 := Eval(args[i-1], env)
		v2 := Eval(args[i], env)

		if !Equal(env, v1, v2) {
			return Nil
		}
	}

	return True
}
