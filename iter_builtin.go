package uclisp

func init() {
	Builtin.Define("while", Procedure(whileForm))
}

func whileForm(env Env, args []Elem) Elem {
	if len(args) < 1 {
		Raise(ErrArgCount, len(args))
	}

	test := args[0]
	body := append(List{Symbol("progn")}, args[1:]...)
	for !IsNil(Eval(test, env)) {
		Eval(body, env)
	}

	return Nil
}
