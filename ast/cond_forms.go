package ast

func init() {
	Global.Set(Symbol("if"), SpecialForm{ifForm})
}

func ifForm(env *Env, args List) Value {
	var test, then, els Form
	if len(args) == 2 {
		test, then = args[0], args[1]
	} else if len(args) == 3 {
		test, then, els = args[0], args[1], args[2]
	} else {
		exceptionArgCount("if", len(args))
	}

	if !test.Eval(env).IsNil() {
		return then.Eval(env)
	}

	if els != nil {
		return els.Eval(env)
	}

	return Nil
}
