package uclisp

func init() {
	Builtin.Define("nil", Nil)
	Builtin.Define("t", True)
	Builtin.Define("not", Procedure(notForm))
	Builtin.Define("cond", Procedure(condForm))
}

func notForm(env Env, args []Elem) Elem {
	if len(args) != 1 {
		Raise(ErrArgCount, len(args))
	}

	val := Eval(args[0], env)

	if IsNil(val) {
		return True
	}

	return Nil
}

func condForm(env Env, args []Elem) Elem {
	for _, clause := range args {
		if clause, err := AssertList(clause); err == nil {
			if len(clause) == 0 {
				continue
			}

			cond := Eval(clause[0], env)
			if Equal(env, cond, True) {
				if len(clause) > 1 {
					body := append(List{Symbol("progn")}, clause[1:]...)
					return Eval(body, env)
				}

				return cond
			}
		} else {
			Raise(err, clause)
		}
	}

	return Nil
}
