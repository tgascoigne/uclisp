package uclisp

func init() {
	Builtin.Set("nil", Nil)
	Builtin.Set("t", True)
	Builtin.Set("not", Procedure(notForm))
	Builtin.Set("cond", Procedure(condForm))
}

var Nil = List{}

var True = trueSymbol("t")

type trueSymbol Symbol

func (t trueSymbol) Equals(env Env, other Elem) bool {
	return other == True
}

func (t trueSymbol) Eval(env Env) Elem {
	return True
}

func notForm(env Env, args []Elem) Elem {
	if len(args) != 1 {
		Raise(ErrArgCount, len(args))
	}

	val := args[0].Eval(env)

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

			cond := clause[0].Eval(env)
			if Equal(env, cond, True) {
				if len(clause) > 1 {
					body := append(List{Symbol("progn")}, clause[1:]...)
					return body.Eval(env)
				}

				return cond
			}
		} else {
			Raise(err)
		}
	}

	return Nil
}
