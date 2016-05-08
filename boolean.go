package uclisp

func init() {
	Builtin.Set("nil", Nil)
	Builtin.Set("t", True)
	Builtin.Set("not", Procedure(notForm))
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
