package uclisp

func init() {
	Builtin.Set("+", Procedure(addForm))
	Builtin.Set("-", Procedure(subForm))
	Builtin.Set("*", Procedure(mulForm))
	Builtin.Set("/", Procedure(divForm))
	Builtin.Set("=", Procedure(mathEqualForm))
	Builtin.Set("/=", Procedure(mathNotEqualForm))
	Builtin.Set("<", Procedure(compLessForm))
	Builtin.Set("<=", Procedure(compLessEqForm))
	Builtin.Set(">", Procedure(compGreaterForm))
	Builtin.Set(">=", Procedure(compGreaterEqForm))
}

func addForm(env Env, args []Elem) Elem {
	accum := Integer(0)

	for i := range args {
		x, err := AssertInteger(args[i].Eval(env))
		if err != nil {
			Raise(err)
		}
		accum += x
	}

	return accum
}

func subForm(env Env, args []Elem) Elem {
	accum := Integer(0)

	for i := range args {
		val, err := AssertInteger(args[i].Eval(env))
		if err != nil {
			Raise(err)
		}

		if i == 0 {
			accum = val
		} else {
			accum -= val
		}
	}

	return accum
}

func mulForm(env Env, args []Elem) Elem {
	accum := Integer(0)

	for i := range args {
		val, err := AssertInteger(args[i].Eval(env))
		if err != nil {
			Raise(err)
		}

		if i == 0 {
			accum = val
		} else {
			accum *= val
		}
	}

	return accum
}

func divForm(env Env, args []Elem) Elem {
	accum := Integer(0)

	for i := range args {
		val, err := AssertInteger(args[i].Eval(env))
		if err != nil {
			Raise(err)
		}

		if i == 0 {
			accum = val
		} else {
			accum /= val
		}
	}

	return accum
}

func mathEqualForm(env Env, args []Elem) Elem {
	for i := range args {
		if i == 0 {
			continue
		}

		v0, err := AssertInteger(args[i-1].Eval(env))
		if err != nil {
			Raise(err)
		}

		v1, err := AssertInteger(args[i].Eval(env))
		if err != nil {
			Raise(err)
		}

		if v0 != v1 {
			return Nil
		}
	}

	return True
}

func mathNotEqualForm(env Env, args []Elem) Elem {
	if len(args) != 2 {
		Raise(ErrArgCount, len(args))
	}

	v0, err := AssertInteger(args[0].Eval(env))
	if err != nil {
		Raise(err)
	}

	v1, err := AssertInteger(args[1].Eval(env))
	if err != nil {
		Raise(err)
	}

	if v0 != v1 {
		return True
	}

	return Nil
}

func compLessForm(env Env, args []Elem) Elem {
	for i := range args {
		if i == 0 {
			continue
		}

		op1, err := AssertInteger(args[i-1].Eval(env))
		if err != nil {
			Raise(err)
		}

		op2, err := AssertInteger(args[i].Eval(env))
		if err != nil {
			Raise(err)
		}

		if !(op1 > op2) {
			return Nil
		}
	}

	return True
}

func compLessEqForm(env Env, args []Elem) Elem {
	for i := range args {
		if i == 0 {
			continue
		}

		op1, err := AssertInteger(args[i-1].Eval(env))
		if err != nil {
			Raise(err)
		}

		op2, err := AssertInteger(args[i].Eval(env))
		if err != nil {
			Raise(err)
		}

		if !(op1 >= op2) {
			return Nil
		}
	}

	return True
}

func compGreaterForm(env Env, args []Elem) Elem {
	for i := range args {
		if i == 0 {
			continue
		}

		op1, err := AssertInteger(args[i-1].Eval(env))
		if err != nil {
			Raise(err)
		}

		op2, err := AssertInteger(args[i].Eval(env))
		if err != nil {
			Raise(err)
		}

		if !(op1 < op2) {
			return Nil
		}
	}

	return True
}

func compGreaterEqForm(env Env, args []Elem) Elem {
	for i := range args {
		if i == 0 {
			continue
		}

		op1, err := AssertInteger(args[i-1].Eval(env))
		if err != nil {
			Raise(err)
		}

		op2, err := AssertInteger(args[i].Eval(env))
		if err != nil {
			Raise(err)
		}

		if !(op1 <= op2) {
			return Nil
		}
	}

	return True
}
