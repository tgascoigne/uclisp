package uclisp

func init() {
	Builtin.Define("+", Procedure(addForm))
	Builtin.Define("-", Procedure(subForm))
	Builtin.Define("*", Procedure(mulForm))
	Builtin.Define("/", Procedure(divForm))
	Builtin.Define("=", Procedure(mathEqualForm))
	Builtin.Define("/=", Procedure(mathNotEqualForm))
	Builtin.Define("<", Procedure(compLessForm))
	Builtin.Define("<=", Procedure(compLessEqForm))
	Builtin.Define(">", Procedure(compGreaterForm))
	Builtin.Define(">=", Procedure(compGreaterEqForm))
}

func addForm(env Env, args []Elem) Elem {
	accum := Integer(0)

	for i := range args {
		xElem := Eval(args[i], env)
		x, err := AssertInteger(xElem)
		if err != nil {
			Raise(err, xElem)
		}
		accum += x
	}

	return accum
}

func subForm(env Env, args []Elem) Elem {
	accum := Integer(0)

	for i := range args {
		valElem := Eval(args[i], env)
		val, err := AssertInteger(valElem)
		if err != nil {
			Raise(err, valElem)
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
		valElem := Eval(args[i], env)
		val, err := AssertInteger(valElem)
		if err != nil {
			Raise(err, valElem)
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
		valElem := Eval(args[i], env)
		val, err := AssertInteger(valElem)
		if err != nil {
			Raise(err, valElem)
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

		v0Elem := Eval(args[i-1], env)
		v0, err := AssertInteger(v0Elem)
		if err != nil {
			Raise(err)
		}

		v1Elem := Eval(args[i], env)
		v1, err := AssertInteger(v1Elem)
		if err != nil {
			Raise(err, v1Elem)
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

	v0Elem := Eval(args[0], env)
	v0, err := AssertInteger(v0Elem)
	if err != nil {
		Raise(err)
	}

	v1Elem := Eval(args[1], env)
	v1, err := AssertInteger(v1Elem)
	if err != nil {
		Raise(err, v1Elem)
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

		op1Elem := Eval(args[i-1], env)
		op1, err := AssertInteger(op1Elem)
		if err != nil {
			Raise(err, op1Elem)
		}

		op2Elem := Eval(args[i], env)
		op2, err := AssertInteger(op2Elem)
		if err != nil {
			Raise(err, op2Elem)
		}

		if !(op1 < op2) {
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

		op1Elem := Eval(args[i-1], env)
		op1, err := AssertInteger(op1Elem)
		if err != nil {
			Raise(err, op1Elem)
		}

		op2Elem := Eval(args[i], env)
		op2, err := AssertInteger(op2Elem)
		if err != nil {
			Raise(err, op2Elem)
		}

		if !(op1 <= op2) {
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

		op1Elem := Eval(args[i-1], env)
		op1, err := AssertInteger(op1Elem)
		if err != nil {
			Raise(err, op1Elem)
		}

		op2Elem := Eval(args[i], env)
		op2, err := AssertInteger(op2Elem)
		if err != nil {
			Raise(err, op2Elem)
		}

		if !(op1 > op2) {
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

		op1Elem := Eval(args[i-1], env)
		op1, err := AssertInteger(op1Elem)
		if err != nil {
			Raise(err, op1Elem)
		}

		op2Elem := Eval(args[i], env)
		op2, err := AssertInteger(op2Elem)
		if err != nil {
			Raise(err, op2Elem)
		}

		if !(op1 >= op2) {
			return Nil
		}
	}

	return True
}
