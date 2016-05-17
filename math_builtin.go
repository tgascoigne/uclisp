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

func addForm(ctx *Context, env Env, args []Elem) Elem {
	accum := Integer(0)

	for i := range args {
		xElem := ctx.Eval(args[i], env)
		x, err := AssertInteger(xElem)
		if err != nil {
			ctx.Raise(err, xElem)
		}
		accum += x
	}

	return accum
}

func subForm(ctx *Context, env Env, args []Elem) Elem {
	accum := Integer(0)

	for i := range args {
		valElem := ctx.Eval(args[i], env)
		val, err := AssertInteger(valElem)
		if err != nil {
			ctx.Raise(err, valElem)
		}

		if i == 0 {
			accum = val
		} else {
			accum -= val
		}
	}

	return accum
}

func mulForm(ctx *Context, env Env, args []Elem) Elem {
	accum := Integer(0)

	for i := range args {
		valElem := ctx.Eval(args[i], env)
		val, err := AssertInteger(valElem)
		if err != nil {
			ctx.Raise(err, valElem)
		}

		if i == 0 {
			accum = val
		} else {
			accum *= val
		}
	}

	return accum
}

func divForm(ctx *Context, env Env, args []Elem) Elem {
	accum := Integer(0)

	for i := range args {
		valElem := ctx.Eval(args[i], env)
		val, err := AssertInteger(valElem)
		if err != nil {
			ctx.Raise(err, valElem)
		}

		if i == 0 {
			accum = val
		} else {
			accum /= val
		}
	}

	return accum
}

func mathEqualForm(ctx *Context, env Env, args []Elem) Elem {
	for i := range args {
		if i == 0 {
			continue
		}

		v0Elem := ctx.Eval(args[i-1], env)
		v0, err := AssertInteger(v0Elem)
		if err != nil {
			ctx.Raise(err)
		}

		v1Elem := ctx.Eval(args[i], env)
		v1, err := AssertInteger(v1Elem)
		if err != nil {
			ctx.Raise(err, v1Elem)
		}

		if v0 != v1 {
			return Nil
		}
	}

	return True
}

func mathNotEqualForm(ctx *Context, env Env, args []Elem) Elem {
	if len(args) != 2 {
		ctx.Raise(ErrArgCount, len(args))
	}

	v0Elem := ctx.Eval(args[0], env)
	v0, err := AssertInteger(v0Elem)
	if err != nil {
		ctx.Raise(err)
	}

	v1Elem := ctx.Eval(args[1], env)
	v1, err := AssertInteger(v1Elem)
	if err != nil {
		ctx.Raise(err, v1Elem)
	}

	if v0 != v1 {
		return True
	}

	return Nil
}

func compLessForm(ctx *Context, env Env, args []Elem) Elem {
	for i := range args {
		if i == 0 {
			continue
		}

		op1Elem := ctx.Eval(args[i-1], env)
		op1, err := AssertInteger(op1Elem)
		if err != nil {
			ctx.Raise(err, op1Elem)
		}

		op2Elem := ctx.Eval(args[i], env)
		op2, err := AssertInteger(op2Elem)
		if err != nil {
			ctx.Raise(err, op2Elem)
		}

		if !(op1 < op2) {
			return Nil
		}
	}

	return True
}

func compLessEqForm(ctx *Context, env Env, args []Elem) Elem {
	for i := range args {
		if i == 0 {
			continue
		}

		op1Elem := ctx.Eval(args[i-1], env)
		op1, err := AssertInteger(op1Elem)
		if err != nil {
			ctx.Raise(err, op1Elem)
		}

		op2Elem := ctx.Eval(args[i], env)
		op2, err := AssertInteger(op2Elem)
		if err != nil {
			ctx.Raise(err, op2Elem)
		}

		if !(op1 <= op2) {
			return Nil
		}
	}

	return True
}

func compGreaterForm(ctx *Context, env Env, args []Elem) Elem {
	for i := range args {
		if i == 0 {
			continue
		}

		op1Elem := ctx.Eval(args[i-1], env)
		op1, err := AssertInteger(op1Elem)
		if err != nil {
			ctx.Raise(err, op1Elem)
		}

		op2Elem := ctx.Eval(args[i], env)
		op2, err := AssertInteger(op2Elem)
		if err != nil {
			ctx.Raise(err, op2Elem)
		}

		if !(op1 > op2) {
			return Nil
		}
	}

	return True
}

func compGreaterEqForm(ctx *Context, env Env, args []Elem) Elem {
	for i := range args {
		if i == 0 {
			continue
		}

		op1Elem := ctx.Eval(args[i-1], env)
		op1, err := AssertInteger(op1Elem)
		if err != nil {
			ctx.Raise(err, op1Elem)
		}

		op2Elem := ctx.Eval(args[i], env)
		op2, err := AssertInteger(op2Elem)
		if err != nil {
			ctx.Raise(err, op2Elem)
		}

		if !(op1 >= op2) {
			return Nil
		}
	}

	return True
}
