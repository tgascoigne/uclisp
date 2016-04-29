package ast

import "fmt"

func init() {
	Global.Set(Symbol("+"), SpecialForm{addForm})
	Global.Set(Symbol("-"), SpecialForm{subForm})
	Global.Set(Symbol("*"), SpecialForm{mulForm})
	Global.Set(Symbol("/"), SpecialForm{divForm})
	Global.Set(Symbol("="), SpecialForm{mathEqualForm})
	Global.Set(Symbol("/="), SpecialForm{mathNotEqualForm})
	Global.Set(Symbol("not"), SpecialForm{notForm})
	Global.Set(Symbol("<"), SpecialForm{compLessForm})
	Global.Set(Symbol("<="), SpecialForm{compLessEqForm})
	Global.Set(Symbol(">"), SpecialForm{compGreaterForm})
	Global.Set(Symbol(">="), SpecialForm{compGreaterEqForm})
}

func addForm(env *Env, args List) Value {
	var accum Integer
	for _, op := range args {
		iop := op.Eval(env)
		if iop.Type() != IntegerType {
			exception(ErrInvalidType, fmt.Sprintf("Type is %v, expected %v"))
		}

		accum += iop.(Integer)
	}

	return accum
}

func subForm(env *Env, args List) Value {
	var accum Integer
	for i, op := range args {
		iop := op.Eval(env)
		if iop.Type() != IntegerType {
			exception(ErrInvalidType, fmt.Sprintf("Type is %v, expected %v"))
		}

		if i == 0 {
			accum = iop.(Integer)
		} else {
			accum -= iop.(Integer)
		}
	}

	return accum
}

func mulForm(env *Env, args List) Value {
	var accum Integer
	for i, op := range args {
		iop := op.Eval(env)
		if iop.Type() != IntegerType {
			exception(ErrInvalidType, fmt.Sprintf("Type is %v, expected %v"))
		}

		if i == 0 {
			accum = iop.(Integer)
		} else {
			accum *= iop.(Integer)
		}
	}

	return accum
}

func divForm(env *Env, args List) Value {
	var accum Integer
	for i, op := range args {
		iop := op.Eval(env)
		if iop.Type() != IntegerType {
			exception(ErrInvalidType, fmt.Sprintf("Type is %v, expected %v"))
		}

		if i == 0 {
			accum = iop.(Integer)
		} else {
			accum /= iop.(Integer)
		}
	}

	return accum
}

func notForm(env *Env, args List) Value {
	if len(args) != 1 {
		exceptionArgCount("not", len(args))
	}

	val := args[0].Eval(env)

	if val.IsNil() {
		return True
	}

	return Nil
}

/* comparisons */

func mathEqualForm(env *Env, args List) Value {
	for i := range args {
		if i == 0 {
			continue
		}

		if args[i-1].Eval(env) != args[i].Eval(env) {
			return Nil
		}
	}

	return True
}

func mathNotEqualForm(env *Env, args List) Value {
	if len(args) != 2 {
		exceptionArgCount("/=", len(args))
	}

	if args[0].Eval(env) != args[1].Eval(env) {
		return True
	}
	return Nil
}

func compLessForm(env *Env, args List) Value {
	for i := range args {
		if i == 0 {
			continue
		}

		op1 := args[i-1].Eval(env)
		if op1.Type() != IntegerType {
			exception(ErrInvalidType, fmt.Sprintf("Type is %v, expected %v"))
		}

		op2 := args[i].Eval(env)
		if op2.Type() != IntegerType {
			exception(ErrInvalidType, fmt.Sprintf("Type is %v, expected %v"))
		}

		iop1, iop2 := op1.(Integer), op2.(Integer)

		if !(iop1 > iop2) {
			return Nil
		}
	}

	return True
}

func compLessEqForm(env *Env, args List) Value {
	for i := range args {
		if i == 0 {
			continue
		}

		op1 := args[i-1].Eval(env)
		if op1.Type() != IntegerType {
			exception(ErrInvalidType, fmt.Sprintf("Type is %v, expected %v"))
		}

		op2 := args[i].Eval(env)
		if op2.Type() != IntegerType {
			exception(ErrInvalidType, fmt.Sprintf("Type is %v, expected %v"))
		}

		iop1, iop2 := op1.(Integer), op2.(Integer)

		if !(iop1 >= iop2) {
			return Nil
		}
	}

	return True
}

func compGreaterForm(env *Env, args List) Value {
	for i := range args {
		if i == 0 {
			continue
		}

		op1 := args[i-1].Eval(env)
		if op1.Type() != IntegerType {
			exception(ErrInvalidType, fmt.Sprintf("Type is %v, expected %v"))
		}

		op2 := args[i].Eval(env)
		if op2.Type() != IntegerType {
			exception(ErrInvalidType, fmt.Sprintf("Type is %v, expected %v"))
		}

		iop1, iop2 := op1.(Integer), op2.(Integer)

		if !(iop1 < iop2) {
			return Nil
		}
	}

	return True
}

func compGreaterEqForm(env *Env, args List) Value {
	for i := range args {
		if i == 0 {
			continue
		}

		op1 := args[i-1].Eval(env)
		if op1.Type() != IntegerType {
			exception(ErrInvalidType, fmt.Sprintf("Type is %v, expected %v"))
		}

		op2 := args[i].Eval(env)
		if op2.Type() != IntegerType {
			exception(ErrInvalidType, fmt.Sprintf("Type is %v, expected %v"))
		}

		iop1, iop2 := op1.(Integer), op2.(Integer)

		if !(iop1 <= iop2) {
			return Nil
		}
	}

	return True
}
