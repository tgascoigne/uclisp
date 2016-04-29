package ast

import "fmt"

func init() {
	Global.Set(Symbol("+"), SpecialForm{addForm})
	Global.Set(Symbol("-"), SpecialForm{subForm})
	Global.Set(Symbol("*"), SpecialForm{mulForm})
	Global.Set(Symbol("/"), SpecialForm{divForm})
	Global.Set(Symbol("="), SpecialForm{mathEqualForm})
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
