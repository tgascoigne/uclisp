package ast

import (
	"errors"
	"fmt"
)

var ErrInvalidVarForm = errors.New("invalid variable binding list")
var ErrInvalidType = errors.New("invalid type")

func init() {
	Global.Set(Symbol("let"), SpecialForm{letForm})
	Global.Set(Symbol("if"), SpecialForm{ifForm})
	Global.Set(Symbol("+"), SpecialForm{addForm})
	Global.Set(Symbol("="), SpecialForm{mathEqualForm})
}

func letForm(env *Env, args List) Value {
	if len(args) == 0 {
		exceptionArgCount("if", len(args))
	}

	if bindings, ok := args[0].(List); ok {
		for _, b := range bindings {
			if b, ok := b.(List); ok {
				// ((sym value) ..) syntax
				if len(b) != 2 {
					exception(ErrInvalidVarForm, b)
				}

				if sym, ok := b[0].(Symbol); ok {
					env.Set(sym, b[1].Eval(env))
				} else {
					exception(ErrNotASymbol, b)
				}
				continue
			}

			// (sym ..) syntax
			if sym, ok := b.(Symbol); ok {
				env.Set(sym, Nil)
			} else {
				exception(ErrNotASymbol, b)
			}
		}
	} else {
		exception(ErrInvalidVarForm, args[0])
	}

	var result Value
	result = Nil
	for _, f := range args[1:] {
		result = f.Eval(env)
	}

	return result
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
