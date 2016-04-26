package ast

import (
	"errors"
	"fmt"
)

var ErrInvalidType = errors.New("invalid type")

func init() {
	Global.Set(Symbol("+"), SpecialForm{addForm})
	Global.Set(Symbol("="), SpecialForm{mathEqualForm})
}

func addForm(args List) Value {
	var accum Integer
	for _, op := range args {
		iop := op.Eval()
		if iop.Type() != IntegerType {
			exception(ErrInvalidType, fmt.Sprintf("Type is %v, expected %v"))
		}

		accum += iop.(Integer)
	}

	return accum
}

func mathEqualForm(args List) Value {
	for i := range args {
		if i == 0 {
			continue
		}

		if args[i-1].Eval() != args[i].Eval() {
			return Nil
		}
	}

	return True
}
