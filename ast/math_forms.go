package ast

import (
	"errors"
	"fmt"
)

var ErrInvalidType = errors.New("invalid type")

func init() {
	specialForms[Symbol("+")] = SpecialForm{addForm}
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
