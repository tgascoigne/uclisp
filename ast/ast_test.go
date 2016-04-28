package ast_test

import (
	"testing"

	"github.com/tgascoigne/uclisp/ast"
)

func TestAddOp1(t *testing.T) {
	/* (+ 2 4) */
	expr := ast.AddForm{
		Operands: []ast.Form{
			ast.Integer(2),
			ast.Integer(4),
		},
	}

	result := expr.Eval(Global)
	intResult := result.(ast.Integer)
	if int(intResult) != 6 {
		t.Errorf("Value incorrect: got %v expected 6", intResult)
	}
}

func TestAddOp2(t *testing.T) {
	/* (+ 2 (+ 3 4 5)) */
	expr := ast.AddForm{
		Operands: []ast.Form{
			ast.Integer(2),
			ast.AddForm{
				Operands: []ast.Form{
					ast.Integer(3),
					ast.Integer(4),
					ast.Integer(5),
				},
			},
		},
	}

	result := expr.Eval(Global)
	intResult := result.(ast.Integer)
	if int(intResult) != 6 {
		t.Errorf("Value incorrect: got %v expected 14", intResult)
	}
}
