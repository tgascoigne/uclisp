package ast_test

import (
	"lisp/ast"
	"testing"
)

func TestAddOp1(t *testing.T) {
	/* (+ 2 4) */
	expr := ast.AddForm{
		Operands: []ast.Expression{
			ast.SimpleExpression{ast.Integer(2)},
			ast.SimpleExpression{ast.Integer(4)},
		},
	}

	result := expr.Eval()
	intResult := result.(ast.Integer)
	if int(intResult) != 6 {
		t.Errorf("Value incorrect: got %v expected 6", intResult)
	}
}

func TestAddOp2(t *testing.T) {
	/* (+ 2 (+ 3 4 5)) */
	expr := ast.AddForm{
		Operands: []ast.Expression{
			ast.SimpleExpression{ast.Integer(2)},
			ast.AddForm{
				Operands: []ast.Expression{
					ast.SimpleExpression{ast.Integer(3)},
					ast.SimpleExpression{ast.Integer(4)},
					ast.SimpleExpression{ast.Integer(5)},
				},
			},
		},
	}

	result := expr.Eval()
	intResult := result.(ast.Integer)
	if int(intResult) != 6 {
		t.Errorf("Value incorrect: got %v expected 14", intResult)
	}
}
