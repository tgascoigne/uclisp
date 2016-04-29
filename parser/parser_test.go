package parser

import (
	"encoding/json"
	"fmt"
	"testing"

	"github.com/tgascoigne/uclisp/ast"
)

func dump(prog ast.Form) string {
	astStr, _ := json.Marshal(prog)
	return fmt.Sprintf("%v", string(astStr))
}

type simpleTest struct {
	Expression string
	Result     ast.Value
}

var simpleTestCases = []simpleTest{
	{"(+ 2 (+ 3 4 5))", ast.Integer(14)},
	{"(+ 2 (+ 3 4) 5 (+ 6 7 8 9))", ast.Integer(44)},
	{`(+
	2
	(+ 3 4)
	5
	(+ 6 7 8 9
	))`, ast.Integer(44)},
	{"(- 10 (+ 5 1))", ast.Integer(4)},
	{"(* 2 12 (/ 5 2 1))", ast.Integer(48)},
	{"(= 1 1 1)", ast.True},
	{"(= 1 2 1)", ast.Nil},
	{"(= 1 2)", ast.Nil},
	{"(if 1 (+ 2 2))", ast.Integer(4)},
	{"(if (= 1 1) (+ 2 2))", ast.Integer(4)},
	{"(if nil (+ 2 2))", ast.Nil},
	{"(if nil (+ 2 2) (+ 4 4))", ast.Integer(8)},
	{`(if (= 1 2)
		    (+ 2 2)
		  (+ 4 4))`, ast.Integer(8)},
	{`(let ((x 2) (y 4))
		(+ x y))`, ast.Integer(6)},
	{`(let ((x 2) (y (+ 1 1)))
		(+ x y))`, ast.Integer(4)},
	{`(let ((x 2) (y (+ 1 1)))
		(let ((z (+ x y)))
		  (+ z y)))`, ast.Integer(6)},
	{"(let ((x 2) (y 4)) (+ x y) (+ x x x y))", ast.Integer(10)},
	{"(car (list 1 2))", ast.Integer(1)},
	{"(cdr (list 1 2))", ast.List{ast.Integer(2)}},
	{"(car '(1 2))", ast.Integer(1)},
	{"(cdr '(1 2))", ast.List{ast.Integer(2)}},
	{"(= nil nil)", ast.True},
	{"(= nil t)", ast.Nil},
}

func TestSimpleCases(t *testing.T) {
	for _, tc := range simpleTestCases {
		prog := Parse("test", tc.Expression)
		expr := prog

		t.Logf(dump(prog))

		result := expr.Eval(ast.Global)
		if !result.Equals(ast.Global, tc.Result) {
			t.Errorf("Value incorrect: got %v expected %v. Expression: %v", result, tc.Result, tc.Expression)
		}
	}
}
