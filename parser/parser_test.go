package parser

import (
	"encoding/json"
	"fmt"
	"lisp/ast"
	"testing"
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
	{"(= 1 1 1)", ast.True},
	{"(= 1 2 1)", ast.Nil},
	{"(= 1 2)", ast.Nil},
	{"(if 1 (+ 2 2))", ast.Integer(4)},
	{"(if (= 1 1) (+ 2 2))", ast.Integer(4)},
	{"(if nil (+ 2 2))", ast.Nil},
	{"(if nil (+ 2 2) (+ 4 4))", ast.Integer(8)},
	{"(if (= 1 2) (+ 2 2) (+ 4 4))", ast.Integer(8)},
}

func TestSimpleCases(t *testing.T) {
	for _, tc := range simpleTestCases {
		prog := Parse("test", tc.Expression)
		expr := prog

		t.Logf(dump(prog))

		result := expr.Eval(ast.Global)
		if result != tc.Result {
			t.Errorf("Value incorrect: got %v expected %v. Expression: %v", result, tc.Result, tc.Expression)
		}
	}
}
