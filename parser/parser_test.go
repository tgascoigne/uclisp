package parser

import (
	"encoding/json"
	"fmt"
	"lisp/ast"
	"testing"
)

func dump(prog ast.Prog) string {
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
}

func TestSimpleCases(t *testing.T) {
	for _, tc := range simpleTestCases {
		prog := Parse("test", tc.Expression)
		expr := prog[0]

		t.Logf(dump(prog))

		result := expr.Eval()
		if result != tc.Result {
			t.Errorf("Value incorrect: got %v expected %v. Expression: %v", result, tc.Result, tc.Expression)
		}
	}
}
