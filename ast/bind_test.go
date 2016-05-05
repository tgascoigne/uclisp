package ast_test

import (
	"encoding/json"
	"fmt"
	"testing"

	"github.com/tgascoigne/uclisp/ast"
	"github.com/tgascoigne/uclisp/parser"
)

type barObject struct {
	Z int
}

type fooObject struct {
	X, Y int
	Bar  barObject
}

func (f fooObject) TestFunc1() int {
	return f.X * f.Y
}

func (f fooObject) TestFunc2(a, b int) int {
	return a + b
}

func (f *fooObject) TestFunc3(a, b int) (int, int) {
	f.X = a
	f.Y = b
	return f.X, f.Y
}

var foo = fooObject{
	X: 20,
	Y: 16,
	Bar: barObject{
		10,
	},
}

type bindTest struct {
	Expression string
	Result     ast.Value
}

var bindTestCases = []bindTest{
	{"(with foo X)", ast.Integer(foo.X)},
	{"(with foo Y)", ast.Integer(foo.Y)},
	{"(with foo (+ X Y))", ast.Integer(foo.X + foo.Y)},
	{`(progn
		(define bar (lambda (env) (with env X)))
		(bar foo))`, ast.Integer(foo.X)},
	{`(progn
		(with foo (set X (+ X 500)))
		(with foo X))`, ast.Integer(foo.X + 500)},
	{"(with foo (TestFunc1))", ast.Integer(foo.X * foo.Y)},
	{"(with foo (TestFunc2 12 34))", ast.Integer(12 + 34)},
	{"(with foo (TestFunc3 12 34))", ast.List{ast.Integer(12), ast.Integer(34)}},
	{"(with foo (TestFunc3 12 34) (list X Y))", ast.List{ast.Integer(12), ast.Integer(34)}},
	{`(progn
		(let ((ztmp (with foo (with Bar Z))))
		  (with foo (with Bar (set Z 20)))
		  (* (with foo (with Bar Z)) ztmp)))`, ast.Integer(foo.Bar.Z * 20)},
}

func TestSimpleCases(t *testing.T) {
	for _, tc := range bindTestCases {
		prog := parser.Parse("test", tc.Expression)
		expr := prog

		t.Logf(dump(prog))

		// Create a new global env for each test case for isolation
		ast.Global = ast.NewEnv(ast.Builtin)
		fooCopy := foo
		ast.Global.Define(ast.Symbol("foo"), ast.Bind(&fooCopy))

		result := expr.Eval(ast.Global)
		if !result.Equals(ast.Global, tc.Result) {
			t.Errorf("Value incorrect: got %v expected %v. Expression: %v", result, tc.Result, tc.Expression)
		}
	}
}

func TestFieldPersistence(t *testing.T) {
	fooCopy := foo

	if fooCopy.X != foo.X {
		t.Errorf("Value incorrect: got %v expected %v", fooCopy.X, foo.X)
	}

	// Create a new global env for each test case for isolation
	ast.Global = ast.NewEnv(ast.Builtin)
	ast.Global.Define(ast.Symbol("foo"), ast.Bind(&fooCopy))

	prog := parser.Parse("test", `(progn
		(with foo (set X (+ X 500)))
		(with foo X))`)
	expr := prog

	t.Logf(dump(prog))

	result := expr.Eval(ast.Global)
	if !result.Equals(ast.Global, ast.Integer(foo.X+500)) {
		t.Errorf("Value incorrect: got %v expected %v. Expression: %v", result, ast.Integer(foo.X+500))
	}

	if fooCopy.X != (foo.X + 500) {
		t.Errorf("Value incorrect: got %v expected %v", fooCopy.X, (foo.X + 500))
	}

	fooCopy.X = foo.X

	result = expr.Eval(ast.Global)
	if !result.Equals(ast.Global, ast.Integer(foo.X+500)) {
		t.Errorf("Value incorrect: got %v expected %v. Expression: %v", result, ast.Integer(foo.X+500))
	}

	if fooCopy.X != (foo.X + 500) {
		t.Errorf("Value incorrect: got %v expected %v", fooCopy.X, (foo.X + 500))
	}

}

func dump(prog interface{}) string {
	astStr, _ := json.Marshal(prog)
	return fmt.Sprintf("%v", string(astStr))
}
