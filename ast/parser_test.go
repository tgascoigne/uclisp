package ast

import (
	"encoding/json"
	"fmt"
	"testing"
)

func dump(prog interface{}) string {
	astStr, _ := json.Marshal(prog)
	return fmt.Sprintf("%v", string(astStr))
}

type simpleTest struct {
	Expression string
	Result     Value
}

var simpleTestCases = []simpleTest{
	{`"foo"`, String("foo")},
	{`(progn "foo")`, String("foo")},
	{"(+ 2 (+ 3 4 5))", Integer(14)},
	{"(+ 2 (+ 3 4) 5 (+ 6 7 8 9))", Integer(44)},
	{`(+
	2
	(+ 3 4)
	5
	(+ 6 7 8 9
	))`, Integer(44)},
	{"(- 10 (+ 5 1))", Integer(4)},
	{"(* 2 12 (/ 5 2 1))", Integer(48)},
	{"(= 1 1 1)", True},
	{"(= 1 2 1)", Nil},
	{"(= 1 2)", Nil},
	{"(if 1 (+ 2 2))", Integer(4)},
	{"(if (= 1 1) (+ 2 2))", Integer(4)},
	{"(if nil (+ 2 2))", Nil},
	{"(if nil (+ 2 2) (+ 4 4))", Integer(8)},
	{`(if (= 1 2)
		    (+ 2 2)
		  (+ 4 4))`, Integer(8)},
	{`(let ((x 2) (y 4))
		(+ x y))`, Integer(6)},
	{`(let ((x 2) (y (+ 1 1)))
		(+ x y))`, Integer(4)},
	{`(let ((x 2) (y (+ 1 1)))
		(let ((z (+ x y)))
		  (+ z y)))`, Integer(6)},
	{"(let ((x 2) (y 4)) (+ x y) (+ x x x y))", Integer(10)},
	{"(car (list 1 2))", Integer(1)},
	{"(cdr (list 1 2))", List{Integer(2)}},
	{"(car '(1 2))", Integer(1)},
	{"(cdr '(1 2))", List{Integer(2)}},
	{"(= nil nil)", True},
	{"(= nil t)", Nil},
	{"(/= 1 2)", True},
	{"(/= 1 1)", Nil},
	{"(< 3 2 1)", True},
	{"(< 1 2 3)", Nil},
	{"(> 3 2 1)", Nil},
	{"(> 1 2 3)", True},
	{"(<= 3 2 2 1)", True},
	{"(>= 1 2 2 3)", True},
	{"(>= 3 2 2 1)", Nil},
	{"(<= 1 2 2 3)", Nil},
	{"(not nil)", True},
	{"(not t)", Nil},
	{"(not (if (= 1 1) t))", Nil},
	{`(progn
		(define square (lambda (x) (* x x)))
		(square 2))`, Integer(4)},
	{`(progn
		(define x 20)
		(define x 40)
		x)`, Integer(20)},
	{`(progn
		(define x 20)
		(set x 40)
		x)`, Integer(40)},
	{`(progn
		(define x 20)
		(define y 40)
		(list x y))`, List{Integer(20), Integer(40)}},
	{`(progn
		(define x 20)
		(define y 40)
		'(x y))`, List{Symbol("x"), Symbol("y")}},
	{`'(x y)`, List{Symbol("x"), Symbol("y")}},
}

func TestSimpleCases(t *testing.T) {
	for _, tc := range simpleTestCases {
		prog := Parse("test", tc.Expression)
		expr := prog

		t.Logf(dump(prog))

		// Create a new global env for each test case for isolation
		Global = NewEnv(Builtin)

		result := expr.Eval(Global)
		if !result.Equals(Global, tc.Result) {
			t.Errorf("Value incorrect: got %v expected %v. Expression: %v", result, tc.Result, tc.Expression)
		}
	}
}

type exceptionTest struct {
	Expression string
	Exception  error
}

var exceptionTestCases = []exceptionTest{
	{`(progn
		(let ((x 20)) (* 2 x))
		x)`, ErrNoSymbol},
	{`(progn
		(set x 20)
		x)`, ErrNoSymbol},
}

func TestExceptionCases(t *testing.T) {
	for _, tc := range exceptionTestCases {
		func() {
			expectedCause := tc.Exception
			defer func() {
				if err := recover(); err != nil {
					if detailed, ok := err.(DetailedError); !ok || expectedCause != detailed.Cause() {
						t.Errorf("Didn't raise expected exception: %v, expected %v", detailed.Cause(), expectedCause)
					}
				}
			}()

			prog := Parse("test", tc.Expression)
			expr := prog

			t.Logf(dump(prog))

			// Create a new global env for each test case for isolation
			Global = NewEnv(Builtin)

			result := expr.Eval(Global)
			t.Errorf("Result was %v", result) // Eval should have panicked
		}()
	}
}
