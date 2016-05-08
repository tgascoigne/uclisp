package uclisp_test

import (
	"testing"

	"github.com/tgascoigne/uclisp"
)

var mathTests = BasicTests{
	{"(+ 2 (+ 3 4 5))", uclisp.Integer(14)},
	{"(+ 2 (+ 3 4) 5 (+ 6 7 8 9))", uclisp.Integer(44)},
	{`(+
	2
	(+ 3 4)
	5
	(+ 6 7 8 9
	))`, uclisp.Integer(44)},
	{"(- 10 (+ 5 1))", uclisp.Integer(4)},
	{"(* 2 12 (/ 5 2 1))", uclisp.Integer(48)},
	{"(= 1 1 1)", uclisp.True},
	{"(= 1 2 1)", uclisp.Nil},
	{"(= 1 2)", uclisp.Nil},
	{"(/= 1 2)", uclisp.True},
	{"(/= 1 1)", uclisp.Nil},
	{"(< 3 2 1)", uclisp.True},
	{"(< 1 2 3)", uclisp.Nil},
	{"(> 3 2 1)", uclisp.Nil},
	{"(> 1 2 3)", uclisp.True},
	{"(<= 3 2 2 1)", uclisp.True},
	{"(>= 1 2 2 3)", uclisp.True},
	{"(>= 3 2 2 1)", uclisp.Nil},
	{"(<= 1 2 2 3)", uclisp.Nil},
}

func TestMath(t *testing.T) {
	mathTests.Do(t)
}
