package uclisp_test

import (
	"testing"

	"github.com/tgascoigne/uclisp"
)

var progTests = BasicTests{
	{"(progn)", uclisp.Nil},
	{"(progn 2)", uclisp.Integer(2)},
	{"(progn 2 (* 2 2))", uclisp.Integer(4)},
}

func TestProg(t *testing.T) {
	progTests.Do(t)
}
