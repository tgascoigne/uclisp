package uclisp_test

import (
	"testing"

	"github.com/tgascoigne/uclisp"
)

var evalTests = BasicTests{
	{"(progn (backtrace))", uclisp.Nil},
	{"(eval '(+ 1 1))", uclisp.Integer(2)},
	{"(let ((x '(* 2 2))) (eval x))", uclisp.Integer(4)},
}

var evalExceptionTests = ExceptionTests{
	{"(eval)", uclisp.ErrArgCount},
	{"(eval foo bar)", uclisp.ErrArgCount},
}

func TestEval(t *testing.T) {
	evalTests.Do(t)
	evalExceptionTests.Do(t)
}
