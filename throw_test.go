package uclisp_test

import (
	"testing"

	"github.com/tgascoigne/uclisp"
)

var throwTests = BasicTests{
	{"(catch 'foo (catch 'bar (throw 'foo 1)))", uclisp.Integer(1)},
	{"(catch 'foo (let ((x 20)) (throw 'foo 1) x))", uclisp.Integer(1)},
	{"(let ((x 20)) (catch 'foo (throw 'foo 20) (setq x 1)) x)", uclisp.Integer(20)},
	{"(catch* 'val (throw 'foo 1) (progn (* val 2)))", uclisp.Integer(2)},
}

var throwExceptionTests = ExceptionTests{
	{"(catch)", uclisp.ErrArgCount},
	{"(catch 'foo)", uclisp.ErrArgCount},
	{"(catch 1 (throw 'foo 1))", uclisp.ErrNotASymbol},
	{"(throw)", uclisp.ErrArgCount},
	{"(throw 'foo)", uclisp.ErrArgCount},
	{"(throw 1 1)", uclisp.ErrNotASymbol},
	{"(catch*)", uclisp.ErrArgCount},
	{"(catch* 'val)", uclisp.ErrArgCount},
	{"(catch* 'val (throw 'foo 1))", uclisp.ErrArgCount},
	{"(catch* 1 (throw 'foo 1) 1)", uclisp.ErrNotASymbol},
}

func TestThrow(t *testing.T) {
	throwTests.Do(t)
	throwExceptionTests.Do(t)
}
