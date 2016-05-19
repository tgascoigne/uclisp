package uclisp_test

import (
	"testing"

	"github.com/tgascoigne/uclisp"
)

var envTests = BasicTests{
	{"(let ((x 2)) x)", uclisp.Integer(2)},
	{"(let (x) x)", uclisp.Nil},
	{"(let () t)", uclisp.True},
	{"(let ((x 4)) (let ((x 2)) x))", uclisp.Integer(2)},
	{"(let ((x 2) (x 4)) x)", uclisp.Integer(4)},
	{"(let* ((x 2) (y (* x 2))) y)", uclisp.Integer(4)},
	{"(let ((x 20)))", uclisp.Nil},
	{"(progn (define x 20) (* x x))", uclisp.Integer(400)},
	{"(progn (define x 20) (let ((x 5)) x))", uclisp.Integer(5)},
	{"(progn (define x 20) (let ((y x)) x))", uclisp.Integer(20)},
	{"(progn (define x 20) (let ((x 5)) (set 'x 10) x))", uclisp.Integer(10)},
	{"(progn (define x 20) (set 'x 10) (let ((x 5)) x))", uclisp.Integer(5)},
	{"(progn (define x 20) (set 'x 10) (let ((x 5)) x) x)", uclisp.Integer(10)},
	{"(progn (define x 1) (defined x))", uclisp.True},
	{"(defined y)", uclisp.Nil},
	{"(progn (define x 1) (let (y) (defined x)))", uclisp.True},
	{"(progn (define x 1) (let (x) (defined x)))", uclisp.True},
}

var envExceptionTests = ExceptionTests{
	{"(let)", uclisp.ErrArgCount},
	{"(let*)", uclisp.ErrArgCount},
	{"(let ((x 2) (y (* x 2))) y)", uclisp.ErrSymbolNotDefined},
	{"(let t t)", uclisp.ErrNotAList},
	{"(let ((x 1 2)) x)", uclisp.ErrInvalidBindSpec},
	{"(let ((20 1)) x)", uclisp.ErrNotASymbol},
	{"(let (20) t)", uclisp.ErrNotASymbol},
	{"(define)", uclisp.ErrArgCount},
	{"(define foo)", uclisp.ErrArgCount},
	{"(define foo 1 2)", uclisp.ErrArgCount},
	{"(define 1 2)", uclisp.ErrNotASymbol},
	{"(defined)", uclisp.ErrArgCount},
	{"(defined foo bar)", uclisp.ErrArgCount},
	{"(defined 1)", uclisp.ErrNotASymbol},
	{"(let (foo) (set 'foo 1 1))", uclisp.ErrArgCount},
	{"(let (foo) (set 'foo))", uclisp.ErrArgCount},
	{"(let (foo) (set 'bar 1))", uclisp.ErrSymbolNotDefined},
	{"(let (foo) (set 1 1))", uclisp.ErrNotASymbol},
}

func TestEnvironment(t *testing.T) {
	envTests.Do(t)
	envExceptionTests.Do(t)
}
