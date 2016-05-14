package uclisp_test

import (
	"testing"

	"github.com/tgascoigne/uclisp"
)

var envTests = BasicTests{
	{"(let ((x 2)) x)", uclisp.Integer(2)},
	{"(let (x) x)", uclisp.Nil},
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
}

func TestEnvironment(t *testing.T) {
	envTests.Do(t)
}
