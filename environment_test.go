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
}

func TestEnvironment(t *testing.T) {
	envTests.Do(t)
}
