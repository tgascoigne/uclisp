package uclisp_test

import (
	"testing"

	"github.com/tgascoigne/uclisp"
)

var lambdaTests = BasicTests{
	{"((lambda (x) (* x x)) 2)", uclisp.Integer(4)},
	{"((lambda () 8))", uclisp.Integer(8)},
	{"((lambda (x y z) (+ x y z)) 1 2 3)", uclisp.Integer(6)},
	{"(progn (define square (lambda (x) (* x x))) (square (square 2)))", uclisp.Integer(16)},
	{"(progn (define x 40) (define foo (lambda (x) x)) (foo 20))", uclisp.Integer(20)},
	{"(progn (define x 40) (define foo (lambda (x) x)) (foo x))", uclisp.Integer(40)},
	{"(progn (define x 40) (define foo (lambda () x)) (foo))", uclisp.Integer(40)},
	{"((lambda (x &optional y) y) 1)", uclisp.Nil},
	{"((lambda (x &optional y) y) 1 2)", uclisp.Integer(2)},
	{"((lambda (&optional y) y) 2)", uclisp.Integer(2)},
	{"((lambda (x &optional y &rest z) z) 1 2 3 4 5)", uclisp.List{uclisp.Integer(3), uclisp.Integer(4), uclisp.Integer(5)}},
	{"((lambda (&optional y &rest z) z) 1 2 3 4 5)", uclisp.List{uclisp.Integer(2), uclisp.Integer(3), uclisp.Integer(4), uclisp.Integer(5)}},
	{"((lambda (&rest z) z) 1 2 3 4 5)", uclisp.List{uclisp.Integer(1), uclisp.Integer(2), uclisp.Integer(3), uclisp.Integer(4), uclisp.Integer(5)}},
}

func TestLambda(t *testing.T) {
	lambdaTests.Do(t)
}
