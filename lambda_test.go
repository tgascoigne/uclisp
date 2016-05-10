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
}

func TestLambda(t *testing.T) {
	lambdaTests.Do(t)
}
