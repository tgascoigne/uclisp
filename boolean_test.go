package uclisp_test

import (
	"testing"

	"github.com/tgascoigne/uclisp"
)

var boolTests = BasicTests{
	{"(not nil)", uclisp.True},
	{"(not t)", uclisp.Nil},
	{"(cond (t))", uclisp.True},
	{"(cond (t (+ 1 1)))", uclisp.Integer(2)},
	{"(cond ((= 1 1) (+ 1 1)) ((= 1 2) (+ 2 2)))", uclisp.Integer(2)},
	{"(cond ((= 1 2) (+ 1 1)) ((= 1 1) (+ 2 2)))", uclisp.Integer(4)},
	{"(cond ((= 1 2) (+ 1 1)) ((= 1 3) (+ 2 2)))", uclisp.Nil},
	{"(cond ((= 1 1) (+ 1 1) (+ 2 2)) ((= 1 3) (+ 4 4)))", uclisp.Integer(4)},
	{"(cond (nil (+ 1 1) (+ 2 2)) ((> 1 3) (+ 4 4)))", uclisp.Integer(8)},
	{"(cond ((eq '(1 1) '(1 2)) \"hello\") ((eq '(1 1) '(1 1))))", uclisp.String("hello")},
	{"(cond ((eq \"a\" \"a\") \"a\"))", uclisp.String("a")},
}

func TestBoolean(t *testing.T) {
	boolTests.Do(t)
}
