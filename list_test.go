package uclisp_test

import (
	"testing"

	"github.com/tgascoigne/uclisp"
)

var listTests = BasicTests{
	{"(list 1 2 3 4)", uclisp.List{uclisp.Integer(1), uclisp.Integer(2), uclisp.Integer(3), uclisp.Integer(4)}},
	{"(list 1 2 3 (+ 2 2))", uclisp.List{uclisp.Integer(1), uclisp.Integer(2), uclisp.Integer(3), uclisp.Integer(4)}},
	{"(list 'abc 'def 123)", uclisp.List{uclisp.Symbol("abc"), uclisp.Symbol("def"), uclisp.Integer(123)}},
	{"(list 'abc (list 'def 123))", uclisp.List{uclisp.Symbol("abc"), uclisp.List{uclisp.Symbol("def"), uclisp.Integer(123)}}},
	{"(list)", uclisp.Nil},
	{"()", uclisp.Nil},
}

func TestList(t *testing.T) {
	listTests.Do(t)
}
