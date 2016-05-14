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
	{"(car (list))", uclisp.Nil},
	{"(cdr (list))", uclisp.Nil},
	{"(last (list))", uclisp.Nil},
	{"(butlast (list))", uclisp.Nil},
	{"(car (list (+ 1 1) (+ 2 2) (+ 3 3)))", uclisp.Integer(2)},
	{"(cdr (list (+ 1 1) (+ 2 2) (+ 3 3)))", uclisp.List{uclisp.Integer(4), uclisp.Integer(6)}},
	{"(last (list (+ 1 1) (+ 2 2) (+ 3 3)))", uclisp.List{uclisp.Integer(6)}},
	{"(butlast (list (+ 1 1) (+ 2 2) (+ 3 3)))", uclisp.List{uclisp.Integer(2), uclisp.Integer(4)}},
	{"(let ((l1 '(1 2 3)) (l2 '(4 5 6))) (append l1 l2))", uclisp.List{uclisp.Integer(1), uclisp.Integer(2), uclisp.Integer(3), uclisp.Integer(4), uclisp.Integer(5), uclisp.Integer(6)}},
	{"(let ((l2 '(4 5 6))) (append '(1 2 3) l2))", uclisp.List{uclisp.Integer(1), uclisp.Integer(2), uclisp.Integer(3), uclisp.Integer(4), uclisp.Integer(5), uclisp.Integer(6)}},
	{"(let ((l2 '(4 5 6))) (append l2))", uclisp.List{uclisp.Integer(4), uclisp.Integer(5), uclisp.Integer(6)}},
}

func TestList(t *testing.T) {
	listTests.Do(t)
}
