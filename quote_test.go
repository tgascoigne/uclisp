package uclisp_test

import (
	"testing"

	"github.com/tgascoigne/uclisp"
)

var quoteTests = BasicTests{
	{"(progn `(* 2 2))", uclisp.List{uclisp.Symbol("*"), uclisp.Integer(2), uclisp.Integer(2)}},
	{"(quote (+ 1 1))", uclisp.List{uclisp.Symbol("+"), uclisp.Integer(1), uclisp.Integer(1)}},
	{"'(+ 1 1)", uclisp.List{uclisp.Symbol("+"), uclisp.Integer(1), uclisp.Integer(1)}},
	{"(let ((x '(+ 1 1))) `(* 2 ,x))", uclisp.List{uclisp.Symbol("*"), uclisp.Integer(2), uclisp.List{uclisp.Symbol("+"), uclisp.Integer(1), uclisp.Integer(1)}}},
	{"(let ((x '(2 2))) `(* 2 ,@x))", uclisp.List{uclisp.Symbol("*"), uclisp.Integer(2), uclisp.Integer(2), uclisp.Integer(2)}},
	{"(eval (let ((x '(2 2))) `(* 2 ,@x)))", uclisp.Integer(8)},
}

var quoteExceptionTests = ExceptionTests{
	{"(quote)", uclisp.ErrArgCount},
	{"(quote 1 1)", uclisp.ErrArgCount},
	{"(backquote)", uclisp.ErrArgCount},
	{"(backquote 1 1)", uclisp.ErrArgCount},
	{"(backquote 1)", uclisp.ErrNotAList},
	{"`(+ ,@\"x\")", uclisp.ErrNotAList},
}

func TestQuote(t *testing.T) {
	quoteTests.Do(t)
	quoteExceptionTests.Do(t)
}
