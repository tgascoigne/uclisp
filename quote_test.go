package uclisp_test

import (
	"testing"

	"github.com/tgascoigne/uclisp"
)

var quoteTests = BasicTests{
	{"`(1 ,(+ 1 1) ,@(list (+ 1 1 1) (+ 1 1 1 1)))", uclisp.List{uclisp.Integer(1), uclisp.Integer(2), uclisp.Integer(3), uclisp.Integer(4)}},
	{"'(+ 1 (+ 1 1))", uclisp.List{uclisp.Symbol("+"), uclisp.Integer(1), uclisp.List{uclisp.Symbol("+"), uclisp.Integer(1), uclisp.Integer(1)}}},
	{"`(+ 1 ,(+ 1 1))", uclisp.List{uclisp.Symbol("+"), uclisp.Integer(1), uclisp.Integer(2)}},
	{"(eval '(+ 1 (+ 1 1)))", uclisp.Integer(3)},
}

func TestQuote(t *testing.T) {
	quoteTests.Do(t)
}
