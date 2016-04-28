package parser

import (
	"bytes"

	"github.com/tgascoigne/uclisp/ast"
)

//go:generate nex -e lisp.nex
//go:generate go tool yacc -o lisp.y.go lisp.y

type result struct {
	Ast ast.Form
}

func (l *Lexer) Ast(prog ast.Form) {
	l.parseResult.(*result).Ast = prog
}

func (l *Lexer) Error(err string) {}

func Parse(filename, source string) ast.Form {
	result := &result{}
	lexer := NewLexerWithInit(bytes.NewBufferString(source), func(lex *Lexer) {
		lex.parseResult = result
	})

	yyErrorVerbose = true
	_ = yyParse(lexer)
	return result.Ast
}
