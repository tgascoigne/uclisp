package ast

import "bytes"

//go:generate nex -e lisp.nex
//go:generate go tool yacc -o lisp.y.go lisp.y

type result struct {
	Ast Form
}

func (l *Lexer) Ast(prog Form) {
	l.parseResult.(*result).Ast = prog
}

func (l *Lexer) Error(err string) {}

func Parse(filename, source string) Form {
	result := &result{}
	lexer := NewLexerWithInit(bytes.NewBufferString(source), func(lex *Lexer) {
		lex.parseResult = result
	})

	yyErrorVerbose = true
	_ = yyParse(lexer)
	return result.Ast
}
