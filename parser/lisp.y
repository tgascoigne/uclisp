%{
package parser

import "lisp/ast"
%}

%union {
    prog ast.List
    form ast.Form
    sym ast.Symbol
    ival int
}

%token '(' ')'

%token <sym> tSymbol
%token <ival> tIntAtom

%token tWhitespace
%token tEOL

%type <prog> list members
%type <form> form

%start prog

%%

prog
    : form
      { yylex.(*Lexer).Ast([]ast.Form{$1}) }
    ;

form
    : list
      {
        $$ = ast.FuncForm{$1[0], $1[1:]}
      }
    | tSymbol
      { $$ = ast.Form($1) }
    | tIntAtom
      { $$ = ast.Integer($1) }
    ;

list
    : '(' members ')'
      { $$ = $2 }
    ;

members
    : ws✳
      { $$ = make(ast.List, 0) }
    | members form
      { $$ = append($$, $2) }
    | form
      {
        $$ = make(ast.List, 0)
        $$ = append($$, $1)
      }
    ;

ws
	: tWhitespace
	;

ws＋
	: ws
	| ws ws＋

ws✳
	: /* empty */
	| ws＋

%%
