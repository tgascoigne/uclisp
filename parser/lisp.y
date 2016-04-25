%{
package parser

import "lisp/ast"
%}

%union {
    prog ast.List
    expr ast.Expression
    sym ast.Symbol
    ival int
}

%token '(' ')'

%token <sym> tSymbol
%token <ival> tIntAtom

%token tWhitespace
%token tEOL

%type <prog> list members
%type <expr> expr

%start prog

%%

prog
    : expr
      { yylex.(*Lexer).Ast([]ast.Expression{$1}) }
    ;

expr
    : list
      {
        $$ = ast.Form{$1[0], $1[1:]}
      }
    | tSymbol
      { $$ = ast.Expression($1) }
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
    | members expr
      { $$ = append($$, $2) }
    | expr
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
