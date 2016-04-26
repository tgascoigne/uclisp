%{
package parser

import "lisp/ast"
%}

%union {
    list ast.List
    form ast.Form
    sym ast.Symbol
    ival int
}

%token '(' ')'

%token <sym> tSymbol
%token <ival> tIntAtom

%token tWhitespace
%token tEOL

%type <list> list_members
%type <form> form

%start prog

%%

prog
    : form
      { yylex.(*Lexer).Ast($1) }
    ;

form
    : '(' list_members ')'
      { $$ = $2 }
    | tSymbol
      { $$ = ast.Form($1) }
    | tIntAtom
      { $$ = ast.Integer($1) }
    ;

list_members
    : ws✳
      { $$ = make(ast.List, 0) }
    | list_members form
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
