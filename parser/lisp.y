%{
package parser

import "github.com/tgascoigne/uclisp/ast"
%}

%union {
    list ast.List
    form ast.Form
    sym ast.Symbol
    str string
    ival int
}

%token '(' ')'

%token <sym> tSymbol
%token <ival> tIntAtom
%token <str> tStringAtom

%token tWhitespace
%token tEOL

%type <list> list
%type <form> form quoted_form

%start prog

%%

prog
    : form
      { yylex.(*Lexer).Ast($1) }
    ;

form
    : '(' list ')'
      { $$ = ast.ListForm($2) }
	| quoted_form
    | tSymbol
      { $$ = ast.Form($1) }
    | tIntAtom
      { $$ = ast.Integer($1) }
    | tStringAtom
      { $$ = ast.String($1) }
    ;

quoted_form
	: '\'' '(' list ')'
      { $$ = ast.Quoted{$3} }
    | '\'' tSymbol
      { $$ = ast.Quoted{$2}}
	;

list
    : ws✳
      { $$ = make(ast.List, 0) }
    | list form
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
