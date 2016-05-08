%{
package ast

%}

%union {
    prog Prog
    list List
    form Form
    value Value
    sym Symbol
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
%type <form> form
%type <value> value
%type <prog> prog

%start begin

%%

begin
	: prog
      { yylex.(*Lexer).Ast($1) }
	;

prog
	: ws✳
      { $$ = make(Prog, 0) }
    | prog form
      { $$ = append($$, $2) }
    | form
      { $$ = Prog{$1} }
    ;

form
    : '(' list ')'
      { $$ = ListForm($2) }
    | tSymbol
      { $$ = Form($1) }
    | '\'' form
      { $$ = ListForm{Symbol("quote"), $2} }
    ;

value
	: tIntAtom
      { $$ = Integer($1) }
    | tStringAtom
      { $$ = String($1) }
	;

list
    : ws✳
      { $$ = make(List, 0) }
    | list form
      { $$ = append($$, $2) }
    | form
      {
        $$ = make(List, 0)
        $$ = append($$, $1)
      }
    ;

(defun test (v) (message "%s" v))

(test (car '((+ 1 1) 2)))

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
