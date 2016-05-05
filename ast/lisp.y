%{
package ast

%}

%union {
    prog Prog
    list List
    form Form
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
%type <form> form quoted_form
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
	| quoted_form
    | tSymbol
      { $$ = Form($1) }
    | tIntAtom
      { $$ = Integer($1) }
    | tStringAtom
      { $$ = String($1) }
    ;

quoted_form
	: '\'' '(' list ')'
      { $$ = Quoted{$3} }
    | '\'' tSymbol
      { $$ = Quoted{$2}}
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
