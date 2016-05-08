%{
package uclisp

%}

%union {
    prog Prog
    elem Elem
}

%token '(' ')' '\''

%token <elem> tSymbol tIntAtom tStringAtom

%token tWhitespace
%token tEOL

%type <elem> elem
%type <prog> list_contents

%start begin

%%

begin
	: elem
      { yylex.(*Lexer).Ast($1) }
	;

elem
	: '(' list_contents ')'
      { $$ = $2 }
	| '\'' '(' list_contents ')'
      { $$ = append(Prog{ Symbol("quote") }, $3...) }
    | tSymbol
    | tIntAtom
    | tStringAtom
    ;

list_contents
	: ws✳
      { $$ = make(Prog, 0) }
    | list_contents elem
      { $$ = append($$, $2) }
    | elem
      { $$ = Prog{$1} }
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
