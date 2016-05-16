%{
package uclisp

%}

%union {
    list List
    elem Elem
}

%token '(' ')' '\'' '`' ',' '@'

%token <elem> tSymbol tIntAtom tStringAtom

%token tWhitespace
%token tEOL

%type <elem> elem
%type <list> list_contents

%start begin

%%

begin
	: list_contents
    { yylex.(*Lexer).Ast(append(List{Symbol("progn")}, $1...)) }
	;

elem
	: '\'' elem
      { $$ = List{ Symbol("quote"), $2 } }
	| '`' '(' list_contents ')'
      { $$ = List{ Symbol("backquote"), List($3) } }
	| ',' elem
      { $$ = List{ Symbol(","), $2 } }
	| ',' '@' elem
      { $$ = List{ Symbol(",@"), $3 } }
	| '(' list_contents ')'
      { $$ = List($2) }
    | tSymbol
    | tIntAtom
    | tStringAtom
    ;

list_contents
	: ws✳
      { $$ = make(List, 0) }
    | list_contents elem
      { $$ = append($$, $2) }
    | elem
      { $$ = List{$1} }
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
