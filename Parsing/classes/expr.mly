%{
	
%}

%right  MINUS PLUS
%right DIV MOD

%%
%public exprs:
	| expr SEMI  {""}
	| expr SEMI exprs  {""}

expr:
	| id= IDENTIFIER {id}
	| num= FLOATLIT {string_of_float num}
	| e1=expr MINUS e2=expr {"menos("^e1^","^e2^")"}
	| e1=expr PLUS e2=expr {"mas("^e1^","^e2^")"}
	| e1=expr DIV e2=expr {"div("^e1^","^e2^")"}
	| e1=expr MOD e2=expr {"mod("^e1^","^e2^")"}
%%
