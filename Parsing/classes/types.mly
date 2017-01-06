%{
	
%}
%%
%public types:
	| id= IDENTIFIER {T_Identifier id}
	| FLOAT {T_Float}
	| BOOLEAN {T_Boolean}
	| BYTE {T_Byte}
	| CHAR {T_Char}
	| INT {T_Int}
	| LONG {T_Long}
	| SHORT {T_Short}
	| DOUBLE {T_Double}
	| id=IDENTIFIER t=type_generic_impl { T_Generic(id*t)}

type_generic_impl:
	| LANG t=types RANG {t}

%%

