%{
	
%}
%%
%public types:
	| id=IDENTIFIER {id}
	| FLOAT {"float"}
	| BOOLEAN {"bool"}
	| BYTE {"byte"}
	| CHAR {"char"}
	| INT {"int"}
	| LONG {"long"}
	| SHORT {"short"}
	| DOUBLE {"double"}
	| id=IDENTIFIER type_generic_impl { id^"< >" }

type_generic_impl:
	| LANG types RANG {}
%%

