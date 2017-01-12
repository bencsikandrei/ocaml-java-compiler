%{
	open Expressions

%}
%%
%public types:
	| FLOAT {T_Float}
	| BOOLEAN {T_Boolean}
	| BYTE {T_Byte}
	| CHAR {T_Char}
	| INT {T_Int}
	| LONG {T_Long}
	| SHORT {T_Short}
	| DOUBLE {T_Double}
	| id=IDENTIFIER t=type_generic_impl {T_Generic(id,t)}
	
type_generic_impl:
	| LANG t=types RANG {t}


/* typeName */
%public typeName:
	| pri=primitiveType { Primitive(pri) }
	| qn=qualifiedName { Qualified(qn) }
	
;

%public qualifiedName:
	| id=IDENTIFIER { id }
	| qn=qualifiedName DOT id=IDENTIFIER { qn^"."^id }

%public typeSpecifier:
	tn=typeName { tn }
	| tn=typeName ds=dims { ArrayType(tn,ds) }
;

%public dims:
	DIM { " [ ] " }
	| ds=dims DIM { ds^" [ ] " }
;

%public primitiveType:
	| BOOLEAN { P_Boolean  }
	| CHAR { P_Char  }
	| BYTE { P_Byte  }
	| SHORT { P_Short  }
	| INT { P_Int  }
	| LONG { P_Long  }
	| FLOAT { P_Float  }
	| DOUBLE { P_Double }
	| VOID { P_Void }
;

%%

