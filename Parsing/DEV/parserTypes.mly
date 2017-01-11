%{
	open Expressions
%}
/*
%start primitiveType
%type < Expressions.primitive > primitiveType

%start typeSpecifier
%type < Expressions.types > typeSpecifier
*/
%%

/* types */
%public primitiveType:
	BOOLEAN { P_Boolean  }
	| CHAR { P_Char  }
	| BYTE { P_Byte  }
	| SHORT { P_Short  }
	| INT { P_Int  }
	| LONG { P_Long  }
	| FLOAT { P_Float  }
	| DOUBLE { P_Double }
	| VOID { P_Void }
;

/* typeName */
%public typeName:
	pri=primitiveType { Primitive(pri) }
	| qn=qualifiedName { Qualified(qn) }
;

%public typeSpecifier:
	tn=typeName { tn }
	| tn=typeName ds=dims { ArrayType(tn,ds) }
;

%public dims:
	DIM { " [ ] " }
	| ds=dims DIM { ds^" [ ] " }
;
%%
