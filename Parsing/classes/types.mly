%{
open Ast
%}
%%

%public primitiveType:
	| FLOAT {PT_Float}
	| BOOLEAN {PT_Boolean}
	| BYTE {PT_Byte}
	| CHAR {PT_Char}
	| INT {PT_Int}
	| LONG {PT_Long}
	| SHORT {PT_Short}
	| DOUBLE {PT_Double}

%public types: /* Names of classes, generics, primitive types*/
	| pri=primitiveType { T_Primitive pri  }
	| qn=qualifiedName { T_Qualified qn } 

%public dims: /* brackets can be more than one set */
	| DIM { 1 }
	| ds=dims DIM { ds+1 }

%public allTypes:
	| t=types {AL_Types t}
	| a=arrayTypes {a}

%public arrayTypes: /* types with dimensions */ 
	| tn=types ds=dims { AL_Array(tn,ds) } 
	
definedType: /* identifier or generic definition*/
	| id=IDENTIFIER { DT_Id id }
	| id=IDENTIFIER LGEN t=types RGEN { DT_Generic(id,t) }

%public qualifiedName: /* name or name.name*/
	| id=definedType { [id] } 
	| qn=qualifiedName DOT id=definedType { qn@[id]}
%%

