%{
	
%}

%start javaMethods
%start <string> javaMethods

%%
javaMethods:
	|head=javaMethod EOF {head}
	|head=javaMethod tail=javaMethods {head^"\n"^tail}

javaMethod:
	|m=methodPrivacy {"Method: "^m}
	|ANOT an=IDENTIFIER m=methodPrivacy {an^" Method: "^m}

methodPrivacy:
	| PUBLIC m=methodGroup {"Public "^m}
	| PRIVATE m=methodGroup {"Private "^m}
	| PROTECTED m=methodGroup {"Protected "^m}

methodGroup:
	| m=methodStaticity {m}
	| name=methodNameAndParams body=methodBodyReturnVoid {name^" {"^body^"}"} (* constructor *)

methodStaticity:
	| m=methodBase {m}
	| STATIC m=methodBase {"static "^m}

methodBase:
	| VOID name=methodNameAndParams body=methodBodyReturnVoid {name^" {"^body^"}"}
	| typ=types name=methodNameAndParams body=methodBodyReturnNonVoid {name^" {"^body^"} return type:"^typ}

methodNameAndParams:
	| name=IDENTIFIER LPAR RPAR {name}
	| name=IDENTIFIER LPAR ats=attrs RPAR {name^" -> "^ats}

methodBodyReturnNonVoid:
	| LCURL RETURN e=expr SEMI RCURL {" -> "^e}
	| LCURL e1=exprs RETURN e2=expr SEMI RCURL {e1^" -> "^e2}

methodBodyReturnVoid:
	| LCURL RCURL {""}
	| LCURL e=exprs RCURL {e}

attrs:
	|at1=attr {at1}
	|at1=attr COMM ats=attrs {at1^","^ats}

attr:
	|typ=types name=IDENTIFIER {typ^":"^name}
 
exprs:
	|e1=expr SEMI {e1^";"}(* replace with real exprs from other team*)
	|e1=expr SEMI e2=exprs {e1^";\n"^e2} 

types:
	| id= IDENTIFIER {id}
	| num= FLOATLIT {string_of_float num}
	| str=STRLIT {str}
	| i=INTLIT {string_of_int i}

expr:
	| id= IDENTIFIER {id}
	| num= FLOATLIT {string_of_float num}
	| e1=expr MINUS e2=expr {"menos("^e1^","^e2^")"}
	| e1=expr PLUS e2=expr {"mas("^e1^","^e2^")"}
	| e1=expr DIV e2=expr {"div("^e1^","^e2^")"}
	| e1=expr MOD e2=expr {"mod("^e1^","^e2^")"}

%%
