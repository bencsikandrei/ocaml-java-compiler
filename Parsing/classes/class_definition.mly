%{

%}

%start javaClass
%start <string> javaClass

%%
javaClass:
	| c=j_class EOF { "" } (* for testing *)

%public j_class:
	| modif=option(class_modifs) CLASS id=IDENTIFIER 
		tp=option(type_params_defin) sup=option(super) 
		interf=option(interfaces) bod=class_body { }

class_modifs:
	| m=class_modif { }
	| m=class_modifs ms=class_modif { }

class_modif:
	| ABSTRACT { }
	| PUBLIC { }
	| PRIVATE { }
	| PROTECTED { }
	| ANOT id=IDENTIFIER { }
	| STATIC { }
	| FINAL { }
	| STRICTFP { }

super:
	| EXTENDS id=IDENTIFIER typ=option(type_params_defin) { }

interfaces:
	| IMPLEMENTS i=interface_list { }

interface_list:
	| id=IDENTIFIER { }
	| id=IDENTIFIER COMM l=interface_list { }

class_body:
	| SEMI { }
	| LCURL inside_class RCURL { }
	| LCURL RCURL { }

inside_class_l:
	| inside_class { }
	| inside_class_l inside_class { }

inside_class:
	(* | javaMethod { } *)
	| attribute { }
	| javaClass { }

attribute:
	| exprs { }


%%