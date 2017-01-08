%{
open Definitions
%}

%start javaClass
%start <abstractSyntaxTree> javaClass

%%
javaClass:
	| c=j_class EOF { STR "" } (* for testing *)

%public j_class:
	| modif=option(modifiers) CLASS id=IDENTIFIER 
		tp=option(type_params_defin) sup=option(super) 
		interf=option(interfaces) bod=class_body { }

super:
	| EXTENDS id=IDENTIFIER typ=option(type_params_defin) { }

interfaces:
	| IMPLEMENTS i=interface_list { }

interface_list:
	| id=IDENTIFIER { }
	| id=IDENTIFIER COMM l=interface_list { }

class_body:
	| SEMI { }
	| LCURL inside_class_l RCURL { }
	| LCURL RCURL { }

inside_class_l:
	| inside_class { }
	| inside_class_l inside_class { }

inside_class:
	| javaMethod { }
	| j_class { }
(*	| attribute SEMI { }

attribute:
	| option(modifiers) types variable_decls { } 

variable_decls:
	| var_decl { }
	| variable_decls COMM var_decl { }

var_decl:
 	| var_decl_id { }
 	| var_decl_id ASSIGN var_init { }

var_decl_id:
	| IDENTIFIER { }
	| var_decl_id LBRAC RBRAC { }

var_init:
	| exprs { } *)

%%
