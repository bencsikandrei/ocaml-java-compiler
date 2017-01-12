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
	| m=modifiers cma=class_method_or_attribute { match cma with | IC_Method me -> me.jmmodifiers<-m; cma | _ -> cma }
	| cma=class_method_or_attribute { cma }

class_method_or_attribute:
	| m=method_or_attribute {m}
	| j_class_plain {IC_Class }

method_or_attribute:
	| t=type_params_defin j=javaMethod_plain_return {j.jmtparam<-t;IC_Method j}
	| VOID j=javaMethod_plain {IC_Method j}
	| types variable_decls {IC_Attribute }
	| t=types j=javaMethod_plain {j.jmrtype<-RT_Type t; IC_Method j}


%public j_class_plain:
	| CLASS id=IDENTIFIER 
		tp=option(type_params_defin) sup=option(super) 
		interf=option(interfaces) bod=class_body { }


(* attribute_plain:
	types variable_decls { }

inside_class:
	| javaMethod { }
	| j_class { }
	| attribute SEMI { } 

attribute:
	| option(modifiers) types variable_decls { } *)

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
	| expr { }

%%
