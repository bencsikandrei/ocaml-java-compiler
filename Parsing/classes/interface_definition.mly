%{
	
%}
%%
%public j_interface:
	| normal_interface { }
	/*| annotation_decl { }*/

normal_interface:
	| option(modifiers) INTERFACE Interface_type option(super_int) interf_body { }

super_int:
	| EXTENDS Interface_type { }
	| super_int COMM Interface_type { }

Interface_type:
	| IDENTIFIER option(type_params_defin) { }

interf_body:
	| LCURL option(interf_member_decls) RCURL { }

interf_member_decls:
	| interf_member_decl { }
	| interf_member_decls interf_member_decl { }

interf_member_decl:
	/* ConstantDeclaration
	AbstractMethodDeclaration */
	| j_class { }
	| j_interface { }

%%