%{
	open Definitions
%}

%start javaClass
%start <abstractSyntaxTree> javaClass

%%
javaClass:
	| c=j_class EOF { STR "" } /* for testing */

%public j_class:
	| modif=option(modifiers) CLASS id=IDENTIFIER 
		tp=option(type_params_defin) sup=option(super) 
		interf=option(interfaces) bod=class_body { 
			let modif = match modif with | None -> [] | Some m -> m in
			let tp = match tp with | None -> [] | Some tp -> tp in
			let sup = match sup with | None -> C_Object | Some sup -> sup in
			let interf = match interf with | None -> [] | Some interf -> interf in
				{cmodifiers=modif;
				cidentifier=id;
				ctparam=tp;
				cparent=sup;
				cinterfaces=interf;
				cbody=bod} 
		}

super:
	| EXTENDS id=IDENTIFIER typ=option(type_params_defin) {
		match typ with | None -> C_Parent(id,None) | Some typ -> C_Parent(id,Some typ)
	}

interfaces:
	| IMPLEMENTS i=interface_list { i }

interface_list:
	| id=IDENTIFIER { id::[] }
	| id=IDENTIFIER COMM l=interface_list { id::l }

class_body:
	| SEMI { [IC_Semi] }
	| LCURL bod=inside_class_l RCURL { bod }
	| LCURL RCURL { [IC_Empty] }

inside_class_l:
	| i=inside_class { i::[] }
	| i=inside_class l=inside_class_l { i::l }

inside_class:
	| m=modifiers cma=class_method_or_attribute { match cma with | IC_Method me -> me.jmmodifiers<-m; cma |IC_Class c -> c.cmodifiers<-m; cma | _ -> cma }
	| 			  cma=class_method_or_attribute { cma }

class_method_or_attribute:
	| m=method_or_attribute { m }
	| c=j_class_plain { IC_Class c }

method_or_attribute:
	| t=type_params_defin j=javaMethod_plain_return { j.jmtparam<-t;IC_Method j }
	| VOID j=javaMethod_plain { IC_Method j }
	| types variable_decls { IC_Attribute }
	| t=allTypes j=javaMethod_plain { j.jmrtype<-RT_Type t; IC_Method j }

%public j_class_plain:
	| CLASS id=IDENTIFIER 
		tp=option(type_params_defin) sup=option(super) 
		interf=option(interfaces) bod=class_body {
			let tp = match tp with | None -> [] | Some tp -> tp in
			let sup = match sup with | None -> C_Object | Some sup -> sup in
			let interf = match interf with | None -> [] | Some interf -> interf in
				{cmodifiers=[];
				cidentifier=id;
				ctparam=tp;
				cparent=sup;
				cinterfaces=interf;
				cbody=bod} 
		}


/* attribute_plain:
	types variable_decls { }

inside_class:
	| javaMethod { }
	| j_class { }
	| attribute SEMI { } 

attribute:
	| option(modifiers) types variable_decls { } */

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
	| expression { }

%%
