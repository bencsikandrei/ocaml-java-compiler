%{
	
%}
%%
%public j_interface:
	| i=normal_interface { i }
	| i=AnnotationTypeDeclaration { i }

normal_interface:
	| modif=option(modifiers) INTERFACE 
		id=IDENTIFIER tp=option(type_params_defin) 
		sup=option(super_int) bod=interf_body { 
			let modif = match modif with | None -> [] | Some m -> m in
			let tp = match tp with | None -> [] | Some tp -> tp in
			let sup = match sup with | None -> [] | Some sup -> sup in
				{imodifiers=modif;
				iidentifier=id;
				itparam=tp;
				iparent=sup;
				ibody=bod} 
		}

super_int:
	| EXTENDS id=IDENTIFIER typ=option(type_params_defin) {
		match typ with | None -> (id,None)::[] | Some typ -> (id,Some typ)::[]
		}
	| sup=super_int COMM id=IDENTIFIER typ=option(type_params_defin) {
		match typ with | None -> (id,None)::sup | Some typ -> (id,Some typ)::sup
		}

interf_body:
	| LCURL b=option(interf_member_decls) RCURL {
		match b with | None -> [] | Some b -> b
		}

interf_member_decls:
	| d=interf_member_decl { d::[] }
	| d=interf_member_decl l=interf_member_decls { d::l }

interf_member_decl:
	/* ConstantDeclaration
	AbstractMethodDeclaration */
	| nim=NotImplMethod { II_Method nim }
	| c=j_class { II_Class c }
	| i=j_interface { II_Interface i }

%%