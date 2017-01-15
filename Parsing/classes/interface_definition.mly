%{
	
%}
%%
%public j_interface:
	| i=interface_plain { JI_IN i }
	| i=AnnotationTypeDeclarations {JI_AN i }

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

interface_plain:
	| INTERFACE id=IDENTIFIER tp=option(type_params_defin) 
		sup=option(super_int) bod=interf_body { 
			let tp = match tp with | None -> [] | Some tp -> tp in
			let sup = match sup with | None -> [] | Some sup -> sup in
				{imodifiers=[];
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
	| l=interf_member_decls d=interf_member_decl { l@[d] }

interf_member_decl:
	(*| modif=modifiers lvd=localVariableDeclStmt { II_Field(lvd) } /* default public static final attributes that must be initialized */*)
	| nim=NotImplMethod { II_Method nim }
	| c=j_class { II_Class c }
	| i=j_interface { II_Interface i }

%%