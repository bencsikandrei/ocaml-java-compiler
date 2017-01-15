%{
	
%}
%%
%public j_interface:
	| i=j_interface_plain { JI_IN i }
	| i=AnnotationTypeDeclarations {JI_AN i }

/*
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
*/

%public j_interface_plain:
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
	/*| modif=modifiers  }  default public static final attributes that must be initialized */
	| modifs=modifiers t=tmp {
		let tmp var= match var with
			|JI_IN a-> a.imodifiers<-modifs;
			|JI_AN a-> a.iaModifiers<-modifs;
		in

		match t with 
		| II_Class a -> 
			(match a with
				| JavClass a -> a.cmodifiers<-modifs; t
				| JavEnum e -> e.emodifiers<-modifs; t)
	 	| II_Method a -> a.jmmodifiers<-modifs; t
		| II_Interface a ->  tmp a; t
		| II_Atr f -> f.attrmodifiers<- modifs;t
		
		
	
	 }
	| t=tmp {t}

tmp:
	| t=type_params_defin m=tmp2 { 
		match m with 
		| II_Class a -> 
			(match a with
				| JavClass a -> a.ctparam<-t;m
				| JavEnum e -> m)
	 	| II_Method a -> a.jmtparam<-t;m
	 	| _ ->m
	 }
	| m=tmp2 {m}
	| i=j_interface { II_Interface  i }
	| lvd=fieldVariableDeclaration { II_Atr(lvd) }

tmp2:
	| nim=NotImplMethod_plain { II_Method nim } 
	| c=j_class_plain { II_Class c }

%%