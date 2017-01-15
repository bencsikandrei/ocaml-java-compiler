%{
	
%}

%%

%public AnnotationTypeDeclaration:
	|m=option(modifiers) ANOT i=IDENTIFIER b=AnnotationTypeBody { let m = match m with | Some m -> m | None ->[] in   {iaModifiers=m; iaName=i; ibody=b} }

AnnotationTypeBody:
	| LCURL RCURL { [] }
	| LCURL a=AnnotationTypeElementDeclarations RCURL {a}

AnnotationTypeElementDeclarations:
	| e=AnnotationTypeElementDeclaration {e::[]}
	| e=AnnotationTypeElementDeclaration es=AnnotationTypeElementDeclarations {e::es}

AnnotationTypeElementDeclaration:
	| m=option(modifiers) t=allTypes i=IDENTIFIER d=option(DefaultValue) SEMI { let m = match m with | Some m ->m | None [] in ATED_Basic {atedModifs=m;  atedType=t;atedName=i; default=d } }
	| modifiers fieldVariableDeclaration {ATED_Declar}(* TODO *)
	| e=j_class {ATED_Class e}
	| e=j_interface { ATED_Inter e}
	| e=AnnotationTypeDeclaration {ATED_Annot e}
	| SEMI {ATED_None}

 DefaultValue:
	| DEFAULT e=ElementValue {e}


%public Annotation:
	| a=NormalAnnotation {a}
	| a=MarkerAnnotation {a}
	| a=SingleElementAnnotation {a}
 
NormalAnnotation:
	| ANOT q=qualifiedName LPAR e=option(ElementValuePairs) RPAR { let e=match e with | Some e -> e|None ->[] in {aName=q;aElemPairs=e}  }

MarkerAnnotation:
	| ANOT q=qualifiedName {  {aName=q;aElemPairs=[]} }

SingleElementAnnotation:
	| ANOT q=qualifiedName LPAR e=ElementValue RPAR {   {aName=q;aElemPairs=[{evpId="value" ; evpValue=e}] } }

ElementValuePairs:
	| e=ElementValuePair {e::[]}
	| e=ElementValuePair es=ElementValuePairs {e::es} 

ElementValuePair:
	| i=IDENTIFIER ASSIGN e=ElementValue { {evpId=i ; evpValue=e} }

ElementValue:
	| c=conditionalExpression {EV_Ex c}
	| a=Annotation {EV_Annot a}
	| e= ElementValueArrayInitializer {EV_array e} 

ElementValueArrayInitializer:
	LCURL e=option(ElementValues) option(COMM) RCURL {match e with | Some e -> e | None [] }

ElementValues:
	| e=ElementValue {e::[]}
	| es=ElementValues COMM e=ElementValue {es@[e]}

%%