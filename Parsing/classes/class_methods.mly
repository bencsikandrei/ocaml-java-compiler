%{
	
%}

%start javaMethods
%start <string> javaMethods

%%

javaMethods:
	|javaMethod EOF {""}
	|javaMethod javaMethods {""}

(* Method Layout declarations *)
	%public javaMethod:
		|MethodHeader MethodBody {} (* It is a compile-time error if a method or constructor parameter that is declared final is assigned to within the body of the method or constructor. *)

	MethodHeader:  
		|option(MethodModifiers) option(TypeParameters) ResultType MethodDeclarator option(Throws) {}

	ResultType:
		|types {}
		|VOID {}

	MethodDeclarator: (*compile error if two methods with the same id and param list *)
		|IDENTIFIER LPAR RPAR {}
		|IDENTIFIER LPAR FormalParameters RPAR {}

(* Parameter List declarations *)

	FormalParameters:(* two with same name compile error *)
		|LastFormalParameter {}
		|FormalParameter COMM FormalParameters {}

	FormalParameter:
		|option(VariableModifiers) types VariableDeclaratorId {}

	LastFormalParameter:
		|option(VariableModifiers) types ELIPSIS VariableDeclaratorId {}
		|FormalParameter {}

	VariableModifiers:
		|VariableModifier {}
		|VariableModifier VariableModifiers {}

	VariableModifier:
		|FINAL {}
		|Annotation {} (* If an annotation a on a formal parameter corresponds to an annotation type T, and T has a (meta-)annotation m that corresponds to annotation.Target , then m must have an element whose value is annotation.ElementType.PARAMETER , or a compile-time error occurs *)

(* Method Modifiers *)
	MethodModifiers: (* error if more than onae pub, priv, prot *) (* abstract -> ¬( private , static , final , native , strictfp , or synchronized *) (* native -> ¬ strictfp *)
		|MethodModifier {}
		|MethodModifier MethodModifiers{}

	MethodModifier: 
		|Annotation {}
		|PUBLIC {}
		|PROTECTED {}
		|PRIVATE {}
		|ABSTRACT {}
		|STATIC {}
		|FINAL {}
		|SYNCHRONIZED {}
		|NATIVE {}
		|STRICTFP {}

(* throws *)

	Throws:
		THROWS ExceptionTypeList {}

	ExceptionTypeList:
		|ExceptionType {}
		|ExceptionTypeList COMM ExceptionType {}

	ExceptionType: (* TODO check section 4.3 *)
(* 
		|ClassType {}
		|TypeVariable {}
*)		
		|types {}


(* Method Body *)
MethodBody:
	|Block {}
	|SEMI {}


(* aux *)
Annotation:
	|ANOT IDENTIFIER{}

VariableDeclaratorId:
	|IDENTIFIER {}

TypeParameters:
	| LANG TypeParameterList RANG {}

TypeParameterList:
	|TypeParameter	{}
	|TypeParameter COMM TypeParameterList {}
	

TypeParameter: (* TODO *)
	|IDENTIFIER {}

%public Block:
	| LCURL RCURL {}
	| LCURL exprs RCURL {}

%%
