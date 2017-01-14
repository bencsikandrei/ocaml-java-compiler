%{
open Ast
%}

%start javaMethods
%start <abstractSyntaxTree> javaMethods

%%

javaMethods:
	| p=javaMethod_list {JML p}	

javaMethod_list:
	|j1=javaMethod EOF {j1::[]}
	|j1=javaMethod j2=javaMethod_list {j1::j2}

(* Method Layout declarations *)
%public javaMethod:  
		|mm=modifiers 	tp=type_params_defin 	rt=ResultType md=MethodDeclarator th=Throws mb=MethodBody { {jmmodifiers=mm;jmtparam=tp;jmrtype=rt;jmdeclarator=md;jmthrows=th;jmbody=mb} }
		|				tp=type_params_defin 	rt=ResultType md=MethodDeclarator th=Throws mb=MethodBody { {jmmodifiers=[];jmtparam=tp;jmrtype=rt;jmdeclarator=md;jmthrows=th;jmbody=mb} }
		|mm=modifiers 							rt=ResultType md=MethodDeclarator th=Throws mb=MethodBody { {jmmodifiers=mm;jmtparam=[];jmrtype=rt;jmdeclarator=md;jmthrows=th;jmbody=mb} }
		|										rt=ResultType md=MethodDeclarator th=Throws mb=MethodBody { {jmmodifiers=[];jmtparam=[];jmrtype=rt;jmdeclarator=md;jmthrows=th;jmbody=mb} }
		|mm=modifiers	tp=type_params_defin 	rt=ResultType md=MethodDeclarator 			mb=MethodBody { {jmmodifiers=mm;jmtparam=tp;jmrtype=rt;jmdeclarator=md;jmthrows=[];jmbody=mb} }
		|				tp=type_params_defin 	rt=ResultType md=MethodDeclarator 			mb=MethodBody { {jmmodifiers=[];jmtparam=tp;jmrtype=rt;jmdeclarator=md;jmthrows=[];jmbody=mb} }
		|mm=modifiers 						 	rt=ResultType md=MethodDeclarator 			mb=MethodBody { {jmmodifiers=mm;jmtparam=[];jmrtype=rt;jmdeclarator=md;jmthrows=[];jmbody=mb} }
		|										rt=ResultType md=MethodDeclarator 			mb=MethodBody { {jmmodifiers=[];jmtparam=[];jmrtype=rt;jmdeclarator=md;jmthrows=[];jmbody=mb} }

%public javaMethod_plain_return:  
		|rt=ResultType md=MethodDeclarator th=Throws mb=MethodBody { {jmmodifiers=[];jmtparam=[];jmrtype=rt;jmdeclarator=md;jmthrows=th;jmbody=mb} }
		|rt=ResultType md=MethodDeclarator 			mb=MethodBody { {jmmodifiers=[];jmtparam=[];jmrtype=rt;jmdeclarator=md;jmthrows=[];jmbody=mb} }

%public javaMethod_plain:  
		|md=MethodDeclarator th=Throws  mb=MethodBody { {jmmodifiers=[];jmtparam=[];jmrtype=RT_Void;jmdeclarator=md;jmthrows=th;jmbody=mb} }
		|md=MethodDeclarator 			mb=MethodBody { {jmmodifiers=[];jmtparam=[];jmrtype=RT_Void;jmdeclarator=md;jmthrows=[];jmbody=mb} }

	ResultType:
		|e=allTypes {RT_Type e}
		|VOID {RT_Void}

	MethodDeclarator: (*compile error if two methods with the same id and param list *)
		|i=IDENTIFIER LPAR RPAR {{mname=i;mparams=[]}}
		|i=IDENTIFIER LPAR p=FormalParameters RPAR {{mname=i;mparams=p}}

(* Parameter List declarations *)

	FormalParameters:(* two with same name compile error *)
		|p=LastFormalParameter {p::[]}
		|p1=FormalParameter COMM p2=FormalParameters {p1::p2}

	FormalParameter:
		|m=option(VariableModifiers) t=allTypes v=VariableDeclaratorId 
			{let m=match m with |None -> [] | Some m ->m in {pmodif=m;ptype=t;pname=v;pelipsis=false}}

	LastFormalParameter:
		|m=option(VariableModifiers) t=allTypes ELIPSIS v=VariableDeclaratorId
			{let m=match m with |None -> [] | Some m ->m in {pmodif=m;ptype=t;pname=v;pelipsis=true}}
		| ep=FormalParameter {ep}

	VariableModifiers:
		|m1=VariableModifier {m1::[]}
		|m1=VariableModifier m2=VariableModifiers {m1::m2}

	VariableModifier:
		|FINAL {VM_Final}
		|a=Annotation {VM_Annot a} (* If an annotation a on a formal parameter corresponds to an annotation type T, and T has a (meta-)annotation m that corresponds to annotation.Target , then m must have an element whose value is annotation.ElementType.PARAMETER , or a compile-time error occurs *)

(* throws *)

	Throws:
		THROWS ExceptionTypeList {[]}

	ExceptionTypeList:
		|ExceptionType {}
		|ExceptionTypeList COMM ExceptionType {}

	ExceptionType: (* TODO check section 4.3 *)
(* 
		|ClassType {}
		|TypeVariable {}
*)		
		|allTypes {}

MethodBody:
	|LCURL b=block RCURL { b }
	|LCURL RCURL { ST_Empty }
	|SEMI { ST_Empty }


(* aux TODO *)


VariableDeclaratorId:
	|i=IDENTIFIER {DI_Identifier i}

%%
