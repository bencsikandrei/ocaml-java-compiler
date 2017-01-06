%{
open Definitions
%}

%start javaMethods
%start <javaMethod list> javaMethods

%%

javaMethods:
	|j1=javaMethod EOF {j1::[]}
	|j1=javaMethod j2=javaMethods {j1::j2}

(* Method Layout declarations *)
%public javaMethod:  
		|mm=option(MethodModifiers) tp=option(type_params_defin) rt=ResultType md=MethodDeclarator th=option(Throws) mb=MethodBody {
			let mm=match mm with | None -> [] | Some mm ->mm in
			let tp=match tp with | None -> [] | Some tp -> tp in
			let th=match th with | None	-> [] | Some th -> th in 
			{jmmodifiers=mm;jmtparam=tp;jmrtype=rt;jmdeclarator=md;jmthrows=th;jmbody=mb} 
		}

	ResultType:
		|e=types {RT_Type e}
		|VOID {RT_Void}

	MethodDeclarator: (*compile error if two methods with the same id and param list *)
		|i=IDENTIFIER LPAR RPAR {{mname=i;mparams=[]}}
		|i=IDENTIFIER LPAR p=FormalParameters RPAR {{mname=i;mparams=p}}

(* Parameter List declarations *)

	FormalParameters:(* two with same name compile error *)
		|p=LastFormalParameter {p::[]}
		|p1=FormalParameter COMM p2=FormalParameters {p1::p2}

	FormalParameter:
		|m=option(VariableModifiers) t=types v=VariableDeclaratorId 
			{let m=match m with |None -> [] | Some m ->m in {pmodif=m;ptype=t;pname=v;pelipsis=false}}

	LastFormalParameter:
		|m=option(VariableModifiers) t=types ELIPSIS v=VariableDeclaratorId
			{let m=match m with |None -> [] | Some m ->m in {pmodif=m;ptype=t;pname=v;pelipsis=true}}
		| ep=FormalParameter {ep}

	VariableModifiers:
		|m1=VariableModifier {m1::[]}
		|m1=VariableModifier m2=VariableModifiers {m1::m2}

	VariableModifier:
		|FINAL {VM_Final}
		|a=Annotation {VM_Annot a} (* If an annotation a on a formal parameter corresponds to an annotation type T, and T has a (meta-)annotation m that corresponds to annotation.Target , then m must have an element whose value is annotation.ElementType.PARAMETER , or a compile-time error occurs *)

(* Method Modifiers *)
	MethodModifiers: (* error if more than onae pub, priv, prot *) (* abstract -> ¬( private , static , final , native , strictfp , or synchronized *) (* native -> ¬ strictfp *)
		|m1=MethodModifier {m1::[]}
		|m1=MethodModifier m2=MethodModifiers{m1::m2}

	MethodModifier: 
		|a=Annotation {MM_Annot a}
		|PUBLIC {MM_Public}
		|PROTECTED {MM_Protected}
		|PRIVATE {MM_Private}
		|ABSTRACT {MM_Abstract}
		|STATIC {MM_Static}
		|FINAL {MM_Final}
		|SYNCHRONIZED {MM_Synchronized}
		|NATIVE {MM_Native}
		|STRICTFP {MM_Strictfp}

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
		|types {}

(* Method Body TODO *)
MethodBody:
	|Block {{expr=""}}
	|SEMI {{expr=""}}


(* aux TODO *)
Annotation:
	|ANOT i=IDENTIFIER{  { aname=i ; aoth="" } }

VariableDeclaratorId:
	|i=IDENTIFIER {DI_Identifier i}

%public Block:
	| LCURL RCURL {}
	| LCURL exprs RCURL {}

%%
