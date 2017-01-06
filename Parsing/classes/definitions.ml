type annotation = { aname:string ; aoth:string };; (* TODO *)
type jexception = { ename:string ; eoth:string };; (* TODO *)

type modifier=
	|MM_Annot of annotation
	|MM_Public
	|MM_Protected
	|MM_Private
	|MM_Abstract
	|MM_Static
	|MM_Final
	|MM_Synchronized
	|MM_Native
	|MM_Strictfp;;

type variableModifier =
	|VM_Final
	|VM_Annot of annotation;;

type types =
	| T_Identifier of string
	| T_Float
	| T_Boolean
	| T_Byte
	| T_Char
	| T_Int
	| T_Long
	| T_Short
	| T_Double
	| T_Generic of string*types

type typeParamList =
	| TPL_Ident of string
	| TPL_Extend of string * string	


type resultType=
	|RT_Type of types	
	|RT_Void

type declaratorId = 
	|DI_Identifier of string

type body={expr:string};;

type formalParameter = {
	pmodif:variableModifier list; 
	ptype:types; 
	pname:declaratorId;
	pelipsis:bool;
};;

type methodDeclarator= {
	mname:string; 
	mparams:formalParameter list
}

type javaMethod={
	jmmodifiers: modifier list;
	jmtparam:typeParamList list; 	
	jmrtype:resultType;
	jmdeclarator:methodDeclarator;
	jmthrows:jexception list;
	jmbody:body;
}

