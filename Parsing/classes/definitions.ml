type annotation = { aname:string ; aoth:string };; (* TODO *)
type jexception = { ename:string ; eoth:string };; (* TODO *)

type modifier=
	|M_Annot of annotation
	|M_Public
	|M_Protected
	|M_Private
	|M_Abstract
	|M_Static
	|M_Final
	|M_Synchronized
	|M_Native
	|M_Strictfp;;

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

