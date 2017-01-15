exception JavaException of string

type primTypes =
	| PT_Float
	| PT_Boolean
	| PT_Byte
	| PT_Char
	| PT_Int
	| PT_Long
	| PT_Short
	| PT_Double

(* arithmetic ops *)
type binop =
	| BO_Add
	| BO_Minus
	| BO_Mul
	| BO_Div
	| BO_Mod

type unop =
	| UO_Plus
	| UO_Minus
	| UO_Increment
	| UO_Decrement

(* logical ops *)
type logbinop =
	| LBO_And (* && *)
	| LBO_Or

type loguop =
	| LUO_Not
	| UO_BNot

(* any type ops *)
type compop =
	| BO_Gt
	| BO_Lt
	| BO_Ge
	| BO_Le 
	| BO_Neq
	| BO_Eq
	| BO_instanceof

(* bitwise ops *)
type bitop =
	| SO_Lshift 
	| SO_Rshift 
	| SO_Logshift
	| SO_And (* & *)
	| SO_Or
	| SO_Xor

type assign =
	| ASS_Equal
	| ASS_Plus
	| ASS_Minus
	| ASS_Mul
	| ASS_Div
	| ASS_Mod
	| ASS_Xor
	| ASS_And
	| ASS_Or
	| ASS_RShift
	| ASS_LShift
	| ASS_LogShift

type literal =
	| L_Int of int
	| L_Str of string
	| L_Float of float
	| L_Double of float
	| L_Char of char
	| L_Boolean of bool
	| L_Long of int
	| L_Null

type typeParam =
	| TPL_Ident of string
	| TPL_Extend of string * string	

type declaratorId = 
	|DI_Identifier of string
	|DI_Args of string * int

type annotation = {
	aName:types;
	aElemPairs: elemValuePair list
}
	
and allTypes =
	| AL_Types of types
	| AL_Array of types*int

and definedType =
	| DT_Id of string
	| DT_Generic of string * typeParam list

and elemValue =
	| EV_Ex of expression
	| EV_Annot of annotation
	| EV_Array of elemValue list

and elemValuePair = {evpId:string;evpValue:elemValue}

and jexception = definedType list

and types=
	| T_Primitive of primTypes
	| T_Qualified of definedType list

and modifier=
	|M_Annot of annotation
	|M_Public
	|M_Protected
	|M_Private
	|M_Abstract
	|M_Static
	|M_Final
	|M_Synchronized
	|M_Native
	|M_Strictfp

and variableModifier =
	|VM_Final
	|VM_Volatile
	|VM_Transient
	|VM_Annot of annotation

and resultType=
	|RT_Type of allTypes	
	|RT_Void

and parentName = string*(typeParam list option)

and parentClass=
	| C_Parent of parentName
	| C_Object

and formalParameter = {
	pmodif: variableModifier list; 
	ptype: allTypes; 
	pname: declaratorId;
	pelipsis: bool;
}

and methodDeclarator= {
	mname: string; 
	mparams: formalParameter list
}


and enhanced_for =
	| Enhanced_for of variableModifier list option * allTypes * string

(* Clases and methods *)
and javaMethod={
	mutable jmmodifiers: modifier list;
	mutable jmtparam: typeParam list; 	
	mutable jmrtype: resultType;
	jmdeclarator: methodDeclarator;
	jmthrows: jexception list;
	jmbody: statement;
}

and javaClass={
	mutable cmodifiers: modifier list;
	cidentifier: string;
	mutable ctparam: typeParam list;
	cparent: parentClass;
	cinterfaces: string list;
	cbody: insideClass list;
}

and attribute={
	mutable attrmodifiers: modifier list;
	atype: allTypes;
	adeclarator: expression list;
}

and insideClass=
	| IC_Method of javaMethod
	| IC_Attribute of attribute
	| IC_Class of javaClass
	| IC_Semi
	| IC_Empty
	| IC_Interface of javaInterface
	| IC_Static of statement
	| IC_Constructor of javaMethod

and annotationTypeDeclaration = {
	mutable iaModifiers: modifier list;
	iaName:string;
	ibody:annotationTypeElementDeclaration list;
		
}

and annotationTypeElementDeclaration = 
	| ATED_Class of javaClass
	| ATED_Inter of javaInterface
	| ATED_None
	| ATED_Declar of attribute
	| ATED_Basic of annotationTED

and annotationTED = {
	mutable atedModifs: modifier list; 
	atedName:string; 
	atedType:allTypes; 
	default:elemValue option }
















and javaInterfaceStruct={
	mutable imodifiers: modifier list;
	iidentifier: string;
	itparam: typeParam list;
	iparent: parentName list;
	ibody: insideInterface list;
}

and javaInterface=
	| JI_IN of javaInterfaceStruct
	| JI_AN of annotationTypeDeclaration

and insideInterface=
	| II_Class of javaClass
	| II_Interface of javaInterface
	| II_Method of javaMethod
	| II_Field of statement

and constructor={
	mutable constrmodifiers: modifier list;
	constrtparam: typeParam list; 	
	constrdeclarator: methodDeclarator;
	constrthrows: jexception list;
	constrbody: statement;
}

(* expressions.ml *)
and expression =
	| Identifier of string
	| Literal of literal
	| EX_Empty
	| EX_Binop of binop * expression * expression
	| EX_Compop of compop * expression * expression
	| EX_Instanceof of compop * expression * types
	| EX_Bitop of bitop * expression * expression
	| EX_Logbinop of logbinop * expression * expression
	| EX_Loguop of loguop * expression
	| EX_Unop of unop * expression
	| EX_Postfix of unop * expression
	| EX_Assign of assign * expression * expression	
	| EX_Primitive of primTypes * int option
	| EX_Cast of expression * expression 
	| EX_Class of expression * int 
	| EX_Ternary of expression * expression * expression
	| EX_Case of expression
	| EX_Default
	| EX_Array_access of expression * expression
	| EX_Field_access of expression * expression option
	| EX_Method_access of expression * expression list
	| EX_Array_alloc of types * expression list option * int option
	| EX_Plain_array_alloc of expression * expression list
	| EX_Plain_class_alloc of expression * insideClass list
	| EX_Class_alloc of definedType list * expression list option
	| EX_New_alloc of expression option * expression
	| EX_Var_decl of expression * expression list option
	| EX_Primary of primaryType
	| EX_QualifiedName of definedType list

and primaryType =
	| P_Qualified of definedType list
	| P_NotJustName of expression

and catch_header =
	| Catch_header of types * expression

and statement = 
	| ST_Empty 
	| ST_Block of statement list
	| ST_Label of string
	| ST_Expression of expression
	| ST_If of expression * statement * statement option
	| ST_Switch of expression * statement
	| ST_Case of expression list * statement list
	| ST_While of expression * statement
	| ST_For of statement list * expression * statement list * statement
	| ST_Efor of enhanced_for * expression * statement 
	| ST_Do_while of statement list * expression
	| ST_Break of string
	| ST_Continue of string
	| ST_Return of expression
	| ST_Throw of expression
	| ST_Lvar_decl of expression
	| ST_Synch of expression * statement
	| ST_Try of statement * statement list * statement
	| ST_Catch of catch_header * statement
	| ST_Catches of statement list
	| ST_Finally of statement
	| ST_Assert of expression * expression option
	| ST_Var_decl of string option * allTypes * expression list
	| ST_Local_class of javaClass
	| ST_Local_interface of javaInterface

type import={
	impStatic: bool;
	impPack: string list;
	impAll: bool;
}

type fileContent=
	| F_Class of javaClass
	| F_Interface of javaInterface

type javaCompilationFile={
	fPackage: string list;
	fImports: import list;
	fContent: fileContent list;
}

type abstractSyntaxTree = 
	| JML of javaMethod list
	| STR of string
	| STATE of statement
	| EXPR of expression
	| JCLASS of javaClass
	| JFILE of javaCompilationFile