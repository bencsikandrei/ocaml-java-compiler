open String
open Ast
let indent var =
	let size = length var in 
		let rec iterate pos=
			if pos=size then 
				""
			else
				if (String.get var pos)='\n' then 
					"\n\t"^(iterate (pos+1)) 
				else 
					(String.make 1 (String.get var pos))^(iterate (pos+1))
		in
			"\t"^iterate 0;;

let rec print_list print_f  list separator = match list with
	| [] ->  ""
	| head::tail -> (print_f head)^separator^(print_list print_f tail separator);;


let string_of_primitive var = match var with
	| PT_Float -> "float"
	| PT_Boolean ->  "bool"
	| PT_Byte ->  "byte"
	| PT_Char ->  "char"
	| PT_Int ->  "int"
	| PT_Long ->  "long"
	| PT_Short ->  "short"
	| PT_Double ->  "double"

let rec string_of_definedType var = match var with
	| DT_Id id -> id
	| DT_Generic(t,al) -> t^"<"^(print_list string_of_definedType al " ")^">"
let rec string_of_allTypes var = match var with
	| AL_Types a -> string_of_types a
	| AL_Array (a,dim) -> "["^(string_of_types a)^"]dims="^(string_of_int dim)
and string_of_types var = match var with
	| T_Primitive v -> string_of_primitive v
	| T_Qualified v -> print_list string_of_definedType v "-"

(* get string of operators *)
let string_of_bo = function
	| BO_Add -> "+"
	| BO_Minus -> "-"
	| BO_Mul -> "*"
	| BO_Div -> "/"
	| BO_Mod -> "%"

let string_of_uo = function
	| UO_Plus -> "+"
	| UO_Minus -> "-"
	| UO_Increment -> "++"
	| UO_Decrement -> "--"

let string_of_lbo = function
	| LBO_Or -> "||"
	| LBO_And -> "&&"

let string_of_luo = function
	| LUO_Not -> "!"
	| UO_BNot -> "~"

let string_of_compop = function
	| BO_Gt -> ">"
	| BO_Lt -> "<"
	| BO_Ge -> ">="
	| BO_Le  -> "<="
	| BO_Neq -> "!="
	| BO_Eq -> "==" 
	| BO_instanceof -> "instanceof"

let string_of_bitop = function
	| SO_Lshift -> "<<"
	| SO_Rshift -> ">>"
	| SO_Logshift-> ">>>"
	| SO_And -> "&"
	| SO_Or -> "|"
	| SO_Xor -> "^"

let string_of_assign = function
	| ASS_Equal -> "="
	| ASS_Plus -> "+="
	| ASS_Minus -> "-="
	| ASS_Mul -> "*="
	| ASS_Div -> "/="
	| ASS_Mod -> "%="
	| ASS_Xor -> "^="
	| ASS_And -> "&="
	| ASS_Or -> "|="
	| ASS_RShift -> ">>="
	| ASS_LShift -> "<<="
	| ASS_LogShift -> ">>>="
(* end of operators *)

(* extract from option *)
let str_of_option e =
	match e with
	| Some(ex) -> ex
	| _ -> ""

let list_of_option l =
  match l with
  | Some(x) -> x
  | _ -> []

let exp_of_option e =
	match e with
	| Some(ex) -> ex
	| _ -> Identifier("")

let stms_of_option s =
	match s with
	| Some(s) -> s
	| _ -> ST_Empty 

let int_of_option v =
	match v with
	| Some(v) -> v
	| _ -> 0
(* end of option *)
let rec string_mul i s =
	match i with
	| 0 -> s
	| _ -> (string_mul (i-1) (s^s))

let print_annot a=a.aname;;
let print_excep a=a.ename;;

let print_modif modifier= match modifier with
	|M_Annot a -> print_annot a
	|M_Public -> "public"
	|M_Protected -> "protected"
	|M_Private -> "private"
	|M_Abstract -> "abstract"
	|M_Static -> "static"
	|M_Final -> "final"
	|M_Synchronized -> "synchronized"
	|M_Native -> "native"
	|M_Strictfp -> "strictfp";; 

let print_vm var = match var with
	|VM_Final -> "final"
	|VM_Annot a -> print_annot a
	|VM_Transient -> "transient"
	|VM_Volatile -> "volatile"

let print_type_param var = match var with
	| TPL_Ident s -> s
	| TPL_Extend (s1,s2) -> s1^"-ext-"^s2	;;

let print_return_type var = match var with
	|RT_Type t-> string_of_allTypes t	
	|RT_Void -> "void"

let print_declaratorId var = match var with 
	|DI_Identifier s -> s
	|DI_Args(i,d)-> i^(string_mul d "[]")

let print_formal_parameter var = 
	let el = match var.pelipsis with | true -> "..." | false -> "" in
	(print_list print_vm var.pmodif " ")^" : "^string_of_allTypes var.ptype^el^" : "^(print_declaratorId var.pname)
;;

let print_method_declarator var = var.mname^"\n"^(indent (print_list print_formal_parameter var.mparams "\n"))

(* end of my add *)
  
let string_of_enhanced_for ef =
	match ef with
	| Enhanced_for(ml,t,s) -> (print_list print_vm (list_of_option ml) " ")^(string_of_allTypes t)^" "^s

let string_of_literal x =
 	match x with
	| L_Str(v)-> v
	| L_Float(v) ->  string_of_float v
	| L_Double(v) -> string_of_float v
	| L_Char(v) -> (String.make 1 v)
	| L_Boolean(v) -> string_of_bool v
 	| L_Int(v) -> string_of_int v
 	| L_Null -> "null"
 	| L_Long(v) -> string_of_int v

let rec string_of_exp exp =
	match exp with
	| Identifier id -> id
	| Literal lit -> string_of_literal lit
	| EX_Empty -> ""
	| EX_Binop(op, e1, e2) -> (string_of_exp e1)^(string_of_bo op)^(string_of_exp e2)
	| EX_Compop(op, e1, e2) -> (string_of_exp e1)^(string_of_compop op)^(string_of_exp e2)
	| EX_Instanceof(op, e1, t) -> (string_of_exp e1)^(string_of_compop op)^ (string_of_types t) (* allows instanceof generics*)
	| EX_Bitop(op, e1, e2)-> (string_of_exp e1)^(string_of_bitop op)^(string_of_exp e2)
	| EX_Logbinop(op, e1, e2) -> (string_of_exp e1)^(string_of_lbo op)^(string_of_exp e2)
	| EX_Loguop(op, e) -> (string_of_luo op)^(string_of_exp e)
	| EX_Unop(op, e) -> (string_of_uo op)^(string_of_exp e)
	| EX_Postfix(op, e) -> (string_of_exp e)^(string_of_uo op)
	| EX_Assign(op, e1, e2) -> (string_of_exp e1)^(string_of_assign op)^(string_of_exp e2)
	| EX_Primitive(p, so) -> (string_of_primitive p)^(string_of_int (int_of_option so))
	| EX_Cast(e1, e2) -> " ("^(string_of_exp e1)^") "^(string_of_exp e2)
	| EX_Class(e,i) -> (string_of_exp e)^(string_mul i "[]")
	| EX_Ternary(e1,e2,e3) -> (string_of_exp e1)^" ? "^(string_of_exp e2)^" : "^(string_of_exp e3)
	| EX_Case(e) -> "case "^(string_of_exp e)^":"
	| EX_Default -> "default:"
	| EX_Array_access(e1,e2) -> (string_of_exp e1)^"["^(string_of_exp e2)^"]"
	| EX_Field_access(e, eo) -> (string_of_exp e)^"."^(string_of_exp (exp_of_option eo))
	| EX_Method_access(e,el) -> (string_of_exp e)^"("^(String.concat "," (List.map string_of_exp el))^")"
	| EX_Array_alloc(t,elo, io) -> "new "^(string_of_types t)^(String.concat "" (List.map string_of_exp (list_of_option elo) ))^(string_of_int (int_of_option io))
	| EX_Plain_array_alloc(e,el) -> (string_of_exp e)^"{"^(String.concat "," (List.map string_of_exp el))^"}"
	| EX_Plain_class_alloc(e,el) -> "inside class print"
	| EX_Class_alloc(dtl,elo) -> "new "^(print_list string_of_definedType dtl ".")^"("^(String.concat "," (List.map string_of_exp (list_of_option elo) ))^")"
	| EX_New_alloc(so, e) -> (string_of_exp (exp_of_option so))^"."^(string_of_exp e) (* dot optional *)
	| EX_Var_decl(e, elo) -> (string_of_exp e)^" = "^(String.concat "," (List.map string_of_exp (list_of_option elo))) (* assign optional *)
	| EX_Primary(pt) -> (string_of_primaryType pt)
	| EX_QualifiedName(ldt) -> (print_list string_of_definedType ldt ".")

and string_of_primaryType var =
	match var with
	| P_Qualified(dl) -> (print_list string_of_definedType dl ".")
	| P_NotJustName(e) -> (string_of_exp e)

let string_of_catch_header ch =
	match ch with 
	| Catch_header(t,s) -> (string_of_types t)^" "^(string_of_exp s)

let rec string_of_stmt =
	function
	| ST_Empty -> "/* ST_empty */"
	| ST_Label x -> "\n"^x
	| ST_Block(stl) -> (String.concat "\n" (List.map string_of_stmt stl))
	| ST_Expression e -> "\n"^string_of_exp e
	| ST_If(e, st1, st2) -> "\nif ("^(string_of_exp e)^") {"^(string_of_stmt st1)^"}"^(string_of_stmt (stms_of_option st2))
	| ST_Switch(e, sb) -> "\nswitch ("^(string_of_exp e)^") "^(string_of_stmt sb)
	| ST_While(e, st) ->  "\nwhile ("^(string_of_exp e)^") {"^(string_of_stmt st)^"}"
	| ST_Case(el, st) -> (String.concat ", " (List.map string_of_exp el))^(print_list string_of_stmt st ";") 
	| ST_For(e1, e2, e3, st) -> "\nfor ("^(String.concat "; " (List.map string_of_stmt e1))^" "^(string_of_exp e2)^"; "^(String.concat "; " (List.map string_of_stmt e3))^")"^(string_of_stmt st)
	| ST_Efor(ef,e,s) -> "\nfor("^(string_of_enhanced_for ef)^" : "^(string_of_exp e)^") "^(string_of_stmt s)
	| ST_Do_while(st, e) -> "\ndo {"^(String.concat "; " (List.map string_of_stmt st))^"} while ("^(string_of_exp e)^");"
	| ST_Break(e) -> "\nbreak "^e
	| ST_Continue(e) -> "\ncontinue "^e
	| ST_Return(e) -> "\nreturn "^(string_of_exp e)
	| ST_Throw(e) -> "\nthrow "^(string_of_exp e)
	| ST_Lvar_decl(e) -> "\n"^(string_of_exp e)
	| ST_Synch(e1,e2) -> "\nsynchronized "^(string_of_exp e1)^" : "^(string_of_stmt e2)
	| ST_Try(st1,stl,st2) ->  "\ntry {"^(string_of_stmt st1)^(String.concat "; " (List.map string_of_stmt stl))^(string_of_stmt st2)^"}"
	| ST_Catch(ch, st) ->  "\ncatch ("^(string_of_catch_header ch)^")"^(string_of_stmt st)
	| ST_Catches(stl) -> "\n"^(String.concat "; " (List.map string_of_stmt stl))
	| ST_Finally(st) -> "\nfinally "^(string_of_stmt st)
	| ST_Assert(e1,e2) -> "\nassert ("^(string_of_exp e1)^") : ("^(string_of_exp(exp_of_option e2))^");"
	| ST_Var_decl(so,t, e) -> "\n"^(str_of_option so)^" "^(string_of_allTypes t)^" "^(String.concat ", " (List.map string_of_exp e))^";" 

(*  *)
let print_java_method var = 
	"\nMethod: "^(print_method_declarator var.jmdeclarator)^
	"\nReturn type: "^(print_return_type var.jmrtype)^
	"\nModifiers: "^(print_list print_modif var.jmmodifiers " ")^
	"\nGenerics: "^(print_list print_type_param var.jmtparam " ")^
	"\nThrows: "^(print_list print_excep var.jmthrows " ")^
	"\nBody: "^indent ("\n"^(string_of_stmt var.jmbody));;

let print_parent var = match var with
	| C_Parent(p,g) -> ( p^"<"^(print_list print_type_param (list_of_option g) ",")^">")
	| C_Object -> ""

let rec print_inside_class var = match var with
	| IC_Method(jm) -> (print_java_method jm)
	| IC_Attribute(mlo,alt,el) -> (print_list print_modif (list_of_option mlo) " ")^" "^(string_of_allTypes alt)^" "^(print_list string_of_exp el " ")
	| IC_Class(jc) -> (print_java_class jc)
	| IC_Semi -> ";"
	| IC_Empty -> ""
	| IC_Interface(inter) -> (print_interface inter)
	| IC_Static(b) -> (string_of_stmt b)
	
and print_java_class var =
	"\nModifiers: "^(print_list print_modif var.cmodifiers " ")^
	"\nIdentifier: "^var.cidentifier^
	"\nType Parameters: "^(print_list print_type_param var.ctparam " ")^
	"\nParent: "^(print_parent var.cparent)^
	"\nInterfaces: "^(String.concat ", " var.cinterfaces)^
	"\nBody: "^(print_list print_inside_class var.cbody " ")^" -----------------\n"

and print_fcontent var = match var with
	| F_Class(c) -> print_java_class c
	| F_Interface(i) -> print_interface i

and print_inside_interface var = match var with
	| II_Class(c) -> print_java_class c
	| II_Interface(i) -> print_interface i
	| II_Method(m) -> print_java_method m

and print_parent_name (a,b) = a^"<"^(print_list print_type_param (list_of_option b) ",")^">"

and print_interface var =
	"\nModifiers: "^(print_list print_modif var.imodifiers " ")^
	"\nIdentifier: "^var.iidentifier^
	"\nType Parameters: "^(print_list print_type_param var.itparam " ")^
	"\nParents: "^(print_list print_parent_name var.iparent ", ")^
	"\nBody: "^(print_list print_inside_interface var.ibody " ")

let print_import var =
	"\nStatic: "^(string_of_bool var.impStatic)^
	"\nImport: "^(String.concat ", " var.impPack)^
	"\nAll: "^(string_of_bool var.impAll)

let prit_java_file var =
	"\nPackage: "^(String.concat ", " var.fPackage)^
	"\nImports: "^(print_list print_import var.fImports " ")^
	"\nFile Content: "^(print_list print_fcontent var.fContent " ")^"\n\n"