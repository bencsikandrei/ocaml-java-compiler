(* Code for doing the execution *)
open AST
open Exceptions
open MemoryModel

(* default initializers for values *)
type default = {
	values: (string, valuetype) Hashtbl.t;
}

(* what can we use *)
and valuetype = 
	| IntVal of int
	| StrVal of string
	| FltVal of float
	| BoolVal of bool
	| RefVal of newobject
	| NullVal

(* heap declared objects *)
and newobject = {
	(* the object declaration name *)
	oname: string;
	(* the class it instantiates *)
	oclass: javaclass;
	(* its attributes *)
	oattributes: (string, valuetype) Hashtbl.t;
}

type scope = {
	(* current *)
	currentclass: javaclass;
	visible: (string, valuetype) Hashtbl.t;
}

(* string of valuetypes *)
let string_of_value v =
	match v with 
	| IntVal(i) -> string_of_int i 
	| StrVal(s) -> s
 	| FltVal(f) -> string_of_float f
 	| BoolVal(b) -> string_of_bool b
	(* | RefVal of newobject *)

(* find the start point *)
let get_main_method (jprog : jvm) =
	(* search if there is a main method, 
	if yes -> return it
	else raise an exception *)
	let main_method_name = (jprog.public_class ^ "_main_String[]") 
	in
	try 
		Hashtbl.find jprog.methods main_method_name
	with
	| _ -> raise (NoMainMethod ("Error: Main method not found in class " ^ jprog.public_class ^ ", please define the main method as: public static void main(String[] args) or a JavaFX application class must extend javafx.application.Application "))

let execute_expression expr =
	match expr.edesc with 
	| Val(v) -> match v with
			  | String(s) -> StrVal(s)
			  | Int(i) -> IntVal(int_of_string i)
			  | Float(f) -> FltVal(float_of_string f)
			  | Boolean(b) -> BoolVal(b) 
			  | Null -> NullVal
			  (*| Char of char option
				*)
	(* | New of string option * string list * expression list
	| AssignExp(e1, op, e2) -> 
	| If(e1, e2, e3) of expression * expression * expression
	| NewArray of Type.t * (expression option) list * expression option
	| Call of expression option * string * expression list
	| Attr of expression * string
	| Name of string
	| ArrayInit of expression list
	| Array of expression * (expression option) list
	| Post of expression * postfix_op
	| Pre of prefix_op * expression
	| Op of expression * infix_op * expression
	| CondOp of expression * expression * expression
	| Cast of Type.t * expression
	| Type of Type.t
	| ClassOf of Type.t
	| Instanceof of expression * Type.t
	| VoidClass
 *)

(* execute a variable declaration *)
let execute_vardecl currentscope decl = 
	match decl with
	(* type, name, optional initialization *)
	| (t, n, eo) -> print_endline (Type.stringOf t)

let execute_statement currentscope stmt = 
	match stmt with
	| Expr(e) -> (* print_endline (AST.string_of_expression e) *)
				begin
				match e.edesc with
				| Call(obj, name, args) -> begin
									match name with
									| "println" -> print_endline (string_of_value (execute_expression (List.hd args))) 
									| _ -> print_endline "Statement not executable yet, try a System.out.println().."
									end
				| _ -> print_endline "Statement not executable yet, try a System.out.println().."
				end
	| VarDecl(vardecls) -> List.iter (execute_vardecl currentscope) vardecls

	| _ -> print_endline "Statement not executable yet, try a System.out.println().."

(* Make a structure that contains the whole program, its heap
stack .. *)
let execute_code (jprog : jvm) =
	let startpoint = get_main_method jprog 
	in
	(* since we know that by now we have a public class *)
	let currentscope = { 
							currentclass = (Hashtbl.find jprog.classes jprog.public_class);
							visible = (Hashtbl.create 10) 
						}
	in
	AST.print_method "" startpoint;
	(* run the program *)
	print_endline "### Running ... ###";
	List.iter (execute_statement currentscope) startpoint.mbody
