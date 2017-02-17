(* Code for doing the execution *)
open Type
open AST
open Exceptions
open MemoryModel

(* find the start point *)
let get_main_method (jprog : jvm) =
	(* search if there is a main method, 
	if yes -> return it
	else raise an exception *)
	let main_method_name = (jprog.public_class ^ "_main_String[]") (* classic java main method *)
	in
	try 
		(* get the method from the jvm table *)
		Hashtbl.find jprog.methods main_method_name
	with
	| _ -> raise (NoMainMethod ("Error: Main method not found in class " ^ jprog.public_class ^ 
								", please define the main method as: public static void main(String[] args)" ^
								"or a JavaFX application class must extend javafx.application.Application "))

(* do var++ and var--*)
let rec execute_postfix (jprog : jvm) (e : expression) postop =
	(* see what type *)
	match postop with
	| Incr -> 	begin
				match (execute_expression jprog e) with
				| IntVal(v) -> IntVal(v+1)
				| _ -> raise ArithmeticException
				end
	| Decr -> 	begin
				match (execute_expression jprog e) with
				| IntVal(v) -> IntVal(v-1)
				| _ -> raise ArithmeticException
				end

(* do ++var and --var *)
and execute_prefix (jprog : jvm) preop (e : expression) =
	(* see what type *)
	match preop with
	| Op_incr -> begin 
				match (execute_expression jprog e) with
				| IntVal(v) -> IntVal(v+1)
				| _ -> raise ArithmeticException
				end
	| Op_decr -> begin 
				match (execute_expression jprog e) with
				| IntVal(v) -> IntVal(v-1)
				| _ -> raise ArithmeticException
				end

(* execute an expression and send back it's value *)
and execute_expression (jprog : jvm) expr =
	(* check the descriptor *)
	match expr.edesc with 
	| Val(v) -> begin
				match v with
				| String(s) -> StrVal(s)
				| Int(i) -> IntVal(int_of_string i)
				| Float(f) -> FltVal(float_of_string f)
				| Boolean(b) -> BoolVal(b) 
				| Null -> NullVal
				end
			  (*| Char of char option
				*)
	| Post(e, poi) -> execute_postfix jprog e poi
	| Pre(pri, e) -> execute_prefix jprog pri e
	(* | New of string option * string list * expression list
	| AssignExp(e1, op, e2) -> 
	| If(e1, e2, e3) of expression * expression * expression
	| NewArray of Type.t * (expression option) list * expression option
	| Call of expression option * string * expression list
	| Attr of expression * string
	| Name of string
	| ArrayInit of expression list
	| Array of expression * (expression option) list
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
let execute_vardecl (jprog : jvm) decl = 
	match decl with
	(* type, name, optional initialization *)
	| (Primitive(p), n, eo) -> 
			begin
			let (_, scope) = Stack.top jprog.jvmstack 
			in
			(* matched an  *)
			Hashtbl.add scope.visible n (match eo with 
										| None -> Hashtbl.find jprog.defaults p;
										(* we need type checks here*)
										| Some(e) -> execute_expression jprog e)
			end
	(*
	| Array(typ,size) -> (stringOf typ)^(array_param size)
	| Ref rt -> stringOf_ref rt 
	*)

let execute_statement jprog stmt = 
	match stmt with
	(* treat all the expressions *)
	| Expr(e) -> (* print_endline (AST.string_of_expression e) *)
			begin
			match e.edesc with
			| Call(obj, name, args) -> begin
					match name with
					| "println" -> print_endline (string_of_value (execute_expression jprog (List.hd args))) 
					| _ -> print_endline "Statement not executable yet, try a System.out.println().."
					end
			| _ -> print_endline "Statement not executable yet, try a System.out.println().."
			end
	(* a variable declaration *)
	| VarDecl(vardecls) -> List.iter (execute_vardecl jprog) vardecls

	| _ -> print_endline "Statement not executable yet, try a System.out.println().."


(* add default initializer variables *)
let add_defaults (jprog : jvm) =
	Hashtbl.add jprog.defaults Int (IntVal 0);
	(* Hashtbl.add jprog.defaults Int StrVal(""); 
	*)
	Hashtbl.add jprog.defaults Float (FltVal 0.0);
	Hashtbl.add jprog.defaults Boolean (BoolVal false)

(* Make a structure that contains the whole program, its heap
stack .. *)
let execute_code (jprog : jvm) =
	(* setup the JVM *)
	add_defaults jprog;

	let startpoint = get_main_method jprog 
	in
	(* since we know that by now we have a public class *)
	let currentscope = { 
						visible = (Hashtbl.create 10) 
					   }
	in
	(* add the main mathods scope to the stack *)
	Stack.push (startpoint.mname, currentscope) jprog.jvmstack;
	(* the main method *)
	AST.print_method "" startpoint;
	(* run the program *)
	print_endline "### Running ... ###";
	(* print_scope jprog; *)
	List.iter (execute_statement jprog) startpoint.mbody;
	print_scope jprog
