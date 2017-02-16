(* Code for doing the execution *)
open AST
open Exceptions
open MemoryModel

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
	()

let execute_statement stmt = 
	match stmt with
	| Expr(e) -> (* print_endline (AST.string_of_expression e) *)
				match e.edesc with
				| Call(obj, name, args) -> 
											match name with
											| "println" -> print_endline (String.concat "" (List.map string_of_expression args))
											| _ -> print_endline "Statement not executable yet, try a System.out.println().."
				| _ -> print_endline "Statement not executable yet, try a System.out.println().."
	| _ -> print_endline "Statement not executable yet, try a System.out.println().."

(* Make a structure that contains the whole program, its heap
stack .. *)
let execute_code (jprog : jvm) =
	let startpoint = get_main_method jprog 
	in
	AST.print_method "" startpoint;
	(* run the program *)
	print_endline "### Running ... ###";
	List.iter (execute_statement) startpoint.mbody
