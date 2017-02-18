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
(* 
let rec get_var_names (jprog : jvm) vardecls =
	match vardecls with
	| [] -> [] 
	| hd::tl -> match hd with
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
 *)
(* add vars to current scope *)
let rec add_vars_to_scope (jprog : jvm) decls =
	match decls with
	| [] -> ()
	| hd::tl -> let (_, scope) =  Stack.top jprog.jvmstack 
				in ()
				(* Hashtbl.add scope.visible  *)

let compute_value op val1 val2 =
	match val1,val2 with
	| IntVal(v1),IntVal(v2) -> begin
				match op with
				| Op_add -> IntVal(v1 + v2)
				| Op_sub -> IntVal(v1 - v2)
				| Op_mul -> IntVal(v1 * v2)
				| Op_div -> IntVal(v1 / v2) (* exception when v2 is 0 *)
				| Op_mod -> IntVal(v1 mod v2)
				| Op_or -> IntVal(v1 lor v2)  
				| Op_and -> IntVal(v1 land v2)
				| Op_xor -> IntVal(v1 lxor v2)
				| Op_shl -> IntVal(v1 lsl v2)
				| Op_shr -> IntVal(v1 lsr v2)
				| Op_eq -> BoolVal(v1 == v2) (* the comparisons should work with every type *)
				| Op_ne -> BoolVal(v1 != v2) 
				| Op_gt -> BoolVal(v1 > v2) 
				| Op_lt -> BoolVal(v1 < v2) 
				| Op_ge -> BoolVal(v1 >= v2)
				| Op_le -> BoolVal(v1 <= v2)
				(*| Op_shrr *)
				end
	| FltVal(v1),FltVal(v2) -> begin
				match op with
				| Op_add -> FltVal(v1 +. v2)
				| Op_sub -> FltVal(v1 -. v2)
				| Op_mul -> FltVal(v1 *. v2)
				| Op_div -> FltVal(v1 /. v2) (* exception when v2 is 0 *)
				end
	| BoolVal(v1),BoolVal(v2) -> begin
				match op with 
				| Op_cand -> BoolVal(v1 && v2)
				| Op_cor -> BoolVal(v1 || v2)
				end
	| _,_ -> raise (Exception "Not yet implemented or incorrect operation")

(* do var++ and var--*)
let rec execute_postfix (jprog : jvm) (e : expression) postop =
	(* see what type *) 
	let one = { edesc = Val(Int("1")) }
	in
	match postop with
	| Incr -> execute_assign jprog e Ass_add one
	| Decr -> execute_assign jprog e Ass_sub one
	

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

(* do assignments *)
and execute_assign (jprog : jvm) e1 (op : assign_op) e2 =
	(* see what type *)
	let right = (execute_expression jprog e2)
	in
	let (_, scope) = Stack.top jprog.jvmstack 
	in
	match e1.edesc with
				| Name(n) -> begin
							match op with
							| Assign -> Hashtbl.replace scope.visible n right
							| Ass_add -> Hashtbl.replace scope.visible n (execute_operator jprog e1 Op_add e2)
							| Ass_sub -> Hashtbl.replace scope.visible n (execute_operator jprog e1 Op_sub e2)
							| Ass_mul -> Hashtbl.replace scope.visible n (execute_operator jprog e1 Op_mul e2)
							| Ass_div -> Hashtbl.replace scope.visible n (execute_operator jprog e1 Op_div e2)
							| Ass_mod -> Hashtbl.replace scope.visible n (execute_operator jprog e1 Op_div e2)
							| Ass_and -> Hashtbl.replace scope.visible n (execute_operator jprog e1 Op_and e2) (* and or cand ?? *)
							| Ass_or -> Hashtbl.replace scope.visible n (execute_operator jprog e1 Op_or e2) (* or or cor ?? *)
							| Ass_xor -> Hashtbl.replace scope.visible n (execute_operator jprog e1 Op_xor e2)
							| Ass_shl -> Hashtbl.replace scope.visible n (execute_operator jprog e1 Op_shl e2)
							| Ass_shr -> Hashtbl.replace scope.visible n (execute_operator jprog e1 Op_shr e2)
							(*| Ass_shrr*)
							end;
							Hashtbl.find scope.visible n
				| _ -> raise (Exception "Bad assignment")

(* variable linking *)
and execute_name (jprog : jvm) (name : string) =
	let (_, scope) = Stack.top jprog.jvmstack 
	in
	try 
		Hashtbl.find scope.visible name
	with
	| Not_found -> raise (Exception "Variable not defined")

(* execute operation *)
and execute_operator (jprog : jvm) e1 (op : infix_op) e2 =
	let left = (execute_expression jprog e1)
	in 
	let right = (execute_expression jprog e2)
	in
	compute_value op left right

(* values from AST types *)
and execute_val v =
	match v with
	| String(s) -> StrVal(s)
	| Int(i) -> IntVal(int_of_string i)
	| Float(f) -> FltVal(float_of_string f)
	| Boolean(b) -> BoolVal(b) 
	| Null -> NullVal

(* execute a ternaru a > b ? a: b *)
and execute_ternary (jprog : jvm) exp1 exp2 exp3 =
	match (execute_expression jprog exp1) with
	| BoolVal(true) -> execute_expression jprog exp2
	| BoolVal(false) -> execute_expression jprog exp3
	| _ -> raise (Exception "Illegal ternary operator values")

(* execute an expression and send back it's value *)
and execute_expression (jprog : jvm) expr =
	(* check the descriptor *)
	match expr.edesc with 
	| Val(v) -> execute_val v
			  (*| Char of char option
				*)
	| Post(e, poi) -> execute_postfix jprog e poi
	| Pre(pri, e) -> execute_prefix jprog pri e
	| Name(n) -> execute_name jprog n
	| AssignExp(e1, op, e2) -> execute_assign jprog e1 op e2
	| Op(e1, op, e2) -> execute_operator jprog e1 op e2
	| CondOp(e1, e2, e3) -> execute_ternary jprog e1 e2 e3 (* this is actually the ternary *)
	(* | New of string option * string list * expression list
	| NewArray of Type.t * (expression option) list * expression option
	| Call of expression option * string * expression list
	| Attr of expression * string
	| ArrayInit of expression list
	| Array of expression * (expression option) list
	| Cast of Type.t * expression
	| Type of Type.t
	| ClassOf of Type.t
	| Instanceof of expression * Type.t
	| VoidClass
 *)

(* execute a variable declaration *)
let rec execute_vardecl (jprog : jvm) decls declpairs = 
	match decls with
	| [] -> declpairs
	| hd::tl -> begin
				match hd with
				(* type, name, optional initialization *)
				| (Primitive(p), n, eo) -> 
							let v = (match eo with | None -> Hashtbl.find jprog.defaults p
												   | Some(e) -> execute_expression jprog e) 
							in
							execute_vardecl jprog tl (declpairs@[(n, v)]) (* return a list of tuple (name * value) *)
				end
				(*
				| Array(typ,size) -> (stringOf typ)^(array_param size)
				| Ref rt -> stringOf_ref rt 
				*)
(* execute an if condition with possible else *)
let rec execute_if (jprog : jvm) e (stmt : statement) elseopt =
	match (execute_expression jprog e) with
	| BoolVal(true) -> execute_statement jprog stmt
	| BoolVal(false) -> begin
				match elseopt with 
				| Some(s) -> execute_statement jprog s
				| _ -> ()
				end
	| _ -> raise (Exception "Illegal ternary operator values")

and execute_while (jprog : jvm) e (stmt : statement) = 
	match (execute_expression jprog e) with
	| BoolVal(true) -> execute_statement jprog stmt; execute_while jprog e stmt
	| BoolVal(false) -> ()
	| _ -> raise (Exception "Illegal condition in while loop")

(* blocks have different scope *)
and execute_block (jprog : jvm) (stmt : statement) =
	match stmt with
	(* a variable declaration *)
	| _ -> execute_statement jprog stmt;

(* execute all sorts of statements *)
and execute_statement (jprog : jvm) (stmt : statement) = 
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
	| VarDecl(vardecls) -> let declarations = execute_vardecl jprog vardecls []
						in
						add_vars_to_scope jprog declarations
	(* blocks *)
	| Block(stmtlst) -> let blockvars = [] 
						in
						List.iter (execute_block jprog) stmtlst

	(* the if statement *)
	| If(e, stmt, elseopt) -> execute_if jprog e stmt elseopt
	(* while *)
	| While(e, s) -> execute_while jprog e s 
	(* the for loop *)
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
