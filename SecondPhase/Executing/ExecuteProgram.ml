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
	let (_, scope) =  Stack.top jprog.jvmstack 
	in 
	match decls with
	| [] -> ()
	| hd::tl -> let (n, v) = hd
			in
			(* print_string n; print_endline( "= " ^(string_of_value v)); *)
			Hashtbl.add scope.visible n v;
			add_vars_to_scope jprog tl

(* remove block variables *)
let rec remove_vars_from_scope (jprog : jvm) decls =
	(* print_endline "Any vars to remove from scope ?"; *)
	let (_, scope) =  Stack.top jprog.jvmstack 
	in 
	match decls with
	| [] -> ()
	| hd::tl -> let (n, v) = hd
			in
			(* print_endline "Some"; *)
			(* print_string n; print_endline(string_of_value v); *)
			Hashtbl.remove scope.visible n;
			remove_vars_from_scope jprog tl

(* compute the value of an expression with operators *)
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

(* execute a list of expressions *)
and execute_expressions (jprog : jvm) exps =
	match exps with
	| [] -> []
	| hd::tl -> let decl = execute_expression jprog hd 
				in
				[decl] @ (execute_expressions jprog tl)

(* execute the call, with the method name and parameters *)
(*and execute_call (jprog : jvm) (expo : expression option) (meth : string) (params : expression list) =
	(* if expo is none, it must be in the current scope *)
	match expo with
    | Some e -> (execute_expression e)
    | None -> meth (execute_expressions jprog el)
*)
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
	| ArrayInit(el) -> ArrayVal({aname=None;avals=(execute_expressions jprog el);adim=IntVal(List.length el)}) (* TODO check if all elems of same type *)
	| NewArray(t,expol,expo) -> (* type, dimension, initialization *)
								let init=(List.map (fun expo -> (match expo with | None -> NullVal
																				| Some(e) -> (execute_expression jprog e))) expol)
								in
								let dim=(match expo with	| None -> NullVal
															| Some(e) -> execute_expression jprog e)
								in
								ArrayVal({aname=None;avals=init;adim=dim})
	| Type(t) -> TypeVal(t)
	| Instanceof(e,t) -> begin 
						match (execute_expression jprog e),t with
						| RefVal(r1),Ref(r2) -> if (r1.oname = r2.tid) then BoolVal(true) else BoolVal(false) (* not considering path yet TODO *)
						| _,_ -> BoolVal(false)
						end
	| New(stro,strl,expl) -> (* something ?? classname args *)
							let str = (match stro with | None -> ""
													   | Some(s) -> s)
							in
							StrVal(str^"--"^(String.concat "," strl)^"--"^(ListII.concat_map "," string_of_expression expl))
	| Call(obj, name, args) -> begin
			match name with
			| "println" -> print_endline (string_of_value (execute_expression jprog (List.hd args))); 
			| _ -> print_endline "Call not executable yet, try a System.out.println().."; 
			end; NullVal
	| _ -> StrVal("Not yet implemented")			    	
	(* 
	| Call(expo,meth,el) -> execute_call jprog expo meth el
	| Attr of expression * string
	| Array of expression * (expression option) list
	| Cast of Type.t * expression
	| ClassOf of Type.t
	| VoidClass
 *)

(* execute a variable declaration *)
let rec execute_vardecl (jprog : jvm) (decls : (Type.t * string * expression option) list) declpairs = 
	(* at the end give a list of all declared variables *)
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
			| (Array(t,size), n, eo) -> 
					let v = (match eo with | None -> ArrayVal({aname=Some(n);adim=IntVal(size);avals=[]}) (* TODO initialize default according to size *)
										   | Some(e) -> execute_expression jprog e)
					in
					execute_vardecl jprog tl (declpairs@[(n, v)])
			| (Ref(rt), n, eo) -> 
					let v = (match eo with | None -> NullVal
										   | Some(e) -> execute_expression jprog e)
					in
					execute_vardecl jprog tl (declpairs@[(n, v)])
			end

let rec execute_for_vardecl (jprog : jvm) (decls : (Type.t option * string * expression option) list) declpairs = 
	(* at the end give a list of all declared variables *)
	match decls with
	| [] -> declpairs
	| hd::tl -> begin
			match hd with
			(* type, name, optional initialization *)
			| (Some(Primitive(p)), n, eo) -> 
					let v = (match eo with | None -> Hashtbl.find jprog.defaults p
										   | Some(e) -> execute_expression jprog e) 
					in
					execute_for_vardecl jprog tl (declpairs@[(n, v)]) (* return a list of tuple (name * value) *)
			| (None, n, eo) -> 
					let v = (match eo with | Some(e) -> execute_expression jprog e) 
					in
					execute_for_vardecl jprog tl (declpairs) (* return a list of tuple (name * value) *)
			end

(* execute an if condition with possible else *)
let rec execute_if (jprog : jvm) e (stmt : statement) elseopt =
	match (execute_expression jprog e) with
	| BoolVal(true) -> execute_statement jprog stmt
	| BoolVal(false) -> begin
			match elseopt with 
			(* execute the else part *)
			| Some(s) -> execute_statement jprog s
			| _ -> []
			end
	| _ -> raise (Exception "Illegal if condition")

(* simple while loop *)
and execute_while (jprog : jvm) e (stmt : statement) = 
	match (execute_expression jprog e) with
	| BoolVal(true) -> execute_statement jprog stmt; execute_while jprog e stmt
	| BoolVal(false) -> ()
	| _ -> raise (Exception "Illegal condition in while loop")

(* the simple for statement, not enhanced *)
and execute_for (jprog : jvm) fortest forincr (stmt : statement) =
	let test = (match fortest with 
				| Some(e) -> execute_expression jprog e
				| None -> BoolVal(true))
	in
	match test with
	| BoolVal(true) -> 	execute_statement jprog stmt; 
						List.iter (fun t -> execute_expression jprog t; ()) forincr;
						execute_for jprog fortest forincr stmt
	| BoolVal(false) -> ()
	| _ -> raise (Exception "Illegal condition in while loop")

(* execute all sorts of statements *)
and execute_statement (jprog : jvm) (stmt : statement) : (string * MemoryModel.valuetype) list = 
	match stmt with
	(* treat all the expressions *)
	| Expr(e) -> execute_expression jprog e; (* print_endline (AST.string_of_expression e) *)
				[]
	(* a variable declaration *)
	| VarDecl(vardecls) -> let declarations = execute_vardecl jprog vardecls []
			in
			add_vars_to_scope jprog declarations;
			declarations
	(* blocks *)
	| Block(stmtlst) -> let blockvars =	List.append [] (execute_statements jprog stmtlst) 
			in
			remove_vars_from_scope jprog blockvars;
			[]
			
	(* the if statement *)
	| If(e, stmt, elseopt) -> execute_if jprog e stmt elseopt; []
	(* while *)
	| While(test, stmt) -> execute_while jprog test stmt; []
	(* while *)
	| For(vardecls, fortest, forincr, forstmt) ->
			(* first add the for declared variables to scope *)
			let declarations = execute_for_vardecl jprog vardecls []
			in
			add_vars_to_scope jprog declarations;
			execute_for jprog fortest forincr forstmt;
			remove_vars_from_scope jprog declarations;
			[]
	(* the for loop *)
	| _ -> print_endline "Statement not executable yet, try a System.out.println().."; []

(* execute statements like List.iter 
	a statement can decalre a new variable or not
	it's the case for VarDecl
	so if a block declares a variable, we add it 
	to a list
	*)
and execute_statements (jprog : jvm) (stmts : statement list) =
	match stmts with
	| [] -> []
	| hd::tl -> (* execute one statement, then pass to others *)
				let decl = execute_statement jprog hd 
				in
				decl @ (execute_statements jprog tl)

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
	execute_statements jprog startpoint.mbody;
	print_scope jprog
