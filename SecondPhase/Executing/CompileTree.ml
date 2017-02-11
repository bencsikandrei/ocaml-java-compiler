(* compile the ASTTyped *)
open AST
open MemoryModel
open Exceptions
open Type

(* find in a list, return -1 if not found *)
let find_element lst elem =
	(* tail recursive function *)
	let rec find lst elem cnt = 
		match lst with 
		| [] -> -1
		| hd::tl -> if ((stringOf_modifier hd) = elem) then cnt else find tl elem (cnt+1) in
	(* return the index *)
	find lst elem 0

(* put the classes into a hashtable *)
let add_class jprog fname cls =
	(* check whether the class is public or not *)
	if ((find_element cls.modifiers "public") <> -1) 
	then begin
		(* is there already a public class ? *)
		if (jprog.public_class_present = true) 
		then 
			raise (Exceptions.PublicClassName ("class " ^ cls.id ^ " is public, " ^
												"should be declared in a file " ^
												"named " ^ fname ^ ".java"))
		else
			(* check if the class is in the correct file *)
			if (fname <> cls.id) 
			then begin
				raise (Exceptions.PublicClassName ("class " ^ cls.id ^ " is public, " ^
												"should be declared in a file " ^
												"named " ^ fname ^ ".java"))
			end
	end;
	(* create the object *)
	let javacls = match cls.info with 
		  		| Class(c) -> {	id =  cls.id; 
								cparent = c.cparent;
							    cattributes = c.cattributes;
							    cinits = c.cinits;
							    cconsts = c.cconsts;
							    cmethods = Hashtbl.create 20
								}
				| _ -> {	id =  ""; 
							cparent = { tpath = []; tid = "" };
						    cattributes = [];
						    cinits = [];
						    cconsts = [];
						    cmethods = Hashtbl.create 1
							}
	in
	(* add the class to the programm classes *)
	Hashtbl.add jprog.classes cls.id javacls;
	(* mark that a public class is found *)
	jprog.public_class_present <- true

(* put the methods into a hashtable *)
let add_method table meth =
	Hashtbl.add table meth.mname meth

(* take the environement and the ast *)
let add_classes jprog prog fname =
	List.iter (fun t -> add_class jprog fname t) prog.type_list
	

(* take the environement and add the methods to the global table *)
(* let add_methods env = 
	 
		iterate through the hash table
		take only classes (since interfaces are not supported)
		for classes, loop through their methods
		add the methods
	
	Hashtbl.iter (fun key value -> 
				match value.info with 
					| Class(c) -> List.iter (add_method env.methods) c.cmethods
					| Inter -> ()
				) env.classes *)

(* program is the AST *)
let compile_tree program fname = 
	(match program.package with
  	| None -> ()
	| Some pack -> AST.print_package pack );
  	(* List.iter (fun t -> AST.print_type "" t; print_newline()) program.type_list *)
  	let jprog = { public_class_present = false; methods = Hashtbl.create 20; classes = Hashtbl.create 20 } 
  	in
  	(* add the classes *)
  	add_classes jprog program fname;
  	(* once we have classes, find methods *)
  	(* add_methods jprog; *)
  	(* print the current state *)
  	print_jvm jprog