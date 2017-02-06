(* compile the ASTTyped *)
open AST
open MemoryModel
(* put the classes into a hashtable *)
let add_class table cls =
	Hashtbl.add table cls.id cls

(* put the methods into a hashtable *)
let add_method table meth =
	Hashtbl.add table meth.mname meth

(* take the environement and the ast *)
let add_classes env prog =
	List.iter (fun t -> add_class env.classes t) prog.type_list

(* take the environement and add the methods to the global table *)
let add_methods env = 
	(* 
		iterate through the hash table
		take only classes (since interfaces are not supported)
		for classes, loop through their methods
		add the methods
	*)
	Hashtbl.iter (fun key value -> 
				match value.info with 
					| Class(c) -> List.iter (add_method env.methods) c.cmethods
					| Inter -> ()
				) env.classes

(* program is the AST *)
let compile_tree program = 
	(match program.package with
  	| None -> ()
	| Some pack -> AST.print_package pack );
  	(* List.iter (fun t -> AST.print_type "" t; print_newline()) program.type_list *)
  	let jprog = { methods = Hashtbl.create 10; classes = Hashtbl.create 10 } 
  	in
  	(* add the classes *)
  	add_classes jprog program;
  	(* once we have classes, find methods *)
  	add_methods jprog;
  	(* print the current state *)
  	print_jvm_class jprog