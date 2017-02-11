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


(* put the methods into a hashtable *)
let add_method jprog clsmeth clsname meth  =
	(* for dynamic link / late binding we need to keep some special tables *)
	let mname = clsname ^ "_" ^ meth.mname 
	in
	Hashtbl.add jprog.methods mname meth;
	Hashtbl.add clsmeth meth.mname mname

(* take the environement and add the methods to the global table *)
let add_methods jprog c clsname = 
	 
(* 		iterate through the hash table
		take only classes (since interfaces are not supported)
		for classes, loop through their methods
		add the methods *)
	let methods = Hashtbl.create 20 
	in
	List.iter (add_method jprog methods clsname) c.cmethods;
	methods

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
							    jcmethods = (add_methods jprog c cls.id)
								}
				| _ -> {	id =  ""; 
							cparent = { tpath = []; tid = "" };
						    cattributes = [];
						    cinits = [];
						    cconsts = [];
						    jcmethods = Hashtbl.create 1
							}
	in
	(* add the class to the programm classes *)
	Hashtbl.add jprog.classes cls.id javacls;
	(* mark that a public class is found *)
	jprog.public_class_present <- true


(* take the environement and the ast *)
let add_classes jprog ast fname =
	List.iter (fun t -> add_class jprog fname t) ast.type_list
	



(* program is the AST *)
let compile_tree ast fname = 
	(match ast.package with
  	| None -> ()
	| Some pack -> AST.print_package pack );
  	(* List.iter (fun t -> AST.print_type "" t; print_newline()) ast.type_list *)
  	let jprog = { public_class_present = false; methods = Hashtbl.create 20; classes = Hashtbl.create 20 } 
  	in
  	(* add the classes *)
  	add_classes jprog ast fname;
  	(* once we have classes, find methods *)
  	(* add_methods jprog; *)
  	(* print the current state *)
  	print_jvm jprog