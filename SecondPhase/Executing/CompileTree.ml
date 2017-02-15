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

let check_for_errors jprog fname cls =
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
	end

(* put the methods into a hashtable *)
let add_method jprog clsmeth clsname meth  =
	(* for dynamic link / late binding we need to keep some special tables *)
	let mname = clsname ^ "_" ^ meth.mname 
	in
	(* raise an error if the method is already there *)
	if Hashtbl.mem jprog.methods mname = true
	then begin
		raise (Exceptions.MethodAlreadyDefined ("method " ^ mname ^ " is already declared"))
	end;
	Hashtbl.add jprog.methods mname meth;
	Hashtbl.add clsmeth meth.mname mname
(* add methods from parent *)
let add_method_from_parent jprog clsmeth methname methfullname =
	(* check for the method in the JVM table *)
	print_endline "in add method from parents ";
	print_jvm jprog;
	let m = Hashtbl.find jprog.methods methfullname
	in
	print_endline "after the find ";

	(* see if it's private *)
	let prv = find_element m.mmodifiers "private" 
	in 
	(* if not private and not main *)
	if Hashtbl.mem clsmeth methname = false && methname <> "main" && (prv < 0)
	then
		(* add it *)
		Hashtbl.add clsmeth methname methfullname 

(* take the environement and add the methods to the global table *)
let add_methods jprog (c : AST.astclass) clsname = 
	 
(* 		iterate through the hash table
		take only classes (since interfaces are not supported)
		for classes, loop through their methods
		add the methods *)
	let methods = Hashtbl.create 20 
	in
	List.iter (add_method jprog methods clsname) c.cmethods;
	if c.cparent.tid <> "Object" 
	then begin
		let theparent = Hashtbl.find jprog.classes c.cparent.tid in
		Hashtbl.iter (fun key value -> add_method_from_parent jprog methods key value) theparent.jcmethods;
	end;
	methods

let rec get_parent (clslist : AST.asttype list) parent =
	match clslist with
	| hd::tl -> if (hd.id = parent) 
				then 
					hd
				else 
					get_parent tl parent

(* return true if a class is already added *)
let class_added jprog clsname = 
	print_endline ("Testing " ^ clsname);
	match clsname with
	| "Object" -> true
	| _ -> Hashtbl.mem jprog.classes clsname

(* put the classes into a hashtable *)
let rec add_class jprog ast fname (cls : AST.asttype) =
	(* check if the class is already added *)
	if (class_added jprog cls.id) = false
	then begin
		(* check that class is public, name is good .. *)
		check_for_errors jprog fname cls;
		(* check if class or interface *)
		match cls.info with
		| Class(c) -> 
			(* if the parent has not already been added *)
			if (class_added jprog c.cparent.tid) = false 
			then begin
				print_endline ("Adding parent " ^ c.cparent.tid);
				(* add the parent *)
				add_class jprog ast fname (get_parent ast.type_list c.cparent.tid)
			end;
			(* now that all parents are added, add the class *)
			print_endline ("Adding class " ^ cls.id);
			let javacls = {		id =  cls.id; 
								cparent = c.cparent;
							    cattributes = c.cattributes;
							    cinits = c.cinits;
							    cconsts = c.cconsts;
							    jcmethods = (add_methods jprog c cls.id)
								}
				
			in
			(* add the class to the programm classes *)
			Hashtbl.add jprog.classes cls.id javacls;
			(* mark that a public class is found *)
			jprog.public_class_present <- true
			
		| _ -> ()
	end

(* take the environement and the ast *)
let add_classes jprog ast fname =
	List.iter (add_class jprog ast fname) ast.type_list

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
  	print_jvm jprog;
  	(* print classes *)
  	Hashtbl.iter (fun key value -> print_jclass value) jprog.classes