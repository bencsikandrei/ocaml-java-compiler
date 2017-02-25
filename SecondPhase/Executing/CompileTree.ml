(* compile the ASTTyped *)
open AST
open MemoryModel
open Exceptions
open Type
open Defaults

(* for the logs 
	if true, logs print, else they are mutted
*)
let verbose = ref true

(* ------------------------------- UTILITY ------------------------------------ *)
(* find in a list, return -1 if not found *)
let has_modifier lst elem =
	(* tail recursive function *)
	let rec find lst elem cnt = 
		match lst with 
		| [] -> -1
		| hd::tl -> if ((stringOf_modifier hd) = elem) then cnt else find tl elem (cnt+1) in
	(* return the index *)
	find lst elem 0

(* eliminates programs with simple errors *)
let check_for_errors jprog fname cls =
	(* check whether the class is public or not *)
	if ((has_modifier cls.modifiers "public") <> -1) 
	then begin
		(* is there already a public class ? *)
		if (jprog.public_class_present = true) 
		then 
			raise (Exceptions.PublicClassName ("class " ^ cls.id ^ " is public, " ^
												"should be declared in a file " ^
												"named " ^ fname ^ ".java"))
		else
			(* mark that a public class is found *)
			jprog.public_class_present <- true;
			jprog.public_class <- cls.id;
			(* check if the class is in the correct file *)
			if (fname <> cls.id) 
			then begin
				raise (Exceptions.PublicClassName ("class " ^ cls.id ^ " is public, " ^
												"should be declared in a file " ^
												"named " ^ fname ^ ".java"))
			end
	end

(* get the parant of a class fromt  *)
let rec get_parent (clslist : AST.asttype list) (parent : string) =
	(* match all classes with the parent, 
	raise exception if not found *)
	match clslist with
	| hd::tl -> if (hd.id = parent) 
				then 
					hd
				else 
					get_parent tl parent
	| _ -> raise (Exceptions.UnknownSymbol ("error: cannot find symbol : class " ^ parent))

(* -------------------------------------------------------------------------------------- *)

(* ---------------------------------- METHODS ------------------------------------- *)
(* get method signatures *)
let rec get_method_signature (params : AST.argument list) (strparams : string) =
	let spars = match params with
				| [] -> strparams (* we are done *)
				| hd::tl -> match hd.ptype with
							| Primitive(Int) -> get_method_signature tl (strparams^"_int")
							| Ref(rt) -> get_method_signature tl (strparams^"_"^rt.tid)
							| Array(t, int) -> match t with 
												| Primitive(Int) -> get_method_signature tl (strparams^"_int[]")
												| Ref(rt) -> get_method_signature tl (strparams^"_"^rt.tid^"[]")
							| _ -> ""
	in
	if (spars = "")
		then 
			"_void"
	else
		spars

(* put the methods into a hashtable *)
let add_method (jprog : jvm) clsmeth (clsname : string) (meth : AST.astmethod)  =
	(* for dynamic link / late binding 
	we need to keep some special tables *)
	let signature = (get_method_signature meth.margstype "")
	in
	let jvmmname = clsname ^ "_" ^ meth.mname ^ signature
	in
	(* raise an error if the method is already there *)
	if Hashtbl.mem jprog.methods jvmmname = true
	then begin
		raise (Exceptions.MethodAlreadyDefined ("method " ^ jvmmname ^ " is already declared"))
	end;
	Hashtbl.add jprog.methods jvmmname meth;
	Hashtbl.add clsmeth (meth.mname^signature) jvmmname

(* add methods from parent *)
let add_method_from_parent (jprog : jvm) clsmeth (methname : string) (methfullname : string) =
	(* check for the method in the JVM table *)
	(* print_endline "in add method from parents "; *)
	(* print_jvm jprog; *)
	Log.debug "Adding methods from parent";
	let m = Hashtbl.find jprog.methods methfullname
	in
	(* print_endline "after the find "; *)

	(* see if it's private *)
	let prv = has_modifier m.mmodifiers "private" 
	in 
	(* if not private and not main *)
	if Hashtbl.mem clsmeth methname = false && methname <> "main_String[]" && (prv < 0)
	then
		(* add it *)
		Hashtbl.add clsmeth methname methfullname 

(* take the environement and add the methods to the global table *)
let add_methods (jprog : jvm) (c : AST.astclass) (clsname : string)= 
	 
	(*  iterate through the hash table
		take only classes (since interfaces are not supported)
		for classes, loop through their methods
		add the methods *)
	Log.debug "Adding methods from the class";
	let methods = Hashtbl.create 20 
	in
	List.iter (add_method jprog methods clsname) c.cmethods;
	if c.cparent.tid <> ""
	then begin
		let theparent = Hashtbl.find jprog.classes c.cparent.tid in
		Hashtbl.iter (fun key value -> add_method_from_parent jprog methods key value) theparent.jcmethods;
	end;
	methods
(* -------------------------------------------------------------------------------------- *)

(* ----------------------------- CONSTRUCTORS ----------------------------------------------- *)
(* adds a constructor to the hashtable clsconstr *)
let add_constructor clsconstr (constr : AST.astconst) =
	(* for overloading constructors *)
	let signature = (get_method_signature constr.cargstype "")
	in
	Hashtbl.add clsconstr (constr.cname ^ signature) constr 

(* adds the empty constructor that does absolutely nothing *)
let get_empty_constructor (classname : string) = 
	let empty_const = { cmodifiers = [Public];
					    cname = classname;
					    cargstype = [];
					    cthrows = [];
					    cbody = [];
					    mloc = Location.none
					    } 
	in
	empty_const

(* add the constructors from the ast *)
let add_constructors (c : AST.astclass) (classname : string) =
	(* get every constructor, put it
	into the hashtable, also put signature
	permits overloading *)
	Log.debug "Adding constructors";
	let constructors = Hashtbl.create 10
	in
	(* check if there is a constructor defined *)
	(match c.cconsts with
	| [] -> add_constructor constructors (get_empty_constructor classname) (* an empty list means no constructor*)
	| _  -> List.iter (add_constructor constructors) c.cconsts); (* non empty list means there is at least one *)
	(* return the hashtable *)
	constructors
(* -------------------------------------------------------------------------------------- *)

(* ----------------------------- ATTRIBUTES ----------------------------------------------- *)
(* got through parents attributes, see if not private and inherit *)
let rec get_parent_attributes (pattributes : astattribute list) (attrlst :  astattribute list) =
	(* check each element for its modifiers *)
	match pattributes with 
	| [] -> attrlst
	| hd::tl -> 
			(* does it contain private ? *)
			let prv = has_modifier hd.amodifiers "private" 
			in
			if (prv < 0) 
			then
				(* add non private *)
				get_parent_attributes tl (attrlst @ [hd])
			else 
				(* don't add private ones *)
				get_parent_attributes tl (attrlst)
(* add all attributes to that class, including the ones from the parent *)
let add_attributes (jprog : jvm) (c : AST.astclass) =
	(* Object is not taken care of *)
	Log.debug "Adding attributes";
	if c.cparent.tid <> ""
	then begin
		let parent = Hashtbl.find jprog.classes c.cparent.tid
		in
		c.cattributes @ (get_parent_attributes parent.jattributes [])
	end
	else
		c.cattributes 
(* -------------------------------------------------------------------------------------- *)

(* ------------------------------ CLASSES ------------------------------------ *)
(* return true if a class is already added *)
let class_added (jprog : jvm) (clsname : string) = 
	(* print_endline ("Testing " ^ clsname); *)
	match clsname with
	| "" -> true
	| _ -> Hashtbl.mem jprog.classes clsname

(* put the classes into a hashtable *)
let rec add_class (jprog : jvm) ast (fname : string) (cls : AST.asttype) =
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
				Log.debug ("Checking parent " ^ c.cparent.tid);
				(* add the parent *)
				try 
					add_class jprog ast fname (get_parent ast.type_list c.cparent.tid)
				with
				(* raise an exception if not found *)
				| Exceptions.UnknownSymbol(e) -> print_endline e; Location.print c.cloc; exit (-1)
			end;
			(* now that all parents are added, add the class *)
			Log.debug ("Adding class now " ^ cls.id);
			let javacls = {		id =  cls.id; 
								cparent = c.cparent;
							    jattributes = (add_attributes jprog c);
							    cinits = c.cinits;
							    jconsts = (add_constructors c cls.id);
							    jcmethods = (add_methods jprog c cls.id)
								}				
			in
			(* add the class to the programm classes *)
			Hashtbl.add jprog.classes cls.id javacls;
			
		| _ -> ()
	end

(* take the environement and the ast *)
let add_classes (jprog : jvm) ast (fname : string) =
	List.iter (add_class jprog ast fname) ast.type_list
(* -------------------------------------------------------------------------------------- *)
(* program is the AST *)
let compile_tree (verb : bool) ast (fname : string) = 
	(* change verbosity *)
	verbose := verb;
	(match ast.package with
  	| None -> ()
	| Some pack -> AST.print_package pack );
  	(* List.iter (fun t -> AST.print_type "" t; print_newline()) ast.type_list *)
  	let jprog = { public_class_present = false; 
  		public_class = "";
  		scope_class = "";
  		methods = Hashtbl.create 10; 
  		classes = Hashtbl.create 10;
  		defaults = Hashtbl.create 10;
  		jvmstack = Stack.create ();
  		nextfree = 0;
  		jvmheap = Hashtbl.create 10;
  		} 
  	in
  	(* the jvm automatically includes javalang *)
  	let enhancedast = Defaults.add_default_classes verb ast
  	in
  	(* add the classes *)
  	add_classes jprog enhancedast fname;
  	(* once we have classes, find methods *)
  	(* add_methods jprog; *)
  	(* print the current state *)
  	Log.debug "----- Printing JVM contents -----";
  	(* initiate logs with verbosity level *)
    MemoryModel.initiate_logs verb;
  	print_jvm jprog;
  	(* print classes *)
  	Log.debug "----- Printing each class contents -----";
  	Hashtbl.iter (fun key value -> print_jclass value) jprog.classes;
  	jprog