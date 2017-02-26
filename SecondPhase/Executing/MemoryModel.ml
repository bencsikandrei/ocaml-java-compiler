(* all memory related issues : heap, stack .. *)
open AST
open Hashtbl
open Type
open Log
open Printf

let verbose = ref true;;

type javaclass = {
	(* a type to hold class methods, attributes, constructors,
	name, paret class, etc
	resembles the AST.asttype - but uses Hashtbl for storing *)
	id: string;
	cparent : Type.ref_type;
    jattributes : astattribute list;
    cinits : initial list;
    ctypes : string list; (* list ids of inner classes *)
    jconsts : (string, astconst) Hashtbl.t;
    jcmethods : (string, string) Hashtbl.t
}

(* default initializers for values *)
type default = {
	values: (primitive, valuetype) Hashtbl.t;
}

(* what can we use *)
and valuetype = 
	| TypeVal of Type.t
	| IntVal of  int
	| StrVal of string
	| FltVal of float
	| BoolVal of bool
	| ArrayVal of array
	| RefVal of int
	| VoidVal 
	| NullVal

(* heap declared objects *)
and newobject = {
	(* the class it instantiates *)
	oclass: javaclass;
	(* its attributes *)
	oattributes: (string, valuetype) Hashtbl.t;
}

(* for arrays *)
and array = {
	aname: string option;
	atype: valuetype;
	adim: valuetype list;
	avals: valuetype list;
}

and scope = {
	(* current *)
	visible: (string, valuetype) Hashtbl.t;
}

(* memory specific to classes and methods in the JVM *)
and jvm = {
	(* public class present ? *)
	mutable public_class_present: bool;
	(* save the public class *)
	mutable public_class: string;
	(* current scope class *)
	mutable scope_class: string;
	(* method names and ast type given *)
	methods: (string, astmethod) Hashtbl.t;
	(* class names *)
	classes: (string, javaclass) Hashtbl.t;
	(* defaults *)
	defaults: (primitive, valuetype) Hashtbl.t;
	(* the stack *)
	jvmstack: ( (string * scope) ) Stack.t;
	(* next free address *)
	mutable nextfree: int;
	(* the heap *)
	jvmheap: (int, newobject) Hashtbl.t;

}
(* -------------------------------------------------------------------------------------- *)

(* ------------------------------ TOSTRING ------------------------------------ *)
(* string of valuetypes *)
let rec string_of_value v =
	match v with
	| TypeVal(t) -> stringOf t
	| IntVal(i) -> string_of_int i 
	| StrVal(s) -> s
 	| FltVal(f) -> string_of_float f
 	| BoolVal(b) -> string_of_bool b
 	| ArrayVal(a) -> "["^ListII.concat_map "," string_of_value a.avals^"]"
	| RefVal(addr) -> " @ "^(sprintf "0x%08x" addr)
	| NullVal -> "null"
	| VoidVal -> "void"

(* ------------------------------ PRINTS ------------------------------------ *)
let print_scope jvm = 
	Log.info "# Printing scope #";
	try 
		match (Stack.top jvm.jvmstack) with 
		| (s, v) -> Log.info s; 
				Hashtbl.iter (fun key value -> Log.info (key ^" = "); 
										Log.info (string_of_value value)) v.visible
	with
	| _ -> print_endline "Program exited normally"

(* contents of heap *)
let print_heap jvm =
	Log.info "### The HEAP ###";
	(* Hashtbl.iter (fun key value -> print_endline key; print_endline value.mname) jmc.methods; *)
	Hashtbl.iter (fun key value -> Log.info (" Object: "^value.oclass.id);
									Log.info (Printf.sprintf " @ address: 0x%08x\n" key)
										) jvm.jvmheap

(* the whole content of it *)
let print_jvm jvm = 
	(* Hashtbl.iter (fun key value -> print_endline key; print_endline value.mname) jmc.methods; *)
	Hashtbl.iter (fun key value -> Log.info ("class name: "^key); 
									Log.info (" class in jvm: "^value.id)) jvm.classes;

	Hashtbl.iter (fun key value -> Log.info ("method name: "^key); 
									Log.info (" method in jvm: "^value.mname)) jvm.methods;

	Log.info ("Public class present: " ^ (string_of_bool jvm.public_class_present));

	Log.info ("The public class is: " ^ jvm.public_class)

let print_jclass jclass =
	Log.info ("### Class " ^ jclass.id ^ " ###");
	(* print all attributes *)
	List.iter (fun t -> AST.print_attribute "" t) jclass.jattributes; 
	(* print the class constructors *)
	Hashtbl.iter (fun key value -> Log.info ("constructor: " ^ key);
									Log.info (" | constructor full name: " ^value.cname)) jclass.jconsts;
	(* print the class methods and attributes *)
	Hashtbl.iter (fun key value -> Log.info ("method: " ^ key);
									Log.info (" | method in jvm table: " ^value)) jclass.jcmethods;
	(* print the class methods and attributes *)
	List.iter (fun x -> Log.info ("inner classes:");
									Log.info (" | inner class in jvm table: " ^x)) jclass.ctypes

(* initiate logs *)
let initiate_logs (verb : bool) =
	(* set verbosity *)
	verbose := verb