(* all memory related issues : heap, stack .. *)
open AST
open Hashtbl
open Type
open Log
open Printf

type javaclass = {
	(* a type to hold class methods, attributes, constructors,
	name, paret class, etc
	resembles the AST.asttype - but uses Hashtbl for storing *)
	id: string;
	cparent : Type.ref_type;
    jattributes : astattribute list;
    cinits : initial list;
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
	(* TODO, chage this to take an address *)
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
	| IntVal(i) -> string_of_int i 
	| StrVal(s) -> s
 	| FltVal(f) -> string_of_float f
 	| BoolVal(b) -> string_of_bool b
 	| ArrayVal(a) -> "["^ListII.concat_map "," string_of_value a.avals^"]"
	| RefVal(addr) -> " @ "^(sprintf "0x%08x" addr)
	| NullVal -> "Null"
	| _ -> ""

(* ------------------------------ PRINTS ------------------------------------ *)
let print_scope jvm = 
	print_endline "# Printing scope #";
	try 
		match (Stack.top jvm.jvmstack) with 
		| (s, v) -> print_endline s; 
				Hashtbl.iter (fun key value -> print_string (key ^" = "); 
										print_endline (string_of_value value)) v.visible
	with
	| _ -> print_endline "Program exited normally"

(* contents of heap *)
let print_heap jvm =
	print_endline "### The HEAP ###";
	(* Hashtbl.iter (fun key value -> print_endline key; print_endline value.mname) jmc.methods; *)
	Hashtbl.iter (fun key value -> print_endline (" Object: "^value.oclass.id);
									Printf.printf " @ address: 0x%08x" key
										) jvm.jvmheap

(* the whole content of it *)
let print_jvm jvm = 
	(* Hashtbl.iter (fun key value -> print_endline key; print_endline value.mname) jmc.methods; *)
	Hashtbl.iter (fun key value -> print_string ("class name: "^key); 
									print_endline (" class in jvm: "^value.id)) jvm.classes;

	Hashtbl.iter (fun key value -> print_string ("method name: "^key); 
									print_endline (" method in jvm: "^value.mname)) jvm.methods;

	print_endline ("Public class present: " ^ (string_of_bool jvm.public_class_present));

	print_endline ("The public class is: " ^ jvm.public_class)

let print_jclass jclass =
	print_endline ("### Class " ^ jclass.id ^ " ###");
	(* print all attributes *)
	List.iter (fun t -> AST.print_attribute "" t) jclass.jattributes; 
	(* print the class constructors *)
	Hashtbl.iter (fun key value -> print_string ("constructor: " ^ key);
									print_endline (" | constructor full name: " ^value.cname)) jclass.jconsts;
	(* print the class methods and attributes *)
	Hashtbl.iter (fun key value -> print_string ("method: " ^ key);
									print_endline (" | method in jvm table: " ^value)) jclass.jcmethods