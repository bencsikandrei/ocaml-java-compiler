(* all memory related issues : heap, stack .. *)
open AST
open Hashtbl
open Type

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
	| IntVal of int
	| StrVal of string
	| FltVal of float
	| BoolVal of bool
	| ArrayVal of array
	| RefVal of newobject
	| NullVal

(* heap declared objects *)
and newobject = {
	(* the object declaration name *)
	oname: string;
	(* the class it instantiates *)
	oclass: javaclass;
	(* its attributes *)
	oattributes: (string, valuetype) Hashtbl.t;
}

(* for arrays *)
and array = {
	aname: string option;
	adim: valuetype;
	avals: valuetype list;
}

type scope = {
	(* current *)
	visible: (string, valuetype) Hashtbl.t;
}

(* memory specific to classes and methods in the JVM *)
type jvm = {
	(* public class present ? *)
	mutable public_class_present: bool;
	(* save the public class *)
	mutable public_class: string;
	(* method names and ast type given *)
	methods: (string, astmethod) Hashtbl.t;
	(* class names *)
	classes: (string, javaclass) Hashtbl.t;
	(* defaults *)
	defaults: (primitive, valuetype) Hashtbl.t;
	(* the stack *)
	jvmstack: ( (string * scope) ) Stack.t;
	(* the heap *)
	jvmheap: (string, string) Hashtbl.t;
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
	| RefVal(nw) -> "Class "^nw.oclass.id
	| NullVal -> "Null"

(* ------------------------------ PRINTS ------------------------------------ *)
let print_scope jvm = 
	print_endline "# Printing scope #";
	match (Stack.top jvm.jvmstack) with 
	| (s, v) -> print_endline s; 
				Hashtbl.iter (fun key value -> print_string (key ^"="); 
										print_endline (string_of_value value)) v.visible

let print_jvm jvm = 
	(* Hashtbl.iter (fun key value -> print_endline key; print_endline value.mname) jmc.methods; *)
	Hashtbl.iter (fun key value -> print_string ("class: "^key); 
									print_endline (" value: "^value.id)) jvm.classes;

	Hashtbl.iter (fun key value -> print_string ("method: "^key); 
									print_endline (" value: "^value.mname)) jvm.methods;

	print_endline ("Public class present: " ^ (string_of_bool jvm.public_class_present));

	print_endline ("The public class is: " ^ jvm.public_class)

let print_jclass jclass =
	print_endline ("### Class " ^ jclass.id ^ " ###");
	(* print all attributes *)
	print_endline ("# Attributes #");
	List.iter (fun t -> AST.print_attribute "" t) jclass.jattributes; 
	(* print the class constructors *)
	Hashtbl.iter (fun key value -> print_string ("constructor with signature: " ^ key);
									print_endline (" | constructor name: " ^value.cname)) jclass.jconsts;
	(* print the class methods and attributes *)
	Hashtbl.iter (fun key value -> print_string ("method: " ^ key);
									print_endline (" | method in jvm table: " ^value)) jclass.jcmethods