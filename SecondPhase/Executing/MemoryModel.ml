(* all memory related issues : heap, stack .. *)
open AST
open Hashtbl

type javaclass = {
	(* a type to hold class methods, attributes, constructors,
	name, paret class, etc
	resembles the AST.asttype - but uses Hashtbl for storing *)
	id: string;
	cparent : Type.ref_type;
    cattributes : astattribute list;
    cinits : initial list;
    cconsts : astconst list;
    cmethods : (string, string) Hashtbl.t
}

(* memory specific to classes and methods in the JVM *)
type jvm = {
	(* public class present ? *)
	mutable public_class_present: bool;
	(* method names and ast type given *)
	methods : (string, astmethod) Hashtbl.t;
	(* class names *)
	classes : (string, javaclass) Hashtbl.t;
}


let print_jvm jvm = 
	(* Hashtbl.iter (fun key value -> print_endline key; print_endline value.mname) jmc.methods; *)
	Hashtbl.iter (fun key value -> print_string ("key: "^key); 
									print_endline (" value: "^value.id)) jvm.classes;

	Hashtbl.iter (fun key value -> print_string ("key: "^key); 
									print_endline (" value: "^value.mname)) jvm.methods;

	print_string (string_of_bool jvm.public_class_present)