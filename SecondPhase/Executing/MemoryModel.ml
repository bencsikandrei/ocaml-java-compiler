(* all memory related issues : heap, stack .. *)
open AST
open Hashtbl

(* memory specific to classes and methods in the JVM *)
type jvm_methods_and_classes = {
	(* method names and ast type given *)
	methods : (string, astmethod) Hashtbl.t;
	classes : (string, asttype) Hashtbl.t
}

let print_jvm_class jmc = 
	(* Hashtbl.iter (fun key value -> print_endline key; print_endline value.mname) jmc.methods; *)
	Hashtbl.iter (fun key value -> print_string ("key: "^key); 
									print_endline (" value: "^value.id)) jmc.classes;

	Hashtbl.iter (fun key value -> print_string ("key: "^key); 
									print_endline (" value: "^value.mname)) jmc.methods;
