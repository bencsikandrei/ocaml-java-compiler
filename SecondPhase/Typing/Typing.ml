
exception Recursive_inheritance of string
exception Invalid_inheritance of string



(* ***********************
* AUX FUNCTIONS
* ************************ *)
let rec inlist elem arr = 
	match arr with
	| [] -> false
	| head::tail -> if head=elem then true else inlist elem tail



let rec flatlist lis = 
	match lis with 
	| [] -> ""
	| elem::rest -> elem^(flatlist rest)








(* ***********************
* Class Checking functions
* ************************ *)

(* Checkes in a list if a class with id "name" exists *)
let rec searchClass name (scope:AST.astclass list) =
	match scope with 
	| [] -> raise (Invalid_inheritance ("Class: "^name^" not found"))
	| elem::rest ->
		if elem.id=name then elem
		else searchClass name rest



(* Checkes if a class/method/variable has valid modifiers *)
let checkModifs (mods:AST.modifier list)=
	print_endline("implment checkModifs")(* TODO *)


(* Verifies that the inheritance of a class is valid *)
let rec verifyClassDependency (cl:AST.astclass) (classesScope:AST.astclass list) (chain:string list)=
	if inlist cl.id chain then
		raise (Recursive_inheritance ("Class: "^cl.id^" inherits from itself"))
	else
			if cl.cparent.tid="Object" then ()
			else verifyClassDependency (searchClass cl.cparent.tid classesScope) classesScope (cl.id::chain)


(* Calls the given function for all classes *)
let rec verifyClasses fn classes classesScope=
	match classes with
	| [] -> ()
	| elem::rest -> fn elem classesScope []; verifyClasses fn rest classesScope


(* Sorts te asttype list and returs a list of classes and a list of interfaces *)
let rec colectClassInfo (classes:AST.asttype list) = 
	match classes with
	| [] -> ([],[])
	| elem::rest ->  (
			checkModifs elem.modifiers;
			let c,i = colectClassInfo rest in 
			match elem.info with
			|Class cl ->  cl.id<-elem.id; (cl::c,i)
			|Inter ->  (c,elem::i)
		)

let verifyClassInterior (var:AST.astclass) (classesScope:AST.astclass list) chain =
	[]



(* ***********************
* main function of the module
* ************************ *)

(* Checkes in a list if the ast is valid *)
let typing (var:AST.t) =
	let classesScope,interfaceScope = colectClassInfo var.type_list in
	verifyClasses verifyClassDependency classesScope classesScope;
	verifyClasses verifyClassInterior classesScope classesScope
	
	
	