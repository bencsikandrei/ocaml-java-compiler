
exception Recursive_inheritance of string
exception Invalid_inheritance of string

let rec inlist elem arr = 
	match arr with
	| [] -> false
	| head::tail -> if head=elem then true else inlist elem tail



let rec flatlist lis = 
	match lis with 
	| [] -> ""
	| elem::rest -> elem^(flatlist rest)



let rec searchClass name (scope:AST.asttype list) =
	match scope with 
	| [] -> raise (Invalid_inheritance ("Class: "^name^" not found"))
	| elem::rest ->
		if elem.id=name then elem
		else searchClass name rest



let checkModifs (mods:AST.modifier list)=
	print_endline("implment checkModifs")(* TODO *)




let rec verifyClassDependency (var:AST.asttype) (classesScope:AST.asttype list) chain=
	if inlist var.id chain then
		raise (Recursive_inheritance ("Class: "^var.id^" inherits from itself"))
	else
		match var.info with
		| Class cl -> 
			if cl.cparent.tid="Object" then ()
			else verifyClassDependency (searchClass cl.cparent.tid classesScope) classesScope (var.id::chain)
		| Inter -> ()



let rec verifyClasses classes classesScope=
	match classes with
	| [] -> ()
	| elem::rest -> verifyClassDependency elem classesScope []; verifyClasses rest classesScope



let rec colectClassInfo (classes:AST.asttype list) = 
	match classes with
	| [] -> ([],[])
	| elem::rest ->  (
			checkModifs elem.modifiers;
			let c,i = colectClassInfo rest in 
			match elem.info with
			|Class cl ->  (elem::c,i)
			|Inter ->  (c,elem::i)
		)



let typing (var:AST.t) =
	let classesScope,interfaceScope = colectClassInfo var.type_list in
	verifyClasses classesScope classesScope
	
	