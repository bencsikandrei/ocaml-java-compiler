
exception Recursive_inheritance of string
exception Invalid_inheritance of string
exception DuplicatedModifier of string
exception InvalidAccessModifiers of string


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


let rec flatlistDot lis = 
	match lis with 
	| [] -> ""
	| elem::[] -> elem
	| elem::rest -> elem^"."^(flatlistDot rest)




(* Extract the classes from the asstype *)
let rec getClasses (classes:AST.asttype list) : AST.astclass list =
	match classes with
	| [] -> []
	| elem::rest ->  (
			let c = getClasses rest in 
			match elem.info with
			|Class cl ->  cl.clname<-elem.id;  cl.clmodifiers<-elem.modifiers; cl::c
			|Inter ->  c
		)




(* ***********************
* Class Checking functions
* ************************ *)

let rec checkNoDuplicates (mods:AST.modifier list) =
	match mods with
	| [] -> ()
	| hd::tl -> if (inlist hd tl) then raise (DuplicatedModifier ("Modifier "^(AST.stringOf_modifier hd)^" duplicated.")); 
				checkNoDuplicates tl

let rec checkOneAccessModif (mods:AST.modifier list) =
	let res = List.filter (fun x -> (x=AST.Public || x=AST.Protected || x=AST.Private);) mods in
	if List.length res > 1 then raise (InvalidAccessModifiers ("Can't have "^(flatlistDot (List.map AST.stringOf_modifier res))^" at the same time."))

(* Checkes if a class/method/variable has valid modifiers *)
let checkModifs (mods:AST.modifier list) =
	checkNoDuplicates(mods);
	checkOneAccessModif(mods)


let verifyClassInterior (var:AST.astclass) (classesScope:AST.astclass list) chain =
	[]


(* Checkes in a list if a class with clid "clname" exists *)
let rec searchClass (clname:Type.ref_type) (scope:AST.astclass list) : AST.astclass=
	(*print_string ((flatlistDot clname.tpath)^"."^clname.tid^" -> ");*)
	match scope with 
	| [] -> raise (Invalid_inheritance ("Class: "^(flatlistDot clname.tpath)^"."^clname.tid^" not found"))
	| elem::rest -> (
			match clname.tpath with 
			| [] -> 
				(* print_string (elem.clname^"=??"^clname.tid^"\n"); *)
				if elem.clname=clname.tid then elem
				else searchClass clname rest
			| first::others -> 
				(*print_string (elem.clname^"=?"^first^"\n");*)
				if elem.clname=first then 
					searchClass {tpath=others; tid=clname.tid} elem.classScope
				else searchClass clname rest
		)


(* Verifies that the inheritance of a class is valclid *)
let rec verifyClassDependency (chain:string list) (cl:AST.astclass) =
	if inlist cl.clid chain then
		raise (Recursive_inheritance ("Class: "^cl.clid^" inherits from itself"))
	else
			if ( List.length cl.cparent.tpath == 0 && cl.cparent.tid="Object" ) then ()
			else 
				let par = searchClass cl.cparent cl.classScope in
				verifyClassDependency (cl.clid::chain) par 

(* Verifies that the inheritance of a class is valclid *)
let rec verifyClassDependencyInit (cl:AST.astclass) =
	verifyClassDependency [] cl;
	List.map verifyClassDependencyInit (getClasses cl.ctypes);
	() 

let f (var:AST.astclass) = 
		print_string (var.clname^" - ")

let rec fillScopes (clid:string) (classes:AST.astclass list) (scope:AST.astclass list)  = 
	let f = addScope clid scope in
	List.map f classes

and addScope (clid:string) (scope:AST.astclass list) (aclass:AST.astclass) =
	if(aclass.clid="") then 
		let cs = getClasses aclass.ctypes in
		aclass.clid<-clid^"."^aclass.clname;
		aclass.classScope<-(cs@scope);
		fillScopes aclass.clid cs (cs@scope);
		(*print_string (aclass.clid^" -> ");
		List.map f aclass.classScope;
		print_string "\n";*)
		aclass
	else aclass



let verifyNoMethodDuplicates (methods:AST.astmethod list) = 
	print_endline "TODO  Implement verifyNoMethodDuplicates"



let rec verifyNoClassDuplicates (amethod:AST.asttype list) = 
	print_endline "TODO  Implement verifyNoClassDuplicates"


let rec verifyClassModifiers (aclass:AST.astclass) = 
	checkModifs(aclass.clmodifiers);
	() (*leave this unit to prevent recursive map problems*)


let verifyNoAttributesDuplicated (args:AST.astattribute list) = 
	print_endline "TODO  Implement verifyNoAttributesDuplicated"

let verifyAttributeCoherence (args:AST.astattribute) = 
	print_endline "TODO  Implement verifyAttributeCoherence"

let verifyAttributeModifiers (args:AST.astattribute) = 
	print_endline "TODO  Implement verifyAttributeModifiers"

let rec verifyClassAttributes (aclass:AST.astclass) = 
	verifyNoAttributesDuplicated aclass.cattributes;
	List.map verifyAttributeModifiers aclass.cattributes;
	List.map verifyAttributeCoherence aclass.cattributes;
	List.map verifyClassAttributes (getClasses aclass.ctypes);
	()

let rec verifyClassConstructors (aclass:AST.astclass) = 
	print_endline "TODO  Implement verifyClassConstructors";
	() (*leave this unit to prevent recursive map problems*)

let rec verifyClassInitials (aclass:AST.astclass) = 
	print_endline "TODO  Implement verifyClassInitials";
	() (*leave this unit to prevent recursive map problems*)

let verifyMethodModfier (mods:AST.modifier list) = 
	print_endline "TODO  Implement verifyMethodModfier"

let verifyMethodDuplicatedArguments (args:AST.argument list) = 
	print_endline "TODO  Implement verifyMethodDuplicatedArguments"

let verifyMethodBody (aclass:AST.astclass) (body:AST.statement list) = 
	print_endline "TODO  Implement verifyMethodBody"


let verifyClassMethod (aclass:AST.astclass) (amethod:AST.astmethod) = 
	verifyMethodModfier amethod.mmodifiers;
	verifyMethodDuplicatedArguments amethod.margstype;
	verifyMethodBody aclass amethod.mbody


let rec verifyClassMethods (aclass:AST.astclass) = 
	verifyNoMethodDuplicates aclass.cmethods;
	List.map (verifyClassMethod aclass) aclass.cmethods;
	List.map verifyClassMethods (getClasses aclass.ctypes);
	()

(* Calls *)
let verifyClasses (var:AST.t) (classes:AST.astclass list)  =
	verifyNoClassDuplicates var.type_list;
	List.map verifyClassModifiers classes;
	List.map verifyClassDependencyInit classes;
	List.map verifyClassAttributes classes;
	List.map verifyClassMethods classes;
	List.map verifyClassConstructors classes;
	List.map verifyClassInitials classes



(* ***********************
* Fill the missing information in the ast
* ************************ *)

let getPackageInfo (var:AST.t) = 
	match var.package with 
		| None -> ""
		| Some x -> flatlistDot x


let rec createPckg (x:AST.qualified_name) (var:AST.astclass list) (id:string)=
	match x with 
	| [] -> []
	| last::[] -> 	[{
			AST.clid=id^last ;
	    	AST.clname=last ;
	    	AST.classScope=var;
	    	AST.clmodifiers=[];
	    	AST.cparent = {tpath=[];tid="Object"} ;
	    	AST.cattributes = [];
	    	AST.cinits = [];
	    	AST.cconsts = [];
	    	AST.cmethods = [];
	    	AST.ctypes = [];
	    	AST.cloc = Location.none;
		}]
	| head::tail ->
		let next = createPckg tail var (id^head^".") in
		[{
			AST.clid=id^head ;
    		AST.clname=head ;
    		AST.classScope=next;
    		AST.clmodifiers=[];
	    	AST.cparent = {tpath=[];tid="Object"} ;
	    	AST.cattributes = [];
	    	AST.cinits = [];
	    	AST.cconsts = [];
	    	AST.cmethods = [];
	    	AST.ctypes = [];
	    	AST.cloc = Location.none;
		} ]

let pckgInfo (pckgname:AST.qualified_name option) (var:AST.astclass list) =
	match pckgname with 
	| None -> var
	| Some x -> (createPckg x var "")@var
	

let addBasics (pckgname:AST.qualified_name option) (var:AST.astclass list) :AST.astclass list = 
	let lis = pckgInfo pckgname var in 
    Object.objectInfo::lis
   

(* ***********************
* main function of the module
* ************************ *)

(* Checkes in a list if the ast is valclid *)
let typing (var:AST.t) =
	(*let classesScope,interfaceScope = colectClassInfo var.type_list in*)
	let classes =  addBasics var.package (getClasses var.type_list) in
	fillScopes  (getPackageInfo var) classes classes;
	verifyClasses var classes;
	var
	
	
	