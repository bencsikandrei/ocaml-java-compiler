
exception Recursive_inheritance of string
exception Invalclid_inheritance of string



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


(* Calls the given function for all classes *)
let verifyClasses fn classes =
	List.map fn classes





(* ***********************
* Class Checking functions
* ************************ *)

(* Checkes if a class/method/variable has valclid modifiers *)
let checkModifs (mods:AST.modifier list)=
	print_endline("implment checkModifs")(* TODO *)


let verifyClassInterior (var:AST.astclass) (classesScope:AST.astclass list) chain =
	[]


(* Checkes in a list if a class with clid "clname" exists *)
let rec searchClass (clname:Type.ref_type) (scope:AST.astclass list) : AST.astclass=
	match scope with 
	| [] -> raise (Invalclid_inheritance ("Class: "^(flatlistDot clname.tpath)^"."^clname.tid^" not found"))
	| elem::rest -> (
			match clname.tpath with 
			| [] -> 
				if elem.clname=clname.tid then elem
				else searchClass clname rest
			| first::others -> 
				if elem.clid=first then 
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

let rec getClasses (classes:AST.asttype list) : AST.astclass list =
	match classes with
	| [] -> []
	| elem::rest ->  (
			let c = getClasses rest in 
			match elem.info with
			|Class cl ->  cl.clname<-elem.id; cl::c
			|Inter ->  c
		)

let rec fillScopes (clid:string) (classes:AST.astclass list) (scope:AST.astclass list)  = 
	let f = addScope clid scope in
	List.map f classes

and addScope (clid:string) (scope:AST.astclass list) (aclass:AST.astclass) =
	if(aclass.clid<>"Object") then 
		let cs = getClasses aclass.ctypes in
		aclass.clid<-clid^"."^aclass.clname;
		aclass.classScope<-cs@scope;
		fillScopes aclass.clid cs scope@cs;
		aclass
	else aclass

(* ***********************
* main function of the module
* ************************ *)

let getPackageInfo (var:AST.t) = 
	match var.package with 
		| None -> ""
		| Some x -> flatlistDot x


let addBasics (pckgname:string) (var:AST.astclass list) = 
	let pckgInfo = {
		AST.clid=pckgname ;
    	AST.clname=pckgname ;
    	AST.classScope=[];
    	AST.clmodifiers=[];
    	AST.cparent = {tpath=[];tid="Object"} ;
    	AST.cattributes = [];
    	AST.cinits = [];
    	AST.cconsts = [];
    	AST.cmethods = [];
    	AST.ctypes = [];
    	AST.cloc = Location.none;
	} in 
	let objectInfo = {
		AST.clid="Object";
    	AST.clname="Object";
    	AST.classScope=[];
    	AST.clmodifiers=[];
    	AST.cparent = {tpath=[];tid="Object"} ;
    	AST.cattributes = [];
    	AST.cinits = [];
    	AST.cconsts = [];
    	AST.cmethods = [];
    	AST.ctypes = [];
    	AST.cloc = Location.none;
    } in objectInfo.classScope<-[objectInfo];
    pckgInfo::objectInfo::var
   


(* Checkes in a list if the ast is valclid *)
let typing (var:AST.t) =
	(*let classesScope,interfaceScope = colectClassInfo var.type_list in*)
	let classes =  addBasics (getPackageInfo var) (getClasses var.type_list) in
	fillScopes  (getPackageInfo var) classes classes;
	verifyClasses (verifyClassDependency [] ) classes;
	var
	
	
	