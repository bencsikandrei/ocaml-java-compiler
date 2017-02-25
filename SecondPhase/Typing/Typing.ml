
exception Recursive_inheritance of string
exception Invalid_inheritance of string
exception DuplicatedModifier of string
exception InvalidAccessModifiers of string
exception InvalidModifier of string
exception DuplicatedArgumentName of string
exception DuplicatedClassName of string
exception InvalidMethodBody of string
exception InvalidClassDefinition of string
exception DuplicatedMethod of string
exception InvalidMethodReDefinition of string


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

let both l a b =
	let a = inlist a l in
	let b = inlist b l in
	(a && b)

(* Extract the classes from the asstype *)
let rec getClasses (classes:AST.asttype list) : AST.astclass list =
	match classes with
	| [] -> []
	| elem::rest ->  (
			let c = getClasses rest in 
			match elem.info with
			|Class cl ->  cl.clname<-elem.id; cl.clmodifiers<-elem.modifiers; cl::c
			|Inter ->  c
		)

let cmptypes (t1:Type.t) (t2:Type.t) =
 	(MemoryModel.TypeVal(t1)=MemoryModel.TypeVal(t2))
 


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


let rec isSubClassOf (scope:AST.astclass list) (son:Type.t) (father:Type.t) =
	if (MemoryModel.TypeVal(son)=MemoryModel.TypeVal(father)) then true
	else
		match son with 
		| Ref r ->
			if (r = Type.object_type) then false
			else 
				let aclass = searchClass r scope in
				isSubClassOf aclass.classScope (Ref aclass.cparent) father 
		| _ -> false



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

let checkDuplicateMethod (methodslist:AST.astmethod list) (amethod:AST.astmethod) =
	List.iter (
		fun (m:AST.astmethod) -> if m.mname=amethod.mname then
		(
			if (List.length amethod.margstype)=(List.length m.margstype) then
			(
				print_endline (m.mname^" - "^amethod.mname);
				let cmplist = List.map2 (fun (a1:AST.argument) (a2:AST.argument) -> cmptypes a1.ptype a2.ptype;) amethod.margstype m.margstype in
				if (List.for_all (fun x -> x) cmplist) then raise (DuplicatedMethod ("Method "^m.mname^" is duplicated."))
			)
		);
	    ) methodslist

let rec verifyNoMethodDuplicates (methods:AST.astmethod list) =
	match methods with
	| [] -> ()
	| hd::tl -> checkDuplicateMethod tl hd; verifyNoMethodDuplicates tl

let rec checkNoClassDuplicates (name_class:string list) = 
	match name_class with
	| [] -> ()
	| hd::tl -> if (inlist hd tl) then raise (DuplicatedClassName ("Class name "^hd^" duplicated.")); 
				checkNoClassDuplicates tl;
				()

let rec verifyNoClassDuplicates (classes:AST.asttype list) = 
	let name_class = List.map (fun (x:AST.asttype) -> x.id;) classes in checkNoClassDuplicates name_class;
	List.map (fun (c:AST.asttype) -> 
				match c.info with
				 | AST.Class cl -> verifyNoClassDuplicates (cl.ctypes)
				 | _ -> ()
			) classes;
	()

let rec verifyClassModifiers (aclass:AST.astclass) = 
	checkModifs(aclass.clmodifiers);
	if not (List.for_all (fun m -> inlist m [AST.Public;AST.Private;AST.Protected;AST.Abstract;AST.Static;AST.Final;AST.Strictfp];) aclass.clmodifiers) 
		then raise (InvalidModifier ("Invalid class modifier for class "^aclass.clname^"."));
	if (both aclass.clmodifiers AST.Abstract AST.Final) then raise (InvalidModifier ("Both modifiers abstract and final can't be present at the same time."));
	List.map verifyClassModifiers (getClasses aclass.ctypes);
	() (*leave this unit to prevent recursive map problems*)

let verifyMemberClassModifiers (aclass:AST.astclass) = 
	print_endline "TODO  Implement verifyMemberClassModifiers (inner/outter mod rules)"


let rec checkNoAttributesDuplicates (name_att:string list) = 
	match name_att with
	| [] -> ()
	| hd::tl -> if (inlist hd tl) then raise (DuplicatedArgumentName ("Argument name "^hd^" duplicated.")); 
				checkNoAttributesDuplicates tl

let verifyNoAttributesDuplicated (args:AST.astattribute list) = 
	let name_att = List.map (fun (x:AST.astattribute) -> x.aname;) args in checkNoAttributesDuplicates name_att

let verifyAttributeCoherence (args:AST.astattribute) = 
	print_endline "TODO  Implement verifyAttributeCoherence - asignacion (adefault)"

let verifyAttributeModifiers (att:AST.astattribute) = 
	checkModifs(att.amodifiers);
	if not (List.for_all (fun m -> inlist m [AST.Public;AST.Private;AST.Protected;AST.Static;AST.Final;AST.Transient;AST.Volatile];) att.amodifiers) 
		then raise (InvalidModifier ("Invalid attribute modifier."))

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
	checkModifs(mods);
	if not (List.for_all (fun m -> inlist m [AST.Public;AST.Private;AST.Protected;AST.Abstract;AST.Static;AST.Final;AST.Synchronized;AST.Native;AST.Strictfp];) mods)
		then raise (InvalidModifier ("Invalid class modifier for method."));
	if (both mods AST.Native AST.Strictfp) then raise (InvalidModifier ("Both modifiers native and strictfp can't be present at the same time."));
	if (both mods AST.Private AST.Abstract) then raise (InvalidModifier ("Both modifiers abstract and private can't be present at the same time."));
	if (both mods AST.Abstract AST.Static) then raise (InvalidModifier ("Both modifiers abstract and static can't be present at the same time."));
	if (both mods AST.Abstract AST.Final) then raise (InvalidModifier ("Both modifiers abstract and final can't be present at the same time."))
	
let rec checkNoArgDuplicates (name_args:string list) = 
	match name_args with
	| [] -> ()
	| hd::tl -> if (inlist hd tl) then raise (DuplicatedArgumentName ("Argument name "^hd^" duplicated.")); 
				checkNoArgDuplicates tl

let verifyMethodDuplicatedArguments (args:AST.argument list) = 
	let name_args = List.map (fun (x:AST.argument) -> x.pident;) args in checkNoArgDuplicates name_args
	
let verifyMethodBody (aclass:AST.astclass) (themethod:AST.astmethod) =
	if ( (themethod.msemi) && (not( (inlist AST.Abstract themethod.mmodifiers)||(inlist AST.Native themethod.mmodifiers) )) ) 
		then raise (InvalidMethodBody ("Only abstract or native methods can't define a body."));
	if ( ( (inlist AST.Abstract themethod.mmodifiers)||(inlist AST.Native themethod.mmodifiers) ) && (not themethod.msemi) ) 
		then raise (InvalidMethodBody ("Abstract or native methods can't define a body."));
	if ( (inlist AST.Abstract themethod.mmodifiers) && not (inlist AST.Abstract aclass.clmodifiers) ) then raise (InvalidModifier ("Method: "^themethod.mname^". Can't a define an abstract method in a non-abstract class."));
	print_endline "TODO  Implement verifyMethodBody"


let verifyClassMethod (aclass:AST.astclass) (amethod:AST.astmethod) = 
	verifyMethodModfier amethod.mmodifiers;
	verifyMethodDuplicatedArguments amethod.margstype;
	verifyMethodBody aclass amethod


let rec verifyClassMethods (aclass:AST.astclass) = 
	verifyNoMethodDuplicates aclass.cmethods;
	List.map (verifyClassMethod aclass) aclass.cmethods;
	List.map verifyClassMethods (getClasses aclass.ctypes);
	()

let checkImplementedMethod (parentsmethods:AST.astmethod list) (classmethod:AST.astmethod) =
	try
		checkDuplicateMethod parentsmethods classmethod;
		true
	with
	| DuplicatedMethod e -> false

let addAbstractMethods (parentsmethods:AST.astmethod list) (classmethods:AST.astmethod list) =
	List.append parentsmethods (List.filter (fun (m:AST.astmethod) -> inlist AST.Abstract m.mmodifiers;) classmethods)

(* returns a list of non implemented inherited abstract methods *)
let rec checkAbstractInheritedMethods (aclass:AST.astclass) =
	if aclass.clid="Object" then [] 
	else (
		let res = checkAbstractInheritedMethods (searchClass aclass.cparent aclass.classScope) in
		let nonimplem = List.filter (checkImplementedMethod aclass.cmethods) res  in
		addAbstractMethods nonimplem aclass.cmethods
	)

let rec verifyInheritedAbstract (aclass:AST.astclass) =
	if not (inlist AST.Abstract aclass.clmodifiers) then
		if List.length (checkAbstractInheritedMethods aclass) > 0 then raise (InvalidClassDefinition ("Class "^aclass.clname^" must be abstract or implement inherited abstract methods."));
		();
	List.map verifyInheritedAbstract (getClasses aclass.ctypes);
	()

let redefined (scope:AST.astclass list) (classMethods:AST.astmethod list) (fatherMethod:AST.astmethod) = 
	let res =List.filter (
		fun (cm:AST.astmethod) -> 
			if cm.mname=fatherMethod.mname then (
				if (List.length fatherMethod.margstype)=(List.length cm.margstype) then (
					let cmplist = List.map2 (fun (a1:AST.argument) (a2:AST.argument) -> cmptypes a1.ptype a2.ptype;) fatherMethod.margstype cm.margstype in
					if (List.for_all (fun x -> x) cmplist) then
					(
						let s1 = inlist AST.Static cm.mmodifiers in
						let s2 = inlist AST.Static fatherMethod.mmodifiers in (
							if ( (s1 && (not s2)) || (s2 && (not s1))  ) 
								then raise (InvalidMethodReDefinition ("method "^cm.mname^" must have the same staticity in as defined by its father."));
							if ( not (isSubClassOf scope cm.mreturntype fatherMethod.mreturntype) ) 
								then raise (InvalidMethodReDefinition ("method "^cm.mname^" must have same return type as defined by its father."))
							else true
						)
					) else false
				)else false
			)else false
	) classMethods in
	(List.length res) > 0

(* returns a list of methods from the fathers *)
let rec checkRedefineInheritedMethods (aclass:AST.astclass) :AST.astmethod list =
	if aclass.clid="Object" then aclass.cmethods
	else (
		let res = checkRedefineInheritedMethods (searchClass aclass.cparent aclass.classScope) in
		let noRedefined = List.filter (fun x -> (not (redefined aclass.classScope aclass.cmethods x));) res in
		noRedefined@aclass.cmethods
	)

let rec verifyMethodRedefinition (aclass:AST.astclass) =
	checkRedefineInheritedMethods aclass;
	List.map verifyMethodRedefinition (getClasses aclass.ctypes);
	()


(* Calls *)
let verifyClasses (var:AST.t) (classes:AST.astclass list)  =
	verifyNoClassDuplicates var.type_list;
	List.map verifyClassModifiers classes;
	List.map verifyMemberClassModifiers classes;
	List.map verifyClassDependencyInit classes;
	List.map verifyClassAttributes classes;
	List.map verifyClassMethods classes;
	List.map verifyInheritedAbstract classes;
	List.map verifyMethodRedefinition classes;
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
	    	AST.classScope=Object.objectInfo::var;
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
    		AST.classScope=Object.objectInfo::next;
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
	
	
	