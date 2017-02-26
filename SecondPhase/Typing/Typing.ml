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
exception NonImplemented of string
exception InvalidExpression of string
exception InvalidConstructor of string
exception InvalidStatement of string


(* ***********************
* AUX FUNCTIONS
* ************************ *)

let rec splitInner (str:string) (c:char) (strSize:int) (currentPos:int) (current:string) : string list=
	if currentPos = strSize then
		if current="" then
			[]
		else
			current::[]
	else
		if str.[currentPos] = c  then
			current::(splitInner str c strSize (currentPos+1) "")
		else 
			splitInner str c strSize (currentPos+1) (current^(String.make 1 str.[currentPos]))

let split (str:string) (c:char):string list =
	splitInner str c (String.length str) 0 ""


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

let rec getLast (l:string list) : (string list)*string=
	match l with
	| [] -> ([],"")
	| one::[] -> ([],one)
	| head::tail -> let (h,t) = getLast tail in (head::h,t)

let cmptypes (t1:Type.t) (t2:Type.t) =
 	(MemoryModel.TypeVal(t1)=MemoryModel.TypeVal(t2))
 

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


let getPath (path:string):string list=
	let (first,last)=getLast (split path '.' )in 
	first

let rec getParents (aclass:AST.astclass) : Type.ref_type list=
	if aclass.clid="Object" then [Type.object_type]
	else 
		let father = searchClass aclass.cparent aclass.classScope in
		{Type.tpath=(getPath aclass.clid) ; Type.tid=aclass.clname}::(getParents father)

let rec getFirstRepetiton  (elem:Type.ref_type) (l2:Type.ref_type list) : bool*Type.ref_type*(Type.ref_type list) =
	match l2 with
	| [] -> (false,elem,l2)
	| head::tail -> 
		if cmptypes (Type.Ref elem) (Type.Ref head) then
			(true,elem,tail)
		else
			getFirstRepetiton elem tail

let rec getFirstComonRepetition (l1:Type.ref_type list) (l2:Type.ref_type list) :  Type.ref_type*(Type.ref_type list)=
	match l1 with
	| [] ->   raise (InvalidExpression("*****classes do not share inheritances!!!-Should not happen."))
	| head::tail ->
		let (found,elem,rest) = getFirstRepetiton head l2 in
		if (found) then
			(elem,rest)
		else 
			getFirstComonRepetition tail l2


let rec inferType  (scope:AST.astclass list)  (types:Type.t list) : Type.t*(Type.ref_type list) =
	List.map (fun x -> print_string (Type.stringOf x)) types;
	print_string "\n";
	match types with 
	| [] -> raise (NonImplemented "empty arrayInit")
	| last::[] -> ( 
		match last with 
		| Ref f -> (last,(getParents (searchClass f scope)))
		| _ -> (last,[])
	)
	| head::tail -> 
		let t,parents=inferType scope tail in
		if cmptypes head t then
			(t,parents)
		else (
			match t with
			| Array (tailType,tailDims) ->(
					match head with
					| Array (headType,headDims) ->
						if (tailDims <> headDims) then
							raise (InvalidExpression("Dimension mismatch in array initialization."))
						else (				
							match tailType with
							| Ref tailObjectType -> (
								match headType with
								| Ref headObjectType ->(
									let l1 = getParents (searchClass tailObjectType scope) in
									let l2 = getParents (searchClass headObjectType scope) in
									let (res1,res2) = getFirstComonRepetition l1 l2 in
									(Type.Array(Type.Ref res1,headDims),[])
								)
								| _ -> raise (InvalidExpression("Type mismatch in array initialization."))
							)
							| _ -> raise (InvalidExpression("Type mismatch in array initialization."))
						)
					| _ ->  raise (InvalidExpression("Type mismatch in array initialization."))
			)
			| Ref ft -> (
				match head with
				| Ref fhead -> (
					let l1 = getParents (searchClass fhead scope) in
					let (res1,res2) = getFirstComonRepetition l1 parents in
					(Type.Ref res1,res2)
				)
				| _ ->  raise (InvalidExpression("Type mismatch in array initialization."))
			)
			| _ ->  raise (InvalidExpression("Type mismatch in array initialization."))

		)


(* ***********************
* Class Checking functions
* ************************ *)

let rec solveExpression (aclass:AST.astclass) (args:AST.argument list) (locals:AST.statement list)  (exp:AST.expression)  :Type.t = 

	match exp.etype with
	| Some x -> x
	| None -> (
		match exp.edesc with 
			| AST.New (strOpt, strList, expList) ->  (*TODO find what this strOpt is for*)
				(
					let (hd,tl) =  getLast strList in
					exp.etype <- Some (Type.Ref ({ Type.tpath = hd  ; Type.tid = tl }));
					checkContrstuctor aclass { Type.tpath = hd  ; Type.tid = tl } (List.map (solveExpression aclass args locals) expList);
					Type.Ref { Type.tpath = hd  ; Type.tid = tl }
				)
			| AST.NewArray (t, expOptList, expOpt) -> 
				(
					let dims, dimsDeclared = checkArrayDimensions aclass args locals expOptList in
					(
						exp.etype <- Some(Type.Array (t,dims));
						match expOpt with
						| None -> ()
						| Some x ->( 
							match x.edesc with
							| AST.ArrayInit l-> 
								if(dimsDeclared>0) then 
									raise (InvalidExpression("Cannot define array dimension expression when initializer is provided."))
								else
									let res = solveExpression aclass args locals x in 
									(
										match res with
										| Type.Array (at,d) ->
											( 
												if (d<>dims) then 
													raise (InvalidExpression("Array size definition does not match the initialization."));
												if not ( isSubClassOf aclass.classScope at t) then 
														raise  (InvalidExpression("Array type definition does not match the initialization"));
											)  
										| _ -> raise (InvalidExpression("*************invalid new array-should not happen2."))
									)
							| _ -> raise (InvalidExpression("*************invalid new array-should not happen1."))
						)
					);
					Type.Array (t,dims)
				)
			| AST.Call (expOpt, str, expList) -> print_endline "TODO  Implement Call"; Type.Void
			| AST.Attr (exp ,str) -> (
				print_endline "TODO  Implement Attr"; Type.Void
			)
			| AST.If (exp1, exp2, exp3) -> print_endline "TODO  Implement If"; Type.Void
			| AST.Val v -> (
				(match v with
					| String s -> exp.etype <- Some (Type.Ref { tpath = [] ; tid = "String" })
					| Int i -> exp.etype <- Some (Type.Primitive Int)
					| Float f -> exp.etype <- Some (Type.Primitive Float)
					| Char c -> exp.etype <- Some (Type.Primitive Char)
					| Null -> exp.etype <- Some ( Type.Ref { tpath = [] ; tid = "Null" })
					| Boolean b -> exp.etype <- Some (Type.Primitive Boolean)
				);
				match exp.etype with
					| Some x -> x
					| None -> raise (InvalidExpression("*************invalid new empty type-should not happen3."))
			)
			| AST.Name n ->  raise (NonImplemented "Name??") 
			| AST.ArrayInit expList -> (
				let (t,parents) = inferType aclass.classScope (List.map (solveExpression aclass args locals) expList) in 
				(match t with 
				| Type.Array (at,dims) -> exp.etype <- Some (Type.Array (at,dims+1)); 
				| _ -> exp.etype <- Some (Type.Array (t,1)) 
				);
				match exp.etype with
					| Some x -> x
					| None -> raise (InvalidExpression("*************invalid new empty type-should not happen4."))
			)
			| AST.Array (exp ,expOptList) -> print_endline "TODO  Implement Array"; Type.Void
			| AST.AssignExp (exp1 ,a_op, exp2) -> (
				let t1=solveExpression aclass args locals exp1 in
				let t2=solveExpression aclass args locals exp2 in
				if (isSubClassOf aclass.classScope t2 t1) then
					t1
				else 
					match a_op with
					| Ass_add -> 
						if isSubClassOf aclass.classScope t1 (Type.Ref {tpath=[];tid="String"})then
							match t2 with
							| Type.Primitive p -> t1
							| _ -> raise (InvalidExpression("assign type mismatch"^(Type.stringOf t1)^" != "^(Type.stringOf t2)))
						else
							raise (InvalidExpression("assign type mismatch"^(Type.stringOf t1)^" != "^(Type.stringOf t2)))
					| _ -> raise (InvalidExpression("assign type mismatch"^(Type.stringOf t1)^" != "^(Type.stringOf t2)))
			)
			| AST.Post (exp, postfix_op)  -> print_endline "TODO  Implement Post"; Type.Void
			| AST.Pre (prefix_op ,exp) -> print_endline "TODO  Implement Pre"; Type.Void
			| AST.Op (exp, i_op , exp2) -> print_endline "TODO  Implement Op"; Type.Void
			| AST.CondOp (exp1 , exp2, exp3) -> print_endline "TODO  Implement CondOp"; Type.Void
			| AST.Cast (t, exp) -> print_endline "TODO  Implement Cast"; Type.Void
			| AST.Type t -> print_endline "TODO  Implement Type"; Type.Void
			| AST.ClassOf t -> print_endline "TODO  Implement ClassOf"; Type.Void
			| AST.Instanceof (exp, t) -> print_endline "TODO  Implement Instanceof"; Type.Void
			| AST.VoidClass -> Location.print exp.eloc ; raise (NonImplemented "VoidClass??")(*TODO ???*)
	)


and checkContrstuctor (aclass:AST.astclass) (constuctClass:Type.ref_type) (args:Type.t list)=
	let classtoinst = searchClass constuctClass aclass.classScope in
	if (List.length args = 0) && (List.length classtoinst.cconsts = 0) then () else
	(
		let matchingconst = List.map (
			fun (c:AST.astconst) -> if (List.length c.cargstype)=(List.length args) then
				(
					let cmplist = List.map2 (fun (a1:AST.argument) (a2:Type.t) -> isSubClassOf classtoinst.classScope a1.ptype a2;) c.cargstype args in
					if (List.for_all (fun x -> x) cmplist) then Some c
					else None				
				) else None;
		) classtoinst.cconsts in
		List.map (fun c -> match c with | None -> print_endline "none" | Some c -> (AST.print_const "-" c)) matchingconst; 
		if List.for_all (fun (c:AST.astconst option) -> match c with
						| None -> true
						| Some c -> (
							if (inlist AST.Private c.cmodifiers) && c.cname <> aclass.clname then true else false;
							if (inlist AST.Protected c.cmodifiers) && not 
							(isSubClassOf aclass.classScope (Type.Ref({Type.tpath=(getPath aclass.clid) ; Type.tid=aclass.clname})) (Type.Ref({Type.tpath=(getPath classtoinst.clid) ; Type.tid=classtoinst.clname}))) 
								then true else false								
						)
					) matchingconst then raise (InvalidConstructor ("Can't find accessible constructor for "^classtoinst.clname^" with those arguments."))
	)


and checkArrayDimensions (aclass:AST.astclass) (args:AST.argument list) (locals:AST.statement list) (l: (AST.expression option) list):int*int =
	let res = List.map (
		fun (x:AST.expression option)-> 
		match x with 
			| None -> false
			| Some y -> 
				let aType = solveExpression aclass args locals y  in
				match aType with 
				|Primitive p -> (
					match p with
					| Int -> true
					| _ ->  raise (InvalidExpression("Array dimensions must be ints."))
				)
				| _ -> raise (InvalidExpression("Array dimensions must be ints."))
			) l in
	((List.length res),(completedInOrder res 0 true))

and completedInOrder (res:bool list) (completed:int) (v:bool) :int=
	match res with
	| [] -> (completed)
	| some::rest -> 
		if v then
			if some then completedInOrder rest (completed+1) true
			else completedInOrder rest completed false
		else 
			if some then raise (InvalidExpression("Array dimensions must be cannot be filled after empty ones."))
			else completedInOrder rest completed false

let rec solveStatements (aclass:AST.astclass) (args:AST.argument list) (rType:Type.t) (treated:AST.statement list) (nonTreated:AST.statement list) =
	match nonTreated with
	| [] -> ()
	| head::tail ->	(
		(
			match head with 
			| AST.VarDecl v ->  checkVarDecl aclass args treated v
			| AST.Block stateList -> solveStatements aclass args rType treated stateList
			| AST.Nop -> ()
			| AST.While (exp, state) -> (
				if not (cmptypes (solveExpression aclass args treated exp) (Type.Primitive Type.Boolean)) then
					 raise (InvalidExpression("While statements cannot be resolved without a boolean expression."))
				else 
					solveStatements aclass args rType treated [state]
			)
			| AST.For (t_optStrExp_optList, expOpt, expList,  state) -> (
				solveStatements aclass args rType treated [state]; print_endline "TODO implement FOR"
			)
			| AST.If (exp, state, stateOpt) ->(
				if not (cmptypes (solveExpression aclass args treated exp) (Type.Primitive Type.Boolean)) then
					 raise (InvalidExpression("While statements cannot be resolved without a boolean expression."))
				else 
					(
						solveStatements aclass args rType treated [state];
						match stateOpt with
						|None -> ()
						|Some state -> solveStatements aclass args rType treated [state]
					)
			)
			| AST.Return expOpt -> (
				match expOpt with
				| None -> 
					if cmptypes rType Type.Void then 
						() 
					else 
						raise (InvalidStatement("Return type must be empty."))
				| Some x -> 
					if isSubClassOf aclass.classScope (solveExpression aclass args treated x) rType  then 
						() 
					else 
						raise (InvalidStatement("Return type must be of "^(Type.stringOf rType)))
			)
			| AST.Throw exp -> print_endline "TODO"
			| AST.Try (stateList1, argState_ListList, stateList2) -> print_endline "TODO"
			| AST.Expr exp -> (solveExpression aclass args treated exp ; ())
		);
		solveStatements aclass args rType (treated@[head]) tail
	)

and checkVarDecl (aclass:AST.astclass) (args:AST.argument list) (treated:AST.statement list) (varDecls:(Type.t * string * AST.expression option) list)=
	match varDecls with 
	| [] -> ()
	| (t,id,exp)::rest -> (
		match exp with
		| None -> ()
		| Some e -> 
			let resType = solveExpression aclass args treated e in
			if ( isSubClassOf aclass.classScope resType t)then
				 checkVarDecl aclass args treated rest
			else
				raise (InvalidStatement("invalid intialization of variable "^id^" -> "^(Type.stringOf t)^" != "^(Type.stringOf resType)))
	)



let rec checkNoDuplicates (mods:AST.modifier list) =
	match mods with
	| [] -> ()
	| hd::tl -> if (inlist hd tl) then raise (DuplicatedModifier ("Modifier "^(AST.stringOf_modifier hd)^" duplicated.")); 
				checkNoDuplicates tl

let rec checkOneAccessModif (mods:AST.modifier list) =
	let res = List.filter (fun x -> (x=AST.Public || x=AST.Protected || x=AST.Private);) mods in
	if List.length res > 1 then raise (InvalidAccessModifiers ("Can't have "^(flatlistDot (List.map AST.stringOf_modifier res))^" at the same time."))

(* Checks if a class/method/variable has valid modifiers *)
let checkModifs (mods:AST.modifier list) =
	checkNoDuplicates(mods);
	checkOneAccessModif(mods)

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

let rec checkNoArgDuplicates (name_args:string list) = 
	match name_args with
	| [] -> ()
	| hd::tl -> if (inlist hd tl) then raise (DuplicatedArgumentName ("Argument name "^hd^" duplicated.")); 
				checkNoArgDuplicates tl

let verifyMethodDuplicatedArguments (args:AST.argument list) = 
	let name_args = List.map (fun (x:AST.argument) -> x.pident;) args in checkNoArgDuplicates name_args

let checkDuplicateMethod (methodslist:AST.astmethod list) (amethod:AST.astmethod) =
	List.iter (
		fun (m:AST.astmethod) -> if m.mname=amethod.mname then
		(
			if (List.length amethod.margstype)=(List.length m.margstype) then
			(
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

let rec verifyMemberClassModifiersInner (allowstatic:bool)  (aclass:AST.astclass) =
	if (not allowstatic) && (inlist AST.Static aclass.clmodifiers) then raise (InvalidModifier ("Static types can only be declared in static or top level types."));
	List.map (verifyMemberClassModifiersInner (inlist AST.Static aclass.clmodifiers)) (getClasses aclass.ctypes);
	()

let verifyMemberClassModifiers (aclass:AST.astclass) = 
	if (inlist AST.Private aclass.clmodifiers) then raise (InvalidModifier ("Private modifier is not allowed in top level classes."));
	if (inlist AST.Protected aclass.clmodifiers) then raise (InvalidModifier ("Protected modifier is not allowed in top level classes."));
	if (inlist AST.Static aclass.clmodifiers) then raise (InvalidModifier ("Static modifier is not allowed in top level classes."));
	List.map (fun (c:AST.asttype) -> 
				match c.info with
				 | AST.Class cl -> verifyMemberClassModifiersInner true cl
				 | _ -> ()
			) aclass.ctypes;
	()


let rec checkNoAttributesDuplicates (name_att:string list) = 
	match name_att with
	| [] -> ()
	| hd::tl -> if (inlist hd tl) then raise (DuplicatedArgumentName ("Argument name "^hd^" duplicated.")); 
				checkNoAttributesDuplicates tl

let verifyNoAttributesDuplicated (args:AST.astattribute list) = 
	let name_att = List.map (fun (x:AST.astattribute) -> x.aname;) args in checkNoAttributesDuplicates name_att

let verifyAttributeCoherence (aclass:AST.astclass) (arg:AST.astattribute) = 
	match arg.adefault with
	| None -> ()
	| Some x -> 
		if isSubClassOf aclass.classScope (solveExpression aclass [] [] x ) arg.atype then ()
		else raise (InvalidStatement("Invalid attribute initalization of type: "^(Type.stringOf arg.atype)^" != "^(Type.stringOf (solveExpression aclass [] [] x ))))

let verifyAttributeModifiers (att:AST.astattribute) = 
	checkModifs(att.amodifiers);
	if not (List.for_all (fun m -> inlist m [AST.Public;AST.Private;AST.Protected;AST.Static;AST.Final;AST.Transient;AST.Volatile];) att.amodifiers) 
		then raise (InvalidModifier ("Invalid attribute modifier."))

let rec verifyClassAttributes (aclass:AST.astclass) = 
	verifyNoAttributesDuplicated aclass.cattributes;
	List.map verifyAttributeModifiers aclass.cattributes;
	List.map (verifyAttributeCoherence aclass ) aclass.cattributes;
	List.map verifyClassAttributes (getClasses aclass.ctypes);
	()

let verifyMethodBody (aclass:AST.astclass) (themethod:AST.astmethod) =
	if ( (themethod.msemi) && (not( (inlist AST.Abstract themethod.mmodifiers)||(inlist AST.Native themethod.mmodifiers) )) ) 
		then raise (InvalidMethodBody ("Only abstract or native methods can't define a body."));
	if ( ( (inlist AST.Abstract themethod.mmodifiers)||(inlist AST.Native themethod.mmodifiers) ) && (not themethod.msemi) ) 
		then raise (InvalidMethodBody ("Abstract or native methods can't define a body."));
	if ( (inlist AST.Abstract themethod.mmodifiers) && not (inlist AST.Abstract aclass.clmodifiers) ) then raise (InvalidModifier ("Method: "^themethod.mname^". Can't a define an abstract method in a non-abstract class."));
	solveStatements aclass themethod.margstype themethod.mreturntype [] themethod.mbody




let verifyConstructorBody (aclass:AST.astclass) (cons:AST.astconst) =
	solveStatements aclass cons.cargstype Type.Void [] cons.cbody


let checkDuplicateConstructor (constrlist:AST.astconst list) (acosntructor:AST.astconst) =
	List.iter (
		fun (c:AST.astconst) -> if c.cname=acosntructor.cname then
		(
			if (List.length acosntructor.cargstype)=(List.length c.cargstype) then
			(
				let cmplist = List.map2 (fun (a1:AST.argument) (a2:AST.argument) -> cmptypes a1.ptype a2.ptype;) acosntructor.cargstype c.cargstype in
				if (List.for_all (fun x -> x) cmplist) then raise (DuplicatedMethod ("Constructor "^c.cname^" is duplicated."))
			)
		);
	    ) constrlist

let rec verifyNoConstructorDuplicates (constrs:AST.astconst list) =
	match constrs with
	| [] -> ()
	| hd::tl -> checkDuplicateConstructor tl hd; verifyNoConstructorDuplicates tl

let rec verifyConstructorModifiers (mods:AST.modifier list) =
	checkModifs(mods);
	if not (List.for_all (fun m -> inlist m [AST.Public;AST.Private;AST.Protected];) mods)
		then raise (InvalidModifier ("Invalid modifier for a constructor."))

let rec verifyClassConstructors (aclass:AST.astclass) =
	List.map (fun (c:AST.astconst) -> if c.cname <> aclass.clname then raise (InvalidConstructor ("The name of the constructor must be the same as the class ("^aclass.clname^")."));) aclass.cconsts;
	verifyNoConstructorDuplicates aclass.cconsts;
	List.map (fun (c:AST.astconst) -> verifyConstructorModifiers c.cmodifiers) aclass.cconsts;
	List.map (fun (c:AST.astconst) -> verifyMethodDuplicatedArguments c.cargstype) aclass.cconsts;
	List.map (verifyConstructorBody aclass) aclass.cconsts;
	List.map verifyClassConstructors (getClasses aclass.ctypes);
	() (*leave this unit to prevent recursive map problems*)

let rec verifyClassInitials (aclass:AST.astclass) = 
	List.map (fun (x:AST.initial)-> solveStatements aclass [] Type.Void [] x.block) aclass.cinits;
	List.map verifyClassInitials (getClasses aclass.ctypes);
	()

let verifyMethodModfier (mods:AST.modifier list) = 
	checkModifs(mods);
	if not (List.for_all (fun m -> inlist m [AST.Public;AST.Private;AST.Protected;AST.Abstract;AST.Static;AST.Final;AST.Synchronized;AST.Native;AST.Strictfp];) mods)
		then raise (InvalidModifier ("Invalid class modifier for method."));
	if (both mods AST.Native AST.Strictfp) then raise (InvalidModifier ("Both modifiers native and strictfp can't be present at the same time."));
	if (both mods AST.Private AST.Abstract) then raise (InvalidModifier ("Both modifiers abstract and private can't be present at the same time."));
	if (both mods AST.Abstract AST.Static) then raise (InvalidModifier ("Both modifiers abstract and static can't be present at the same time."));
	if (both mods AST.Abstract AST.Final) then raise (InvalidModifier ("Both modifiers abstract and final can't be present at the same time."))


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
						let fPub= inlist AST.Public fatherMethod.mmodifiers in
						let fPrt= inlist AST.Protected fatherMethod.mmodifiers in
						let cPrt= inlist AST.Protected cm.mmodifiers in
						let cPrv= inlist AST.Private cm.mmodifiers in
						let s1 = inlist AST.Static cm.mmodifiers in
						let s2 = inlist AST.Static fatherMethod.mmodifiers in (
							if ( (fPub && cPrt) || (fPub && cPrv) || (fPrt && cPrv)  ) 
								then raise (InvalidMethodReDefinition ("method "^cm.mname^": cannot redefine method with reduced visibility."));
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
		noRedefined@(List.filter ( fun (x:AST.astmethod) -> not (inlist AST.Private x.mmodifiers)) aclass.cmethods)
	)

let rec verifyMethodRedefinition (aclass:AST.astclass) =
	checkRedefineInheritedMethods aclass;
	List.map verifyMethodRedefinition (getClasses aclass.ctypes);
	()


(* Calls *)
let verifyClasses (var:AST.t) (classes:AST.astclass list)  =
	verifyNoClassDuplicates var.type_list;
	List.map verifyClassModifiers classes;
	List.map verifyMemberClassModifiers (getClasses var.type_list);
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
	
	
	