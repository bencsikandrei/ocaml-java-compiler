(* compile the ASTTyped *)
open AST
open MemoryModel
open Exceptions
open Type
(* create an AST.astmethod *)
let get_method (modifs : modifier list) 
		(name : string) 
		(rettype : Type.t) 
		(argstype : argument list)
	    (throws : Type.ref_type list)
	    (body : statement list) : AST.astmethod =
    {
			mmodifiers = modifs;
		    mname = name;
		    mreturntype = rettype;
		    margstype = argstype;
		    mthrows = throws;
		    mbody = body
	}
;;

(* create an AST.astonstructor *)
let get_constructor (modifs : modifier list)
	    (name : string)
	    (argstype : argument list)
	    (throws : Type.ref_type list)
	    (body : statement list) =
	{
			cmodifiers = modifs;
		    cname = name;
		    cargstype = argstype;
		    cthrows = throws;
		    cbody = body
	}
;;

(* generate a class from all the arguments *)
let get_class (parent : Type.ref_type) 
		(attrs : astattribute list)
	    (inits : initial list)
	    (consts : astconst list)
	    (methods : astmethod list)
	    (types : asttype list)
	    (loc : Location.t) : AST.astclass =
	{		
			cparent = parent;
		    cattributes = attrs;
		    cinits = inits;
		    cconsts = consts;
		    cmethods = methods;
			ctypes = types;
			cloc = loc
	}
;;

let get_asttype (modifs : modifier list)
	    (i : string)
	    (inf : type_info) =
	{	
		    modifiers = modifs;
		    id = i;
		    info = inf;
	}
(* ----------------------------- Methods OBJECT ----------------------------------------------- *)
(* hashcode method
does nothing *)
let hashCode : AST.astmethod = get_method [Public] "hashCode" (Primitive Int) 
			[] [] []
;;
(* the equals method *)
let equals : AST.astmethod = get_method [Public] "equals" (Primitive Boolean)
			[{
		    	final = false ;
    			vararg = false ;
    			ptype = Ref { tpath = []; tid = "Object" };
    			pident = "o"
    			}
 			] [] []
;;

(* the to string *)
let toString : AST.astmethod = get_method [Public] "toString" 
			(Ref { tpath = []; tid = "String" }) 
			[] [] []
;;
(* ----------------------------- END ----------------------------------------------- *)

(* ----------------------------- Class OBJECT ----------------------------------------------- *)
let objectmethods = [
		toString; equals; hashCode
	]
;;
let objectclass = get_class { tpath = []; tid = "" }
		    [] [] []
		    objectmethods
			[]
			Location.none
;;
(* ----------------------------- END ----------------------------------------------- *)



(* ----------------------------- AST.Type OBJECT ----------------------------------------------- *)
let objecttype = get_asttype [] "Object" (Class objectclass)

(* ----------------------------- Constructors EXCEPTION ----------------------------------------------- *)
(* the to string *)
let exception_void = get_constructor [Public] "Exception"[] [] []
let execution_string = get_constructor [Public] "Exception" 
			[{
		    	final = false ;
    			vararg = false ;
    			ptype = Ref { tpath = []; tid = "String" };
    			pident = "s"
			}] [] []
(* ----------------------------- END ----------------------------------------------- *)

(* ----------------------------- Class EXCEPTION ----------------------------------------------- *)
let exceptionmethods = [
		
]
;;
let exceptionconstructors = [
	exception_void; execution_string
]
;;
let exceptionclass = get_class { tpath = []; tid = "Object" }
		    [] [] 
		    exceptionconstructors
		    exceptionmethods
			[]
			Location.none
;;
(* ----------------------------- END ----------------------------------------------- *)


(* ----------------------------- AST.Type EXCEPTION ----------------------------------------------- *)
let exceptiontype = get_asttype [] "Exception" (Class exceptionclass)


(* add the methods 
	This takes the list of classes from JavaLang (defined above)
	and puts them into a new AST.
	This is going back to the main where it is actually compiled	
*)
let	add_default_classes ast =
	let default_class_list = [ objecttype; exceptiontype ]
	in
	print_endline "Adding defaults";
	{
		package = ast.package;
		type_list = (List.append default_class_list ast.type_list)
	}
