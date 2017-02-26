(* compile the ASTTyped *)
open AST
open MemoryModel
open Exceptions
open Type
open Location
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
		    mbody = body;
		    mloc = Location.none;
		    msemi = false;
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
		    cbody = body;
		    mloc = Location.none
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
			clid = "";
    		clname = "";
    		classScope = [];
    		clmodifiers = [];
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
			[] []
			[ 
				Return (Some {edesc=(Name "this");eloc=Location.none;etype=None})
			]
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


(* ----------------------------- Methods STRING ----------------------------------------------- *)
(* modifier name return throws statements *)
(* empty constructor that does nothing :) *)
let string_empty_constructor = get_constructor [Public] "String" 
			[] [] []
;;
(* simple constructor that receives a string *)
let string_constructor = get_constructor [Public] "String" 
			[{
		    	final = false ;
    			vararg = false ;
    			ptype = Array ((Primitive Char),1);
    			pident = "s"
			}] 
			[] 
			[
				(Expr {
					edesc=(AssignExp ({edesc=(Name "value");eloc=Location.none;etype=None}, Assign, {edesc=(Name "s");eloc=Location.none;etype=None}));
					eloc=Location.none;
					etype=None})
			]
;;

let string_toString : AST.astmethod = get_method [Public] "toString" 
			(Array ((Primitive Char),1)) 
			[] 
			[] 
			[ 
				Return (Some {edesc=(Name "value");eloc=Location.none;etype=None})
			]
;;

let string_length : AST.astmethod = get_method [Public] "length" 
			Void
			[] 
			[] 
			[ 
				Return (Some {edesc=(Val (Int "0"));
							eloc=Location.none;
							etype=None})
			]
;;

let string_isEmpty : AST.astmethod = get_method [Public] "isEmpty" 
			Void
			[] 
			[] 
			[ 
				Return (Some {edesc=(Val (Boolean false));
							eloc=Location.none;
							etype=None})
			]
;;

(* ----------------------------- END ----------------------------------------------- *)

(* ----------------------------- Class STRING ----------------------------------------------- *)
let stringmethods = [
		string_toString; string_length; string_isEmpty
	]
;;
let stringconstructors = [
		string_empty_constructor; string_constructor
	]
;;
let stringclass = get_class { tpath = []; tid = "Object" }
		    [{
      			amodifiers=[Public; Static; Final];
		      	aname="value";
		      	atype=Array ((Primitive Char),1);
		      	adefault=Some({edesc=(Val (String ""));eloc=Location.none;etype=None});
		      	aloc=Location.none
		    }] 
		    [] 
		    stringconstructors
		    stringmethods
			[]
			Location.none
;;
(* ----------------------------- END ----------------------------------------------- *)

(* ----------------------------- AST.Type STRING ----------------------------------------------- *)
let stringtype = get_asttype [] "String" (Class stringclass)


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
let nullpointerexception = get_class { tpath = []; tid = "Exception" }
		    [] [] 
		    []
		    []
			[]
			Location.none
;;
let arithmeticexception = get_class { tpath = []; tid = "Exception" }
		    [] [] 
		    []
		    []
			[]
			Location.none
;;
let arrayindexoutofboundsexception = get_class { tpath = []; tid = "Exception" }
		    [] [] 
		    []
		    []
			[]
			Location.none
;;

(* ----------------------------- END ----------------------------------------------- *)


(* ----------------------------- AST.Type EXCEPTION ----------------------------------------------- *)
let exceptiontype = get_asttype [] "Exception" (Class exceptionclass)
let nullpointerexceptiontype = get_asttype [] "NullPointerException" (Class nullpointerexception)
let arithmeticexceptiontype = get_asttype [] "ArithmeticException" (Class arithmeticexception)
let arrayindexoutofboundsexceptiontype = get_asttype [] "ArrayIndexOutOfBoundsException" (Class arrayindexoutofboundsexception)

(* add the methods 
	This takes the list of classes from JavaLang (defined above)
	and puts them into a new AST.
	This is going back to the main where it is actually compiled	
*)
let	add_default_classes (verb : bool) ast =
	let default_class_list = [ objecttype;
							stringtype;
							exceptiontype; 
							nullpointerexceptiontype; 
							arithmeticexceptiontype; 
							arrayindexoutofboundsexceptiontype 
							]
	in
	Log.info "Adding defaults";
	{
		package = ast.package;
		type_list = (List.append default_class_list ast.type_list)
	}
