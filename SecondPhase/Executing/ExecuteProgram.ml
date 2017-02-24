(* Code for doing the execution *)
open Type
open AST
open Exceptions
open MemoryModel
open Log

(* the return value *)
exception ReturnValue of valuetype
(* returns the value as ocaml primitive from valuetype *)
let valuetype_to_ocaml_int (v : valuetype) =
    match v with
    | IntVal(i) -> i

(* get class name from method in jvm table *)
let get_class_name_from_jvm_method (meth : string) =
    (* takes out the class name *)
    String.sub meth 0 (String.index meth '_')

(* create a scope from the name given *)
let get_new_scope (name : string) =
    (name,
    {   
        visible = (Hashtbl.create 10)
    })

(* add object to heap *)
let add_object_to_heap (jprog : jvm) (newobj : newobject) =
    (* add it to the next free position *)
    Hashtbl.add jprog.jvmheap (jprog.nextfree) newobj;
    let oldaddr = jprog.nextfree
    in
    (* increment the free position *)
    jprog.nextfree <- jprog.nextfree + 1;
    (* return the address the object is at *)
    oldaddr

(* reutrn the object at a certain address *)
let get_object_from_heap (jprog : jvm) (addr : int) =
    Hashtbl.find jprog.jvmheap addr

(* get the scope *)
let get_current_scope (jprog : jvm) =
    Stack.top jprog.jvmstack

(* is one class the child of the other *)
let rec is_superclass (jprog : jvm) (chld : javaclass) (prt : string) =
    let prnt = chld.cparent.tid
    in
    Log.debug false ("The parent is "^prnt);
    Log.debug false ("The test is "^prt);
    if (prnt = prt) then true
    else begin
        if (prnt = "Object") then false
        else
            is_superclass jprog (Hashtbl.find jprog.classes prnt) prt
    end

(* receives a valuetype and returns its Type.t equivalent *)
let rec valuetype_to_t (value : valuetype) : Type.t =
    match value with
    | TypeVal(t) -> t
    | ArrayVal(arr) -> Array((valuetype_to_t arr.atype),(valuetype_to_ocaml_int (List.nth arr.adim 0)))
    | VoidVal -> Void 
    | IntVal(i) -> Primitive(Int)
    | FltVal(f) -> Primitive(Float)
    | BoolVal(b) -> Primitive(Boolean)
    (*| StrVal(s) -> 
    | RefVal(rf) -> Ref({tpath=[];tid=}) (* empty path! TODO CHECK *)
    | NullVal
    *)

(* checks that the elements of a list are of some valuetype, returns the type as TypeVal otherwise raises an exception *)
let check_elements_type (l : valuetype list) : valuetype =
    let ty = (TypeVal (valuetype_to_t (List.nth l 0)))
    in
    (List.iter (fun x -> if ((TypeVal (valuetype_to_t x)) <> ty) then (raise (Exception "Incompatible types in array"))) l);
    ty

(* build list of n lenght with the default value *)
(* gives a list of valuetype of the given dimension with default values according to the type *)
let rec build_list (jprog : jvm) (t : Type.t) (i : int) (dim : valuetype list) = 
    (* only treating one dimension *)
    let n : int = (match (List.nth dim 0) with | IntVal(i) -> i)
    in
    match t with
    | Primitive(p) -> if i < n then (Hashtbl.find jprog.defaults p)::(build_list jprog t (i+1) dim) else []

(* find the start point *)
let get_main_method (jprog : jvm) =
    (* search if there is a main method, 
    if yes -> return it
    else raise an exception *)
    let main_method_name = (jprog.public_class ^ "_main_String[]") (* classic java main method *)
    in
    try 
        (* get the method from the jvm table *)
        Hashtbl.find jprog.methods main_method_name
    with
    | _ -> raise (NoMainMethod ("Error: Main method not found in class " ^ jprog.public_class ^ 
                                ", please define the main method as: public static void main(String[] args)" ^
                                "or a JavaFX application class must extend javafx.application.Application "))

(* determine if a class is throwable *)
let is_throwable (jprog : jvm) (jcls : javaclass) = 
    (* iterate all parents until we find Exception
    , or not *)
    let parent = jcls.cparent.tid 
    in
    let rec find_parent (par : string) = begin
        match par with 
        | "Exception" -> true (* if exception is along the way, we are good *)
        | "Object" -> false (* we reached Object, inevitably, we failed *)
        | _ -> let nextpar = (Hashtbl.find jprog.classes par)
                in
                (* check the next parent *)
                find_parent nextpar.cparent.tid
        end
    in
    find_parent parent

(* 
let rec get_var_names (jprog : jvm) vardecls =
    match vardecls with
    | [] -> [] 
    | hd::tl -> match hd with
                (* type, name, optional initialization *)
                | (Primitive(p), n, eo) -> 
                        begin
                        let (_, scope) = Stack.top jprog.jvmstack 
                        in
                        (* matched an  *)
                        Hashtbl.add scope.visible n (match eo with 
                                                    | None -> Hashtbl.find jprog.defaults p;
                                                    (* we need type checks here*)
                                                    | Some(e) -> execute_expression jprog e)
                        end
 *)
(* add vars to current scope *)
let rec add_vars_to_scope (jprog : jvm) decls =
    let (_, scope) =  Stack.top jprog.jvmstack 
    in 
    match decls with
    | [] -> ()
    | hd::tl -> let (n, v) = hd
            in
            (* print_string n; print_endline( "= " ^(string_of_value v)); *)
            Hashtbl.add scope.visible n v;
            add_vars_to_scope jprog tl

(* remove block variables *)
let rec remove_vars_from_scope (jprog : jvm) decls =
    (* print_endline "Any vars to remove from scope ?"; *)
    let (_, scope) =  Stack.top jprog.jvmstack 
    in 
    match decls with
    | [] -> ()
    | (n, v)::tl -> 
            (* print_endline "Some"; *)
            (* print_string n; print_endline(string_of_value v); *)
            Hashtbl.remove scope.visible n;
            remove_vars_from_scope jprog tl

(* compute the value of an expression with operators *)
let compute_value op val1 val2 =
    match val1,val2 with
    | IntVal(v1),IntVal(v2) -> begin
            match op with
            | Op_add -> IntVal(v1 + v2)
            | Op_sub -> IntVal(v1 - v2)
            | Op_mul -> IntVal(v1 * v2)
            | Op_div -> IntVal(v1 / v2) (* exception when v2 is 0 *)
            | Op_mod -> IntVal(v1 mod v2)
            | Op_or -> IntVal(v1 lor v2)  
            | Op_and -> IntVal(v1 land v2)
            | Op_xor -> IntVal(v1 lxor v2)
            | Op_shl -> IntVal(v1 lsl v2)
            | Op_shr -> IntVal(v1 lsr v2)
            | Op_eq -> BoolVal(v1 == v2) (* the comparisons should work with every type *)
            | Op_ne -> BoolVal(v1 != v2) 
            | Op_gt -> BoolVal(v1 > v2) 
            | Op_lt -> BoolVal(v1 < v2) 
            | Op_ge -> BoolVal(v1 >= v2)
            | Op_le -> BoolVal(v1 <= v2)
            (*| Op_shrr *)
            end
    | FltVal(v1),FltVal(v2) -> begin
            match op with
            | Op_add -> FltVal(v1 +. v2)
            | Op_sub -> FltVal(v1 -. v2)
            | Op_mul -> FltVal(v1 *. v2)
            | Op_div -> FltVal(v1 /. v2) (* exception when v2 is 0 *)
            end
    | BoolVal(v1),BoolVal(v2) -> begin
            match op with 
            | Op_cand -> BoolVal(v1 && v2)
            | Op_cor -> BoolVal(v1 || v2)
            end
    (* automatic conversion to string *)
    | StrVal(s), ( _ as v2) -> begin 
            StrVal(s^(string_of_value v2))
            end
    | ( _ as v1), StrVal(s) -> begin 
            StrVal((string_of_value v1)^s)
            end
    | _,_ -> raise (Exception "Not yet implemented or incorrect operation")

(* do var++ and var--*)
let rec execute_postfix (jprog : jvm) (e : expression) postop =
    (* see what type *) 
    let one = { edesc = Val(Int("1")); eloc = Location.none; etype = None }
    in
    match postop with
    | Incr -> execute_assign jprog e Ass_add one
    | Decr -> execute_assign jprog e Ass_sub one
    

(* do ++var and --var *)
and execute_prefix (jprog : jvm) preop (e : expression) =
    (* see what type *)
    match preop with
    | Op_incr -> begin 
            match (execute_expression jprog e) with
            | IntVal(v) -> IntVal(v+1)
            | _ -> raise ArithmeticException
            end
    | Op_decr -> begin 
            match (execute_expression jprog e) with
            | IntVal(v) -> IntVal(v-1)
            | _ -> raise ArithmeticException
            end

(* do assignments *)
and execute_assign (jprog : jvm) e1 (op : assign_op) e2 =
    (* see what type *)
    let right = (execute_expression jprog e2)
    in
    let (_, scope) = Stack.top jprog.jvmstack 
    in
    let result = begin
                match op with
                | Assign -> right
                | Ass_add -> (execute_operator jprog e1 Op_add e2)
                | Ass_sub -> (execute_operator jprog e1 Op_sub e2)
                | Ass_mul -> (execute_operator jprog e1 Op_mul e2)
                | Ass_div -> (execute_operator jprog e1 Op_div e2)
                | Ass_mod -> (execute_operator jprog e1 Op_div e2)
                | Ass_and -> (execute_operator jprog e1 Op_and e2) (* and or cand ?? *)
                | Ass_or -> (execute_operator jprog e1 Op_or e2) (* or or cor ?? *)
                | Ass_xor -> (execute_operator jprog e1 Op_xor e2)
                | Ass_shl -> (execute_operator jprog e1 Op_shl e2)
                | Ass_shr -> (execute_operator jprog e1 Op_shr e2)
                (*| Ass_shrr*)
                end;
    in
    match e1.edesc with
            | Name(n) -> begin
                        match (Hashtbl.mem scope.visible n) with
                        | true -> Hashtbl.replace scope.visible n result;
                                Hashtbl.find scope.visible n
                        | false -> raise (Exception ("Variable not defined "^n))
                        end
            | Attr(exp,name) -> begin (* when this.name is used got to change the attribute of the object *) 
                            match (execute_expression jprog exp) with
                            | RefVal(addr) -> (* get the object from the heap *)
                                            let obj = get_object_from_heap jprog addr 
                                            in
                                            if (Hashtbl.mem obj.oattributes name)
                                            then ((Hashtbl.replace obj.oattributes name result); (Hashtbl.find obj.oattributes name))
                                            else raise NoSuchFieldException 
                            end; NullVal
            | Array(ex,exol) -> begin (* only considering one dimension! *)
                                match (List.nth exol 0) with 
                                | Some(e) -> begin
                                            match (execute_expression jprog e) with
                                            | IntVal(i) -> let var = (string_of_expression ex)
                                                        in (* check it array exists *)
                                                        if (Hashtbl.mem scope.visible var)
                                                        then Hashtbl.replace scope.visible var (array_set_nth (Hashtbl.find scope.visible var) i result)
                                                        else raise (Exception ("Variable not defined "^var))
                                            end
                                | None -> raise (Exception "This should be accepted by the parser but it does. SHAME!")
                                end; NullVal 
            | _ -> raise (Exception "Bad assignment")

(* used to return a list with the nth element modified of the given array *)
and array_set_nth (arr : valuetype) (n : int) (result : valuetype) =
match arr with
| ArrayVal(a) -> if (n < (valuetype_to_ocaml_int (List.nth a.adim 0)))
                then (if (a.atype=(TypeVal (valuetype_to_t result)))
                    then ArrayVal({atype=a.atype;adim=a.adim;aname=a.aname;avals=(List.mapi (fun i x -> if (i=n) then result else x) a.avals)})
                    else raise (Exception "Incompatible type"))
                else raise IndexOutOfBoundsException

(* variable linking *)
and execute_name (jprog : jvm) (name : string) =
    if (name="this") then print_endline "this" else (); (* TODO define this *)
    let (_, scope) = Stack.top jprog.jvmstack 
    in
    try 
        Hashtbl.find scope.visible name
    with
    | Not_found -> raise (Exception ("Variable not defined "^name ))

(* execute operation *)
and execute_operator (jprog : jvm) e1 (op : infix_op) e2 =
    let left = (execute_expression jprog e1)
    in 
    let right = (execute_expression jprog e2)
    in
    compute_value op left right

(* values from AST types *)
and execute_val v =
    match v with
    | String(s) -> StrVal(s)
    | Int(i) -> IntVal(int_of_string i)
    | Float(f) -> FltVal(float_of_string f)
    | Boolean(b) -> BoolVal(b) 
    | Null -> NullVal

(* execute a ternaru a > b ? a: b *)
and execute_ternary (jprog : jvm) exp1 exp2 exp3 =
    match (execute_expression jprog exp1) with
    | BoolVal(true) -> execute_expression jprog exp2
    | BoolVal(false) -> execute_expression jprog exp3
    | _ -> raise (Exception "Illegal ternary operator values")

(* execute a list of expressions *)
and execute_expressions (jprog : jvm) exps =
    match exps with
    | [] -> []
    | hd::tl -> let decl = execute_expression jprog hd 
                in
                [decl] @ (execute_expressions jprog tl)

(* create a new instance *)
and execute_new (jprog : jvm) (classname : string) (params : expression list) =
    (* check if the constructor exists for the class *)
    let jcls = try (Hashtbl.find jprog.classes classname) with | Not_found -> raise ClassNotFoundException
    in
    let signature = classname^(get_method_signature_from_expl jprog params "")
    in
    let constructor = try (Hashtbl.find jcls.jconsts signature) with | Not_found -> raise IllegalArgumentException
    in
    let attrs : (string, valuetype) Hashtbl.t = (Hashtbl.create 20)
    in
    (* add attributes to the attrs of the object *)
    (List.iter (add_attr jprog attrs) jcls.jattributes);
    (* get the values of the arguments *)
    let arg_vals = (get_argument_list jprog constructor.cargstype params [])
    in
    (* new scope required *)
    Stack.push (get_new_scope constructor.cname) jprog.jvmstack;
    (* add attributes to scope *)
    (add_vars_to_scope jprog (Hashtbl.fold (fun k v acc -> (k, v)::acc) attrs []));
    (* run non-static block from class *)
    let st_vals = (List.map (fun x -> if (x.static=false) then (execute_statements jprog x.block) else []) jcls.cinits)
    in
    (* apply attribute changes to return the object *)
    List.iter (fun v -> apply_attrs_modification jprog attrs v) st_vals;
    (* run constructor *)
    (execute_constructor jprog constructor attrs arg_vals);
    (* take out of scope *)
    Stack.pop jprog.jvmstack;
    let newobj = {oclass=jcls;oattributes=attrs}
    in
    (* adding the object to the heap *)
    let addr = add_object_to_heap jprog newobj 
    in
    (* return object *)
    RefVal(addr)

(* execute a constructor has to modify the attributes *)
and execute_constructor (jprog : jvm) (c : astconst) (attrs : (string, valuetype) Hashtbl.t) (params : (string * MemoryModel.valuetype) list) =
    begin
    (* add constructor's arguments to the scope *)
    (add_vars_to_scope jprog params);
    (* execute all statements of a constructor, when there is a return catch the event and return it's value *)
    execute_statements jprog c.cbody;
    (* see if the variables in scope are part of the object *)
    let (_, scope) = Stack.top jprog.jvmstack
    in
    Hashtbl.iter (fun k v -> if (Hashtbl.mem attrs k) then Hashtbl.replace attrs k v) scope.visible;
    end

(* receives and astattribute and returns name and valuetype to add to hashtable *)
and add_attr (jprog : jvm) (ht : (string, valuetype) Hashtbl.t) (attr : astattribute) =
    let init = (match attr.adefault with | Some(e) -> (execute_expression jprog e)
                                         | None -> (match (attr.atype) with
                                                  | Array(ty,dim) -> ArrayVal({atype=(TypeVal ty);aname=None;avals=(build_list jprog ty 0 [IntVal(dim)]);adim=[IntVal(dim)]})
                                                  | Primitive(p) -> Hashtbl.find jprog.defaults p)
                                                  (*| Ref(r) -> RefVal({oclass=javaclass;oattributes=}))*))
    in
    Hashtbl.add ht (attr.aname) init

(* receives params as expression list and returns the signature *)
and get_method_signature_from_expl (jprog : jvm) (params : expression list) (strparams : string) =
    let spars = match params with
                | [] -> strparams (* we are done *)
                | hd::tl -> match (execute_expression jprog hd) with
                            | IntVal(_) -> get_method_signature_from_expl jprog tl (strparams^"_int")
                            | BoolVal(_) -> get_method_signature_from_expl jprog tl (strparams^"_boolean")
                            | RefVal(addr) -> let ob = get_object_from_heap jprog addr 
                                            in
                                            get_method_signature_from_expl jprog tl (strparams^"_"^ob.oclass.id)
                            | FltVal(_) -> get_method_signature_from_expl jprog tl (strparams^"_float")
                            | _ -> ""
                            (*| Array(t, int) -> match t with 
                                                | Primitive(Int) -> get_method_signature tl (strparams^"_int[]")
                                                | Ref(rt) -> get_method_signature tl (strparams^"_"^rt.tid^"[]")*)
    in
    if (spars = "")
        then 
            "_void"
    else
        spars

(* receives a list of params an returns a list of (name argument, value) for methods and constructors *)
and get_argument_list (jprog : jvm) (arglist : argument list) (params : expression list) (paraml : (string * MemoryModel.valuetype) list) =
    match arglist,params with
    | [],[] -> paraml (* we are done *)
    | hd::tl,hd2::tl2 -> get_argument_list jprog tl tl2 (paraml@[hd.pident,(execute_expression jprog hd2)])
    (* temporary fix for main method, we don't treat the String[] args yet *)
    | _, [] -> []
(* receives the attributes from a class with a list of values
 searches in this list the ones that are attributes modifications and overwrites the values in attrs *)
and apply_attrs_modification (jprog : jvm) (attrs : (string, valuetype) Hashtbl.t) (vars : (string * MemoryModel.valuetype) list) =
    List.iter (fun (name, value) -> if (Hashtbl.mem attrs name) then (Hashtbl.replace attrs name value) else ()) vars

(* casting, just to have an structure, because typing is supossed to do by the other group *)
and execute_cast (jprog : jvm) (ty : Type.t) (exp : expression) =
    match (execute_expression jprog exp) with
    | IntVal(i) -> begin
                    match ty with 
                    | Primitive(p) -> begin
                                    match p with
                                    | Int -> IntVal(i)
                                    | Float -> FltVal(float_of_int i)
                                    end
                    end
    | FltVal(i) -> begin
                    match ty with 
                    | Primitive(p) -> begin
                                    match p with
                                    | Int -> IntVal(int_of_float i)
                                    | Float -> FltVal(i)
                                    end
                    end

(* execute an expression and send back it's value *)
and execute_expression (jprog : jvm) expr =
    (* check the descriptor *)
    match expr.edesc with 
    | Val(v) -> execute_val v
    | Post(e, poi) -> execute_postfix jprog e poi
    | Pre(pri, e) -> execute_prefix jprog pri e
    | Name(n) -> execute_name jprog n
    | AssignExp(e1, op, e2) -> execute_assign jprog e1 op e2
    | Op(e1, op, e2) -> execute_operator jprog e1 op e2
    | CondOp(e1, e2, e3) -> execute_ternary jprog e1 e2 e3
    | ArrayInit(el) -> let vlist = (execute_expressions jprog el)
                    in
                    ArrayVal({atype=(check_elements_type vlist);aname=None;avals=vlist;adim=[IntVal(List.length el)]})
    | NewArray(t,expol,expo) -> (* type, dimension, initialization  *)
                                let dim=(match expol with | [] ->  [IntVal(0)]
                                                           | _ -> (List.map (fun expo -> (match expo with 
                                                                                        | None -> IntVal(0)
                                                                                        | Some(e) -> (execute_expression jprog e))) expol))
                                in
                                let init=(match expo with   | None -> ArrayVal({atype=(TypeVal t);aname=None;adim=dim;avals=(build_list jprog t 0 dim)})
                                                            | Some(e) -> execute_expression jprog e) (* overwrites the dimension *)
                                in
                                init
    | Instanceof(e,t) -> begin 
                        match (execute_expression jprog e),t with
                        | RefVal(addr),Ref(r2) -> let r1 = get_object_from_heap jprog addr in
                                if (r1.oclass.id = r2.tid) then BoolVal(true) else BoolVal(false) (* not considering path yet TODO *)
                        | _,_ -> BoolVal(false)
                        end
    | New(stro,strl,expl) -> (* something ?? classname args *)
                            let obj = (match stro with | None -> execute_new jprog (List.nth strl ((List.length strl)-1)) expl (* only local *)
                                                       | Some(s) -> (print_endline "Inner classes, not implemented"); NullVal)
                            in
                            obj
    | Array(exp,expol) -> begin
                        match (execute_expression jprog exp) with 
                        | ArrayVal(arr) -> let indx = (List.map (fun expo -> (match expo with
                                                            | None -> IntVal(0)
                                                            | Some(e) -> (execute_expression jprog e) )) expol)
                                            in (* only checking one dimension *)
                                            let one_dim = (valuetype_to_ocaml_int (List.nth indx 0))
                                            in
                                            try (List.nth arr.avals one_dim) with
                                            | Failure(_) -> raise IndexOutOfBoundsException
                        | _ -> raise (Exception "Not an array")
                        end
    | Attr(exp,name) -> begin
                        match (execute_expression jprog exp) with
                        | RefVal(addr) -> try
                                let obj = get_object_from_heap jprog addr 
                                in
                                (Hashtbl.find obj.oattributes name)
                                with
                                | _ -> raise NoSuchFieldException 
                        end
    | Type(t) -> TypeVal(t)
    | ClassOf(ty) -> print_endline "ClassOf - I have no idea what this is, if this appears, let met know plz"; NullVal
    | VoidClass -> VoidVal
    | Cast(ty,exp) -> execute_cast jprog ty exp
    | Call(expo, name, args) -> begin

            let obj = match expo with | None -> VoidVal
                                    | Some({ edesc = Name(id) }) -> Log.debug false ("Just id: "^id);
                                            let (_,scope) = get_current_scope jprog
                                            in
                                            Hashtbl.find scope.visible id
                                    | Some({ edesc = Attr(o, id)}) -> Log.debug false ("Object name: "^id); NullVal
            (* after we have the jvmheap made, we can actually try for NULL object exception *)
            in
            (* get the signature *)
            let signature = name^(get_method_signature_from_expl jprog args "")
            in
            Log.debug false signature;
            (* A method withoud an object *)
            match (obj, name) with
            | (_, "println") -> print_endline (string_of_value (execute_expression jprog (List.hd args))); 
                    VoidVal
            | (RefVal(addr), n) -> (* we need to the the object *)
                    let obj = get_object_from_heap jprog addr
                    in
                    (* which class is the method from? *)
                    let mname = Hashtbl.find (obj.oclass.jcmethods) signature
                    in
                    (* change the scoped class if it's an object *)
                    let backupscope = jprog.scope_class
                    in
                    jprog.scope_class <- get_class_name_from_jvm_method mname;
                    (* the return value of the method is given, the method needs the class *)
                    let v = execute_call jprog (Hashtbl.find jprog.classes jprog.scope_class) (Some obj) signature args
                    in
                    jprog.scope_class <- backupscope;
                    v
                    
            | (VoidVal, _) -> (* the return value of the method is given, the method needs the class *)
                    Log.debug false ("Class name "^jprog.scope_class);
                    execute_call jprog (Hashtbl.find jprog.classes jprog.scope_class) None signature args;
            | (NullVal, _) -> 
                    raise NullPointerException
                    
            end;
            
    | _ -> StrVal("Not yet implemented")

(* execute a function call *)
and execute_call (jprog : jvm) (cls : javaclass) (obj : newobject option) (signature : string) (args : expression list) = 
    (* find the method and link it dynamicly *)
    let signaturejvm = try (Hashtbl.find cls.jcmethods signature) with | Not_found -> raise (Exception "Method not defined")
    in
    Log.debug false signaturejvm;
    let meth = (Hashtbl.find jprog.methods signaturejvm)
    in
    (* get values of the arguments*)
    let arg_vals = (get_argument_list jprog meth.margstype args [])
    in
    (* add the main mathods scope to the stack *)
    Stack.push (get_new_scope meth.mname) jprog.jvmstack;
    (* add the object's attributes to scope *)
    (add_vars_to_scope jprog (match obj with 
            | Some(o) -> (Hashtbl.fold (fun k v acc -> (k, v)::acc) o.oattributes []) 
            | None -> []));
    (* use the method in the JVM to run it *)
    execute_method jprog meth arg_vals

(* execute a variable declaration *)
and execute_vardecl (jprog : jvm) (decls : (Type.t * string * expression option) list) declpairs = 
    (* at the end give a list of all declared variables *)
    match decls with
    | [] -> declpairs
    | hd::tl -> begin
            match hd with
            (* type, name, optional initialization *)
            | (Primitive(p), n, eo) -> 
                    let v = (match eo with | None -> Hashtbl.find jprog.defaults p
                                           | Some(e) -> execute_expression jprog e) 
                    in
                    execute_vardecl jprog tl (declpairs@[(n, v)]) (* return a list of tuple (name * value) *)
            | (Array(t,size), n, eo) -> 
                    let v = (match eo with | None -> ArrayVal({atype=(TypeVal t);aname=Some(n);adim=[IntVal(size)];avals=(build_list jprog t 0 [IntVal size])})
                                           | Some(e) -> execute_expression jprog e)
                    in
                    execute_vardecl jprog tl (declpairs@[(n, v)])
            | (Ref(rt), n, eo) -> 
                    let v = (match eo with | None -> NullVal
                                           | Some(e) -> execute_expression jprog e)
                    in
                    execute_vardecl jprog tl (declpairs@[(n, v)])
            end

and execute_for_vardecl (jprog : jvm) (decls : (Type.t option * string * expression option) list) declpairs = 
    (* at the end give a list of all declared variables *)
    match decls with
    | [] -> declpairs
    | hd::tl -> begin
            match hd with
            (* type, name, optional initialization *)
            | (Some(Primitive(p)), n, eo) -> 
                    let v = (match eo with | None -> Hashtbl.find jprog.defaults p
                                           | Some(e) -> execute_expression jprog e) 
                    in
                    execute_for_vardecl jprog tl (declpairs@[(n, v)]) (* return a list of tuple (name * value) *)
            | (None, n, eo) -> 
                    let v = (match eo with | Some(e) -> execute_expression jprog e) 
                    in
                    execute_for_vardecl jprog tl (declpairs) (* return a list of tuple (name * value) *)
            end

(* execute an if condition with possible else *)
and execute_if (jprog : jvm) e (stmt : statement) elseopt =
    match (execute_expression jprog e) with
    | BoolVal(true) -> execute_statement jprog stmt
    | BoolVal(false) -> begin
            match elseopt with 
            (* execute the else part *)
            | Some(s) -> execute_statement jprog s
            | _ -> []
            end
    | _ -> raise (Exception "Illegal if condition")

(* simple while loop *)
and execute_while (jprog : jvm) e (stmt : statement) = 
    match (execute_expression jprog e) with
    | BoolVal(true) -> execute_statement jprog stmt; execute_while jprog e stmt
    | BoolVal(false) -> ()
    | _ -> raise (Exception "Illegal condition in while loop")

(* the simple for statement, not enhanced *)
and execute_for (jprog : jvm) fortest forincr (stmt : statement) =
    let test = (match fortest with 
                | Some(e) -> execute_expression jprog e
                | None -> BoolVal(true))
    in
    match test with
    | BoolVal(true) ->  execute_statement jprog stmt; 
                        List.iter (fun t -> execute_expression jprog t; ()) forincr;
                        execute_for jprog fortest forincr stmt
    | BoolVal(false) -> ()
    | _ -> raise (Exception "Illegal condition in while loop")

and get_exception_object (jprog : jvm) (expn : exn) =
    match expn with
    | NullPointerException -> execute_new jprog "NullPointerException" []
    | _ -> execute_new jprog "Exception" []

(* raise an OCaml exception from a java object *)
and raise_exception (expn : string) =
    match expn with
    | "NullPointerException" -> raise NullPointerException
    | "ArithmeticException" -> raise ArithmeticException
    | "IndexOutOfBoundsException" -> raise IndexOutOfBoundsException
    | _ -> raise (Exception expn)

(* execute try catch finally *)
and execute_try (jprog : jvm) (trysl : statement list) 
                (catchsl : (argument * statement list) list) (finsl : statement list) =
    (* execute the statements in try *)
    let tryvars = (
    try 
        execute_statements jprog trysl
    with 
    | _ as expn -> begin
            let e = get_exception_object jprog expn 
            in
            execute_catches jprog catchsl e false
            end)
    in
    remove_vars_from_scope jprog tryvars;
    (* execute the finally if it exists *)
    execute_statements jprog finsl

(* take each catch an execute it *)
and execute_catches (jprog : jvm) (catchsl : (argument * statement list) list) (e : valuetype) (caught : bool) =
    (* take the list of catches and iterate over it *)
                (* the exception has been put into the heap *)
    let expn = get_object_from_heap jprog (match e with | RefVal(addr) -> addr)
    in
    match catchsl, caught with
    (* if none left and exception was caugth we finished *)
    | [], true -> []
    (* if none left but exception not caught, we push it onwards *)
    | [], false -> raise_exception expn.oclass.id
    | (arg, sl)::tl, _ -> 
            (* get the argument type *)
            let argexp = (match arg.ptype with | Ref(rt) -> rt.tid)
            in
            (* now, is it the exception in that catch ? or exc *)
            if (expn.oclass.id = argexp || (is_superclass jprog (Hashtbl.find jprog.classes expn.oclass.id) argexp))
            then begin
                execute_statements jprog sl;
                execute_catches jprog [] e true
            end
            else 
                execute_catches jprog tl e false;
            []

(* execute all sorts of statements *)
and execute_statement (jprog : jvm) (stmt : statement) : (string * MemoryModel.valuetype) list = 
    match stmt with
    (* treat all the expressions *)
    | Expr(e) -> (* print_endline "Executing an expression"; *)
                execute_expression jprog e; (* print_endline (AST.string_of_expression e) *)
                []
    (* a variable declaration *)
    | VarDecl(vardecls) -> let declarations = execute_vardecl jprog vardecls []
            in
            add_vars_to_scope jprog declarations;
            declarations
    (* blocks *)
    | Block(stmtlst) -> 
    		begin
    		try 
	    		let blockvars = List.append [] (execute_statements jprog stmtlst) 
	            in
	            remove_vars_from_scope jprog blockvars;
	            []
        	with 
        	| _ -> []
        	end
    (* the if statement *)
    | If(e, stmt, elseopt) -> execute_if jprog e stmt elseopt; []
    (* while *)
    | While(test, stmt) -> execute_while jprog test stmt; []
    (* the for loop *)
    | For(vardecls, fortest, forincr, forstmt) ->
            (* first add the for declared variables to scope *)
            let declarations = execute_for_vardecl jprog vardecls []
            in
            (* scope will work because of how OCaml Hashtbl is programmed
            --> collision means adding a linked list, so we can easily
            take out the last variables that were added :) *)
            add_vars_to_scope jprog declarations;
            (* do the work *)
            execute_for jprog fortest forincr forstmt;
            (* take all for variables out *)
            remove_vars_from_scope jprog declarations;
            (* a for does not declare variables out of its scope *)
            []
    (* must throw a throwable object, for the moment only those who extend
    from Exception, direcly or not *)
    | Throw(e) -> begin
            (* when a java exn is thrown *) 
            Log.debug false "An excetion has been thrown";
            (* get the exception object *)
            let exnref = execute_expression jprog e
            in
            (* check if it's an object that extends Exception *)
            match exnref with
            | RefVal(addr) as v -> 
                    let tro = get_object_from_heap jprog addr
                    in
                    if (is_throwable jprog tro.oclass) 
                    then 
                        (* [(tro.oclass.id, v)] *)
                        raise_exception (tro.oclass.id)
                    else 
                        raise (Exception "Not throwable")
            | _ -> raise (Exception "Not throwable")
            end

    (* try catch finally *)
    | Try(trysl, catchsl, finsl) ->
            begin
            execute_try jprog trysl catchsl finsl
            end
    | Return(eo) -> (* a return must pop the top of the stack *)
			begin
            (* raise an event when we return something, to stop the execution *)
			let returnvalue = (match eo with 
			| None -> VoidVal
			| Some(e) -> execute_expression jprog e)
		    in
		    (* a way to stop the running statements is to raise an event
		    this event is called ReturnValue *)
		    raise (ReturnValue returnvalue)
			end
    (* a lot of stuf is marked as NOP so we don't need it *)
    | Nop -> print_endline "Not implemented in the parser"; []
    | _ -> print_endline "Statement not executable yet, try a System.out.println().."; []

(* execute statements like List.iter 
    a statement can decalre a new variable or not
    it's the case for VarDecl
    so if a block declares a variable, we add it 
    to a list
    *)
and execute_statements (jprog : jvm) (stmts : statement list) =
    begin
    match stmts with
    | [] -> []
    | hd::tl -> (* execute one statement, then pass to others *)
                let decl = execute_statement jprog hd 
                in
                decl @ (execute_statements jprog tl)
    end

(* execute a method *)
and execute_method (jprog : jvm) (m : astmethod) (args : (string * MemoryModel.valuetype) list) =
    (* add method's arguments to the scope *)
    (add_vars_to_scope jprog args);
	let ret = (try
		(* execute all statements of a method, when there is a return
		catch the event and return it's value *)
		execute_statements jprog m.mbody; 
		(* if there is no return statement, it's void *)
		VoidVal
	   with 
        | ReturnValue(v) -> v)
	in
    (* print the contents of the scope *)
    print_scope jprog;
    (* pop the stack *)
    Log.debug false "Removing a scope";
    begin
    try
        Stack.pop jprog.jvmstack;()
    with 
    | _ -> Log.debug false "### --------- ###";
            Log.debug false ("Exited with " ^ (string_of_value ret));
    end;
    ret

(* add default initializer variables *)
let add_defaults (jprog : jvm) =
    Hashtbl.add jprog.defaults Int (IntVal 0);
    (* Hashtbl.add jprog.defaults Int StrVal(""); 
    *)
    Hashtbl.add jprog.defaults Float (FltVal 0.0);
    Hashtbl.add jprog.defaults Boolean (BoolVal false)

(* Make a structure that contains the whole program, its heap
stack .. *)
let execute_code (jprog : jvm) =
    (* setup the JVM *)
    add_defaults jprog;
    let startpoint = get_main_method jprog 
    in
    (* first scope class *)
    jprog.scope_class <- jprog.public_class;
    (* the main method *)
    AST.print_method "" startpoint;
    (* add the main mathods scope to the stack *)
    Stack.push (get_new_scope startpoint.mname) jprog.jvmstack;
    Log.debug false "### Running ... ###";
    (* print_scope jprog; *)
    (* run the program *)
    let exitval = execute_method jprog startpoint []
	in
	
    print_heap jprog
