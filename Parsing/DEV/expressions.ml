exception JavaException of string

(* arithmetic ops *)
type binop =
	| BO_Add
	| BO_Minus
	| BO_Mul
	| BO_Div
	| BO_Mod

type unop =
	| UO_Plus
	| UO_Minus
	| UO_Increment
	| UO_Decrement

(* logical ops *)
type logbinop =
	| LBO_And (* && *)
	| LBO_Or

type loguop =
	| LUO_Not
	| UO_BNot

(* any type ops *)
type compop =
	| BO_Gt
	| BO_Lt
	| BO_Ge
	| BO_Le 
	| BO_Neq
	| BO_Eq
	| BO_instanceof

(* bitwise ops *)
type bitop =
	| SO_Lshift 
	| SO_Rshift 
	| SO_Logshift
	| SO_And (* & *)
	| SO_Or
	| SO_Xor

type assign =
	| ASS_Equal
	| ASS_Plus
	| ASS_Minus
	| ASS_Mul
	| ASS_Div
	| ASS_Mod
	| ASS_Xor
	| ASS_And
	| ASS_Or
	| ASS_RShift
	| ASS_LShift
	| ASS_LogShift

type types =
	| Primitive of primitive
	| ArrayType of types * string
	| Qualified of string

and primitive =
  	| P_Int 
  	| P_Float 
  	| P_Double 
  	| P_Char 
  	| P_Boolean 
  	| P_Byte 
  	| P_Short 
  	| P_Long
  	| P_Void

type literal =
	| L_Int
	| L_Str
	| L_Float
	| L_Double
	| L_Char
	| L_Boolean
	| L_Null

type expression =
	| Identifier of string
	| Literal of literal
	| EX_Empty
	| EX_Binop of binop * expression * expression
	| EX_Compop of compop * expression * expression
	| EX_Instanceof of compop * expression * types
	| EX_Bitop of bitop * expression * expression
	| EX_Logbinop of logbinop * expression * expression
	| EX_Loguop of loguop * expression
	| EX_Unop of unop * expression
	| EX_Postfix of unop * expression
	| EX_Assign of assign * expression * expression	
	| EX_Primitive of primitive * string option
	| EX_Cast of expression * expression 
	| EX_Class of string 
	| EX_Ternary of expression * expression * expression
	| EX_Case of expression
	| EX_Default
	| EX_Array_access of expression * expression
	| EX_Field_access of expression * expression option
	| EX_Method_access of expression * expression list
	| EX_Array_alloc of types * expression list option * string option
	| EX_Plain_array_alloc of expression * expression list
	| EX_Class_alloc of types * expression list option
	| EX_New_alloc of string option * expression
	| EX_Var_decl of expression * expression list option

type catch_header =
	| Catch_header of types * string

type enhanced_for =
	| Enhanced_for of types * string

type statement = 
	| ST_empty 
	| ST_block of statement list
	| ST_label of string
	| ST_expression of expression
	| ST_if of expression * statement * statement option
	| ST_switch of expression * statement
	| ST_case of expression list * statement
	| ST_while of expression * statement
	| ST_for of statement list * expression * statement list * statement
	| ST_efor of enhanced_for * expression * statement 
	| ST_do_while of statement list * expression
	| ST_break of string
	| ST_continue of string
	| ST_return of expression
	| ST_throw of expression
	| ST_lvar_decl of expression
	| ST_synch of expression * statement
	| ST_try of statement * statement list * statement
	| ST_catch of catch_header * statement
	| ST_catches of statement list
	| ST_finally of statement
	| ST_assert of expression * expression option
	| ST_var_decl of string option * types * expression list
(*
and switch_block =
	| Switch_block of case_block list
 	| Empty

and case_block =
	| Case_block of label list *statement 
	
and label =
	| Case of expression 
	(* | Cases of label list *)
	| Default
*)
(* option removed; TODO revise statement *)
(*
(* get arithmetic operations *)
let get_bo_int op x y = 
	match op with
	| BO_Add -> x + y
	| BO_Minus -> x - y
	| BO_Mul -> x * y
	| BO_Div -> x / y
	| BO_Mod -> x mod y

let get_bo_float op x y = 
	match op with
	| BO_Add -> x +. y
	| BO_Minus -> x -. y
	| BO_Mul -> x *. y
	| BO_Div -> x /. y
	| BO_Mod -> mod_float x y

let get_uo = function
	| UO_Plus -> fun x -> x
	| UO_Minus -> fun x -> -x
	| UO_Increment -> fun x -> pred x 
	| UO_Decrement -> fun x -> succ x

(* get logical ops *)
let get_lbo op a b =
	match op with
	| LBO_And -> a && b
	| LBO_Or -> a || b

let get_luo = function
	| LUO_Not -> fun x -> not x

(* get any type ops *)
let get_compop op x y =
	match op with
	| BO_Gt -> x > y
	| BO_Lt -> x < y
	| BO_Ge -> x >= y
	| BO_Le -> x <= y
	| BO_Neq -> x != y
	| BO_Eq -> x == y

(* get bitwise ops *)
let get_bitop op x y =
	match op with
	| SO_Lshift -> x lsl y
	| SO_Rshift -> x lsr y
	| SO_Logshift -> x asr y
	| SO_And -> x land y
	| SO_Or -> x lor y
	| SO_Xor -> x lxor y
*)
(* get assign TODO *)

(* get string of operations *)
let string_of_bo = function
	| BO_Add -> "+"
	| BO_Minus -> "-"
	| BO_Mul -> "*"
	| BO_Div -> "/"
	| BO_Mod -> "%"

let string_of_uo = function
	| UO_Plus -> "+"
	| UO_Minus -> "-"
	| UO_Increment -> "++"
	| UO_Decrement -> "--"

let string_of_lbo = function
	| LBO_Or -> "||"
	| LBO_And -> "&&"

let string_of_luo = function
	| LUO_Not -> "!"
	| UO_BNot -> "~"

let string_of_compop = function
	| BO_Gt -> ">"
	| BO_Lt -> "<"
	| BO_Gt -> ">="
	| BO_Le  -> "<="
	| BO_Neq -> "!="
	| BO_Eq -> "==" 
	| BO_instanceof -> "instanceof"

let string_of_bitop = function
	| SO_Lshift -> "<<"
	| SO_Rshift -> ">>"
	| SO_Logshift-> ">>>"
	| SO_And -> "&"
	| SO_Or -> "|"
	| SO_Xor -> "^"

let string_of_assign = function
	| ASS_Equal -> "="
	| ASS_Plus -> "+="
	| ASS_Minus -> "-="
	| ASS_Mul -> "*="
	| ASS_Div -> "/="
	| ASS_Mod -> "%="
	| ASS_Xor -> "^="
	| ASS_And -> "&="
	| ASS_Or -> "|="
	| ASS_RShift -> ">>="
	| ASS_LShift -> "<<="
	| ASS_LogShift -> ">>>="

let string_of_primitive = function
  	| P_Int -> "int"
	| P_Float -> "float"
	| P_Double -> "double"
  	| P_Boolean -> "boolean"
  	| P_Char -> "char"
  	| P_Long -> "long"
  	| P_Byte -> "byte"
  	| P_Short -> "short"
  	| P_Void -> "void"

let rec string_of_types = function
	| Primitive(p) -> (string_of_primitive p)
	| ArrayType(t, s) -> (string_of_types t) ^ s
	| Qualified(qn) -> qn

 let string_of_literal x =
 	match x with
	| L_Str -> "string"
	| L_Float ->  "float"
	| L_Double -> "double"
	| L_Char -> "char" 
	| L_Boolean -> "boolean"
 	| L_Int -> "int"
 	| L_Null -> "null"


let rec string_of_exp exp =
	match exp with
	| Identifier id -> id
	| Literal lit -> string_of_literal lit
	| EX_Empty -> ""
	| EX_Binop(op, e1, e2) -> (string_of_exp e1)^(string_of_bo op)^(string_of_exp e2)
	| EX_Compop(op, e1, e2) -> (string_of_exp e1)^(string_of_compop op)^(string_of_exp e2)
	| EX_Instanceof(op, e1, e2) -> (string_of_exp e1)^(string_of_compop op)^ (string_of_types types)
	| EX_Bitop(op, e1, e2)-> (string_of_exp e1)^(string_of_bitop op)^(string_of_exp e2)
	| EX_Logbinop(op, e1, e2) -> (string_of_exp e1)^(string_of_lbo op)^(string_of_exp e2)
	| EX_Loguop(op, e) -> (string_of_luo op)^(string_of_exp e)
	| EX_Unop(op, e) -> (string_of_uo op)^(string_of_exp e)
	| EX_Postfix(op, e) -> (string_of_exp e)^(string_of_uo op)
	| EX_Assign(op, e1, e2) -> (string_of_exp e1)^(string_of_assign op)^(string_of_exp e2)
	| EX_Primitive(p, so) -> (string_of_primitive p)^(str_of_option so)
	| EX_Cast(e1, e2) -> " ("^(string_of_exp e1)^") "^(string_of_exp e2)
	| EX_Class(s) -> s
	| EX_Ternary(e1,e2,e3) -> (string_of_exp e1)^" ? "^(string_of_exp e2)^" : "^(string_of_exp e3)
	| EX_Case(e) -> "case "^(string_of_exp e)^":"
	| EX_Default -> "default:"
	| EX_Array_access(e1,e2) -> (string_of_exp e1)^"["^(string_of_exp e2)^"]"
	| EX_Field_access(e, eo) -> (string_of_exp e)^"."^(exp_of_option eo)
	| EX_Method_access(e,el) -> (string_of_exp e)^"("^(String.concat "," (List.map string_of_exp el))^")"
	| EX_Array_alloc(t,elo, so) -> "new "^(string_of_types types)^(String.concat "" (List.map(list_of_option elo)))^(str_of_option so)
	| EX_Plain_array_alloc(e,el) -> (string_of_exp e)^"{"^(String.concat "," (List.map string_of_exp el))^"}"
	| EX_Class_alloc(t,elo) -> "new "^(string_of_types types)^"("^(String.concat "," (List.map(list_of_option elo)))^")"
	| EX_New_alloc(so, e) -> (str_of_option so)^"."^(string_of_exp e) (* dot optional *)
	| EX_Var_decl(e, elo) -> (string_of_exp e)^" = "^(String.concat "," (List.map (list_of_option elo))) (* assign optional *)
	| _ -> ""

let str_of_option e =
	match e with
	| Some(ex) -> ex
	| _ -> ""

let list_of_option l =
  match l with
  | Some(x) -> x
  | _ -> []

let exp_of_option e =
	match e with
	| Some(ex) -> ex
	| _ -> Identifier("")

let stms_of_option s =
  match s with
  | Some(s) -> s
  | _ -> ST_empty 

let string_of_catch_header ch =
  | Catch_header(t,s) -> (string_of_types t)^" "^s
  | _ -> ""
  
let string_of_enhanced_for ef =
  | Enhanced_for(t,s) -> (string_of_types t)^" "^s
  | _ -> ""
  
let rec string_of_stmt =
	function
	| ST_empty -> ";"
	| ST_label x -> x
	| ST_block(stl) -> (String.concat ";\n" (List.map string_of_stmt stl))
	| ST_expression e -> string_of_exp e
	| ST_if(e, st1, st2) -> "if ("^(string_of_exp e)^") {"^(string_of_stmt st1)^"}"^(string_of_stmt (stmt_of_option st2))
	| ST_switch(e, st) -> "switch ("^(string_of_exp e)^") "^(string_of_stmt st)
	| ST_while(e, st) ->  "while ("^(string_of_exp e)^") {"^(string_of_stmt st)^"}"
	| ST_for(e1, e2, e3, st) -> "for ("^(String.concat "; " (List.map string_of_stmt e1))^"; "^(string_of_exp e2)^"; "^(String.concat "; " (List.map string_of_stmt e3))^")"^(string_of_stmt st)
	| ST_efor(ef,e,s) -> "for("^(string_of_efor ef)^" : "^(string_of_exp e)^") "^(string_of_stmt s)
	| ST_do_while(st, e) -> "do {"^(String.concat "; " (List.map string_of_stmt st))^"} while ("^(string_of_exp e)^")"
	| ST_break(e) -> "break "^e
	| ST_continue(e) -> "continue "^e
	| ST_return(e) -> "return "^(string_of_exp e)
	| ST_throw(e) -> "throw "^(string_of_exp e)
	| ST_lvar_decl(e) -> (string_of_exp e)
	| ST_synch(e1,e2) -> "synchronized "^(string_of_exp e1)^" : "^(string_of_stmt e2))
	| ST_try(st1,stl,st2) ->  "try {"^(string_of_stmt st1)^(String.concat "; " (List.map string_of_stmt stl))^(string_of_stmt st2)^"}"
	| ST_catch(ch, st) ->  "catch ("^(string_of_catch_header ch)^")"^(string_of_block st)
	| ST_catches(stl) -> (String.concat "; " (List.map string_of_stmt stl))
	| ST_finally(st) -> "finally "^(string_of_stmt st)
	| ST_assert(e1,e2) -> "assert "^(string_of_exp e1)^" : "^(string_of_exp(exp_of_option e2))^";"
	| ST_var_decl(so,t, e) -> (str_of_option so)^" "^(string_of_types t)^" = "^(List.map string_of_exp e))