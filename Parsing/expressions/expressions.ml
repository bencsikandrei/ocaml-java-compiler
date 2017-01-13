open Ast
open Printing
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

type literal =
	| L_Int of int
	| L_Str of string
	| L_Float of float
	| L_Double of float
	| L_Char of char
	| L_Boolean of bool
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
	| EX_Primitive of primTypes * string option
	| EX_Cast of expression * expression 
	| EX_Class of expression * int 
	| EX_Ternary of expression * expression * expression
	| EX_Case of expression
	| EX_Default
	| EX_Array_access of expression * expression
	| EX_Field_access of expression * expression option
	| EX_Method_access of expression * expression list
	| EX_Array_alloc of types * expression list option * string option
	| EX_Plain_array_alloc of expression * expression list
	| EX_Class_alloc of types * expression list option
	| EX_New_alloc of expression option * expression
	| EX_Var_decl of expression * expression list option
	| EX_Primary of primaryType
	| EX_QualifiedName of definedType list

and primaryType =
	| P_Qualified of definedType list
	| P_NotJustName of expression

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
	| ST_var_decl of string option * allTypes * expression list

(* extract from option *)
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
	| BO_Ge -> ">="
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

let string_of_catch_header ch =
	match ch with 
	| Catch_header(t,s) -> (string_of_types t)^" "^s
  
let string_of_enhanced_for ef =
	match ef with
	| Enhanced_for(t,s) -> (string_of_types t)^" "^s

let string_of_literal x =
 	match x with
	| L_Str(v)-> v
	| L_Float(v) ->  string_of_float v
	| L_Double(v) -> string_of_float v
	| L_Char(v) -> (String.make 1 v)
	| L_Boolean(v) -> string_of_bool v
 	| L_Int(v) -> string_of_int v
 	| L_Null -> "null"

let rec string_mul i s =
	match i with
	| 0 -> s
	| _ -> (string_mul (i-1) (s^s))

let rec string_of_exp exp =
	match exp with
	| Identifier id -> id
	| Literal lit -> string_of_literal lit
	| EX_Empty -> ""
	| EX_Binop(op, e1, e2) -> (string_of_exp e1)^(string_of_bo op)^(string_of_exp e2)
	| EX_Compop(op, e1, e2) -> (string_of_exp e1)^(string_of_compop op)^(string_of_exp e2)
	| EX_Instanceof(op, e1, t) -> (string_of_exp e1)^(string_of_compop op)^ (string_of_types t) (* allows instanceof generics*)
	| EX_Bitop(op, e1, e2)-> (string_of_exp e1)^(string_of_bitop op)^(string_of_exp e2)
	| EX_Logbinop(op, e1, e2) -> (string_of_exp e1)^(string_of_lbo op)^(string_of_exp e2)
	| EX_Loguop(op, e) -> (string_of_luo op)^(string_of_exp e)
	| EX_Unop(op, e) -> (string_of_uo op)^(string_of_exp e)
	| EX_Postfix(op, e) -> (string_of_exp e)^(string_of_uo op)
	| EX_Assign(op, e1, e2) -> (string_of_exp e1)^(string_of_assign op)^(string_of_exp e2)
	| EX_Primitive(p, so) -> (string_of_primitive p)^(str_of_option so)
	| EX_Cast(e1, e2) -> " ("^(string_of_exp e1)^") "^(string_of_exp e2)
	| EX_Class(e,i) -> (string_of_exp e)^(string_mul i "[]")
	| EX_Ternary(e1,e2,e3) -> (string_of_exp e1)^" ? "^(string_of_exp e2)^" : "^(string_of_exp e3)
	| EX_Case(e) -> "case "^(string_of_exp e)^":"
	| EX_Default -> "default:"
	| EX_Array_access(e1,e2) -> (string_of_exp e1)^"["^(string_of_exp e2)^"]"
	| EX_Field_access(e, eo) -> (string_of_exp e)^"."^(string_of_exp (exp_of_option eo))
	| EX_Method_access(e,el) -> (string_of_exp e)^"("^(String.concat "," (List.map string_of_exp el))^")"
	| EX_Array_alloc(t,elo, so) -> "new "^(string_of_types t)^(String.concat "" (List.map string_of_exp (list_of_option elo) ))^(str_of_option so)
	| EX_Plain_array_alloc(e,el) -> (string_of_exp e)^"{"^(String.concat "," (List.map string_of_exp el))^"}"
	| EX_Class_alloc(t,elo) -> "new "^(string_of_types t)^"("^(String.concat "," (List.map string_of_exp (list_of_option elo) ))^")"
	| EX_New_alloc(so, e) -> (string_of_exp (exp_of_option so))^"."^(string_of_exp e) (* dot optional *)
	| EX_Var_decl(e, elo) -> (string_of_exp e)^" = "^(String.concat "," (List.map string_of_exp (list_of_option elo))) (* assign optional *)
  
let rec string_of_stmt =
	function
	| ST_empty -> "/* ST_empty */"
	| ST_label x -> "/* ST_label */\n"^x
	| ST_block(stl) -> (String.concat "/* ST_block */\n" (List.map string_of_stmt stl))
	| ST_expression e -> "/* ST_expression */\n"^string_of_exp e
	| ST_if(e, st1, st2) -> "/* ST_if */\nif ("^(string_of_exp e)^") {"^(string_of_stmt st1)^"}"^(string_of_stmt (stms_of_option st2))
	| ST_switch(e, sb) -> "/* ST_switch */\nswitch ("^(string_of_exp e)^") "^(string_of_stmt sb)
	| ST_while(e, st) ->  "/* ST_while */\nwhile ("^(string_of_exp e)^") {"^(string_of_stmt st)^"}"
	| ST_case(el, st) -> (String.concat ", " (List.map string_of_exp el))^(string_of_stmt st) 
	| ST_for(e1, e2, e3, st) -> "/* ST_for */\nfor ("^(String.concat "; " (List.map string_of_stmt e1))^" "^(string_of_exp e2)^"; "^(String.concat "; " (List.map string_of_stmt e3))^")"^(string_of_stmt st)
	| ST_efor(ef,e,s) -> "/* ST_efor */\nfor("^(string_of_enhanced_for ef)^" : "^(string_of_exp e)^") "^(string_of_stmt s)
	| ST_do_while(st, e) -> "/* ST_do_while */\ndo {"^(String.concat "; " (List.map string_of_stmt st))^"} while ("^(string_of_exp e)^");"
	| ST_break(e) -> "/* ST_break */\nbreak "^e
	| ST_continue(e) -> "/* ST_continue */\ncontinue "^e
	| ST_return(e) -> "/* ST_return */\nreturn "^(string_of_exp e)
	| ST_throw(e) -> "/* ST_throw */\nthrow "^(string_of_exp e)
	| ST_lvar_decl(e) -> "/* ST_lvar_decl */\n"^(string_of_exp e)
	| ST_synch(e1,e2) -> "/* ST_synch */\nsynchronized "^(string_of_exp e1)^" : "^(string_of_stmt e2)
	| ST_try(st1,stl,st2) ->  "/* ST_try */\ntry {"^(string_of_stmt st1)^(String.concat "; " (List.map string_of_stmt stl))^(string_of_stmt st2)^"}"
	| ST_catch(ch, st) ->  "/* ST_catch */\ncatch ("^(string_of_catch_header ch)^")"^(string_of_stmt st)
	| ST_catches(stl) -> "/* ST_catches */\n"^(String.concat "; " (List.map string_of_stmt stl))
	| ST_finally(st) -> "/* ST_finally */\nfinally "^(string_of_stmt st)
	| ST_assert(e1,e2) -> "/* ST_assert */\nassert ("^(string_of_exp e1)^") : ("^(string_of_exp(exp_of_option e2))^");"
	| ST_var_decl(so,t, e) -> "/* ST_var_decl */\n"^(str_of_option so)^" "^(string_of_allTypes t)^" "^(String.concat ", " (List.map string_of_exp e))^";"