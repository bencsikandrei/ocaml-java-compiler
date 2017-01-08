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
	| UO_PreIncrement
	| UO_PreDecrement
	| UO_BNot

(* logical ops *)
type logbinop =
	| LBO_and (* && *)
	| LBO_or

type loguop =
	| LUO_Not

(* any type ops *)
type compop =
	| BO_gt
	| BO_lt
	| BO_ge
	| BO_le 
	| BO_neq
	| BO_eq

(* bitwise ops *)
type bitop =
	| SO_lshift 
	| SO_rshift 
	| SO_logshift
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

type primitive =
  	| Int 
  	| Float 
  	| Double 
  	| Char 
  	| Boolean 
  	| Byte 
  	| Short 
  	| Long

type literal =
	| L_Int of int
	| L_Str of string
	| L_Float of float
	| L_Double of float
	| L_Char of char
	| L_Boolean of bool
	| L_Null of string

type expression =
	| Identifier of string
	| Literal of literal
	| Binop of binop * expression * expression
	| Compop of compop * expression * expression
	| Bitop of bitop * expression * expression
	| Logbinop of logbinop * expression * expression
	| Loguop of loguop * expression
	| Unop of unop * expression
	| Assign of assign * expression * expression	

type statement = 
	| ST_empty_stmt 
	| ST_label_stmt of string
	| ST_if_stmt of expression * statement * statement option
	| ST_switch_stmt of expression * statement list
	| ST_while_stmt of expression * statement
	| ST_for_stmt of expression list * expression option * expression list * statement
	| ST_do_while_stmt
	| ST_break_stmt
	| ST_continue_stmt
	| ST_return_stmt
	| ST_throw_stmt
	| ST_lvar_decl_stmt
	| ST_synch_stmt
	| ST_try_stmt
	| ST_catch_stmt
	| ST_finally_stmt

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
	| UO_PreIncrement -> fun x -> pred x 
	| UO_PreDecrement -> fun x -> succ x
	| UO_BNot -> fun x -> lnot x

(* get logical ops *)
let get_lbo op a b =
	match op with
	| LBO_and -> a && b
	| LBO_or -> a || b

let get_luo = function
	| LUO_Not -> fun x -> not x

(* get any type ops *)
let get_compop op x y =
	match op with
	| BO_gt -> x > y
	| BO_lt -> x < y
	| BO_ge -> x >= y
	| BO_le -> x <= y
	| BO_neq -> x != y
	| BO_eq -> x == y

(* get bitwise ops *)
let get_bitop op x y =
	match op with
	| SO_lshift -> x lsl y
	| SO_rshift -> x lsr y
	| SO_logshift -> x asr y
	| SO_And -> x land y
	| SO_Or -> x lor y
	| SO_Xor -> x lxor y

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
	| UO_PreIncrement -> "++"
	| UO_PreDecrement -> "--"
	| UO_BNot -> "~"

let string_of_lbo = function
	| LBO_or -> "||"
	| LBO_and -> "&&"

let string_of_luo = function
	| LUO_Not -> "!"

let string_of_compop = function
	| BO_gt -> ">"
	| BO_lt -> "<"
	| BO_ge -> ">="
	| BO_le  -> "<="
	| BO_neq -> "!="
	| BO_eq -> "==" 

let string_of_bitop = function
	| SO_lshift -> "<<"
	| SO_rshift -> ">>"
	| SO_logshift-> ">>>"
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
  	| Int -> "int"
	| Float -> "float"
	| Double -> "double"
  	| Boolean -> "boolean"
  	| Char -> "char"
  	| Long -> "long"
  	| Byte -> "byte"
  	| Short -> "short"

 let string_of_literal x =
 	match x with
	| L_Str x -> x
	| L_Float x -> string_of_float x
	| L_Double x -> string_of_float x
	| L_Char x -> String.make 1 x
	| L_Boolean x -> string_of_bool x
 	| L_Int x -> string_of_int x
 	| L_Null x -> "null"

let rec string_of_exp exp =
	match exp with
	| Identifier id -> id
	| Literal lit -> string_of_literal lit
	| Binop(op, e1, e2) -> (string_of_exp e1)^(string_of_bo op)^(string_of_exp e2)
	| Compop(op, e1, e2) -> (string_of_exp e1)^(string_of_compop op)^(string_of_exp e2)
	| Bitop(op, e1, e2)-> (string_of_exp e1)^(string_of_bitop op)^(string_of_exp e2)
	| Logbinop(op, e1, e2) -> (string_of_exp e1)^(string_of_lbo op)^(string_of_exp e2)
	| Loguop(op, e) -> (string_of_luo op)^(string_of_exp e)
	| Unop(op, e) -> (string_of_uo op)^(string_of_exp e)
	| Assign(op, e1, e2) -> (string_of_exp e1)^(string_of_assign op)^(string_of_exp e2)

(*
(* evaluate *)
let rec eval exp =
	match exp with
	| L_Int ilit -> ilit
	| Binop(op, e1, e2) -> (get_bo_int op) (eval e1) (eval e2)
	| Unop(op, e) -> (get_uo op) (eval e)
*)