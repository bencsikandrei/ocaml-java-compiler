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
	| LBO_and (* && *)
	| LBO_or

type loguop =
	| LUO_Not
	| UO_BNot

(* any type ops *)
type compop =
	| BO_gt
	| BO_lt
	| BO_ge
	| BO_le 
	| BO_neq
	| BO_eq
	| BO_instanceof

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
	| EX_Binop of binop * expression * expression
	| EX_Compop of compop * expression * expression
	| EX_Instanceof of compop * expression * types
	| EX_Bitop of bitop * expression * expression
	| EX_Logbinop of logbinop * expression * expression
	| EX_Loguop of loguop * expression
	| EX_Unop of unop * expression
	| EX_Postfix of unop * expression
	| EX_Assign of assign * expression * expression	
	| EX_Primitive of primitive * string
	| EX_Cast of expression * expression 
	| EX_Class of string 
	| EX_Ternary of expression * expression * expression

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
	| ST_while of expression * statement
	| ST_for of expression list * expression * statement list * statement
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

and switch_block =
	| Switch_block of case_block list
 	| Empty

and case_block =
	| Case_block of label list *statement 
	
and label =
	| Case of expression 
	(* | Cases of label list *)
	| Default

(* option removed; TODO revise statement *)

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
(*
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
	| LBO_or -> "||"
	| LBO_and -> "&&"

let string_of_luo = function
	| LUO_Not -> "!"
	| UO_BNot -> "~"

let string_of_compop = function
	| BO_gt -> ">"
	| BO_lt -> "<"
	| BO_ge -> ">="
	| BO_le  -> "<="
	| BO_neq -> "!="
	| BO_eq -> "==" 
	| BO_instanceof -> "instanceof"

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
	| EX_Binop(op, e1, e2) -> (string_of_exp e1)^(string_of_bo op)^(string_of_exp e2)
	| EX_Compop(op, e1, e2) -> (string_of_exp e1)^(string_of_compop op)^(string_of_exp e2)
	| EX_Bitop(op, e1, e2)-> (string_of_exp e1)^(string_of_bitop op)^(string_of_exp e2)
	| EX_Logbinop(op, e1, e2) -> (string_of_exp e1)^(string_of_lbo op)^(string_of_exp e2)
	| EX_Loguop(op, e) -> (string_of_luo op)^(string_of_exp e)
	| EX_Unop(op, e) -> (string_of_uo op)^(string_of_exp e)
	| EX_Postfix(op, e) -> (string_of_exp e)^(string_of_uo op)
	| EX_Assign(op, e1, e2) -> (string_of_exp e1)^(string_of_assign op)^(string_of_exp e2)
	| EX_Cast(e1, e2) -> " ("^(string_of_exp e1)^") "^(string_of_exp e2)
	| EX_Class(s) -> s
	| EX_Primitive(p, s) -> (string_of_primitive p)^" "^s
	| EX_Ternary(e1, e2, e3) -> (string_of_exp e1)^" ? "^(string_of_exp e2)^" : "^(string_of_exp e3) 
	| EX_Instanceof(op, e1, e2) -> (string_of_exp e1)^(string_of_compop op)^(string_of_types e2)

let exp_of_option e =
	match e with
	| Some(ex) -> ex
	| _ -> Identifier("")

let rec string_of_stmt =
	function
	| ST_empty -> ";"
	| ST_label x -> x
	| ST_expression e -> string_of_exp e
	| ST_if(e, st1, st2) -> "if ("^(string_of_exp e)^") {"^(String.concat "; " (List.map string_of_stmt st1))^(string_of_stmt st2)^"}"
	| ST_switch(e, st) -> "switch ("^(string_of_exp e)^") "^(String.concat "; " (List.map string_of_stmt st))
	| ST_while(e, st) ->  "while ("^(string_of_exp e)^") {"^(String.concat "; " (List.map string_of_stmt st))^"}"
	| ST_for(e1, e2, e3, st) -> "for ("^(String.concat "; " (List.map string_of_exp e1))^"; "^(string_of_exp e2)^"; "^(String.concat "; " (List.map string_of_exp e3))^")"^(String.concat "; " (List.map string_of_stmt st))
	| ST_do_while(st, e) -> "do {"^(String.concat "; " (List.map string_of_stmt st))^"} while ("^(string_of_exp e)^")"
	| ST_break(e) -> "break "^(string_of_exp e)
	| ST_continue(e) -> "continue "^(string_of_exp e)
	| ST_return(e) -> "return "^(string_of_exp e)
	| ST_throw(e) -> "throw "^(string_of_exp e)
	| ST_lvar_decl(e) -> (string_of_exp e)
	| ST_synch(e1,e2) -> "synchronized "^(string_of_exp e)^" : "^(string_of_exp (exp_of_option e))
	| ST_try(st,) ->  "try {"^(String.concat "; " (List.map string_of_stmt st))^"}"
	| ST_catch(e, st) ->  "catch ("^(string_of_types e)^")"^(string_of_block st)
	| ST_finally(st) -> "finally "^(string_of_block st)

and string_of_block  = 
	function
	| BL_NonEmptyBlock(st) -> "{"^(String.concat "; " (List.map string_of_stmt st))^"}"
	| BL_Empty -> "{ }"
*)