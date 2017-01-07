
type binop =
	| BO_Add
	| BO_Minus
	| BO_Mul
	| BO_Div
	| BO_Mod

type unop =
	| UO_Plus
	| UO_Minus

type expression =
	(*STRLIT of string
	| DOUBLELIT of float
	| FLOATLIT of float
	| CHARLIT of char
	| BOOLEANLIT of bool
	| NULLLIT of string*)
	| Literal of int
	| Binop of binop * expression * expression
	| Unop of unop * expression
(* to add variables later *)

(* get operations *)
let get_bo op x y = 
	match op with
	| BO_Add -> x + y
	| BO_Minus -> x - y
	| BO_Mul -> x * y
	| BO_Div -> x / y
	| BO_Mod -> x mod y

let get_uo = function
	| UO_Plus -> fun x -> x
	| UO_Minus -> fun x -> -x

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

(* evaluate *)
let rec eval exp =
	match exp with
	| Literal ilit -> ilit
	| Binop(op, e1, e2) -> (get_bo op) (eval e1) (eval e2)
	| Unop(op, e) -> (get_uo op) (eval e)

(* string *)
let rec string_of_exp exp =
	match exp with
	| Literal ilit -> string_of_int ilit
	| Binop(op, e1, e2) -> (string_of_exp e1)^(string_of_bo op)^(string_of_exp e2)
	| Unop(op, e) -> (string_of_uo op)^(string_of_exp e)
