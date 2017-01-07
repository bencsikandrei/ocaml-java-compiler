(* operators *)
type compop =
	| BO_or 
	| BO_and
	| BO_gt
	| BO_lt
	| BO_ge
	| BO_le 
	| BO_neq
	| BO_eq


type shiftop =
	| SO_lshift 
	| SO_rshift 
	| SO_logshift

(* types *)
type primitive =
	| Int 
	| Float 
	| Double 
	| Char 
	| Boolean 
	| Byte 
	| Short 
	| Long

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

(* to string *)
let string_of_compop = function
	| BO_or -> "||"
	| BO_and -> "&&"
	| BO_gt -> ">"
	| BO_lt -> "<"
	| BO_ge -> ">="
	| BO_le  -> "<="
	| BO_neq -> "!="
	| BO_eq -> "==" 

let string_of_shiftop = function
	| SO_lshift -> "<<"
	| SO_rshift -> ">>"
	| SO_logshift-> ">>>"

let string_of_primitive = function
  | Int -> "int"
  | Float -> "float"
  | Double -> "double"
  | Boolean -> "boolean"
  | Char -> "Char"
  | Long -> "long"
  | Byte -> "byte"
  | Short -> "short"


