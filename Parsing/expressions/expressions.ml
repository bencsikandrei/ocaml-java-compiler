open Ast
open Definitions
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

type enhanced_for =
	| Enhanced_for of modifier list option * types * string

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
	| EX_Primitive of primTypes * int option
	| EX_Cast of expression * expression 
	| EX_Class of expression * int 
	| EX_Ternary of expression * expression * expression
	| EX_Case of expression
	| EX_Default
	| EX_Array_access of expression * expression
	| EX_Field_access of expression * expression option
	| EX_Method_access of expression * expression list
	| EX_Array_alloc of types * expression list option * int option
	| EX_Plain_array_alloc of expression * expression list
	| EX_Plain_class_alloc of expression * insideClass list
	| EX_Class_alloc of types * expression list option
	| EX_New_alloc of expression option * expression
	| EX_Var_decl of expression * expression list option
	| EX_Primary of primaryType
	| EX_QualifiedName of definedType list
	| EX_Field_decl of fieldDeclaration * int (* added this *)

and primaryType =
	| P_Qualified of definedType list
	| P_NotJustName of expression

(* equivalent insideClass *)
and fieldDeclaration = (* equivalent to insideClass *)
	| FF_JavaMethod of javaMethod (* HAVE TO USE DEFINITIONS HERE*)
	| FF_Var_decl of modifier list option * allTypes * expression list (* same as above *)
	| FF_Block of string option * statement

and catch_header =
	| Catch_header of types * expression (* changed from string to expression *)

and statement = 
	| ST_Empty 
	| ST_Block of statement list
	| ST_Label of string
	| ST_Expression of expression
	| ST_If of expression * statement * statement option
	| ST_Switch of expression * statement
	| ST_Case of expression list * statement
	| ST_While of expression * statement
	| ST_For of statement list * expression * statement list * statement
	| ST_Efor of enhanced_for * expression * statement 
	| ST_Do_while of statement list * expression
	| ST_Break of string
	| ST_Continue of string
	| ST_Return of expression
	| ST_Throw of expression
	| ST_Lvar_decl of expression
	| ST_Synch of expression * statement
	| ST_Try of statement * statement list * statement
	| ST_Catch of catch_header * statement
	| ST_Catches of statement list
	| ST_Finally of statement
	| ST_Assert of expression * expression option
	| ST_Var_decl of string option * allTypes * expression list
























(* return types of each defined parser*)

type abstractSyntaxTree = 
	| JML of javaMethod list
	| STR of string
	| STATE of statement
	| EXPR of expression















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
	| _ -> ST_Empty 

let int_of_option v =
	match v with
	| Some(v) -> v
	| _ -> 0


(* get arithmetic operations *)
(*
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
  
let string_of_enhanced_for ef =
	match ef with
	| Enhanced_for(ml,t,s) -> (Printing.print_list print_modif (list_of_option ml) " ")^(string_of_types t)^" "^s

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

let string_of_field field =
	match field with
	| _ -> "field TODO"
(*	| FF_JavaMethod of javaMethod (* HAVE TO USE DEFINITIONS HERE*)
	| FF_Var_decl of modifier list option * allTypes * expression list (* same as above *)
	| FF_Block of string option * statement
*)

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
	| EX_Primitive(p, so) -> (string_of_primitive p)^(string_of_int (int_of_option so))
	| EX_Cast(e1, e2) -> " ("^(string_of_exp e1)^") "^(string_of_exp e2)
	| EX_Class(e,i) -> (string_of_exp e)^(string_mul i "[]")
	| EX_Ternary(e1,e2,e3) -> (string_of_exp e1)^" ? "^(string_of_exp e2)^" : "^(string_of_exp e3)
	| EX_Case(e) -> "case "^(string_of_exp e)^":"
	| EX_Default -> "default:"
	| EX_Array_access(e1,e2) -> (string_of_exp e1)^"["^(string_of_exp e2)^"]"
	| EX_Field_access(e, eo) -> (string_of_exp e)^"."^(string_of_exp (exp_of_option eo))
	| EX_Method_access(e,el) -> (string_of_exp e)^"("^(String.concat "," (List.map string_of_exp el))^")"
	| EX_Array_alloc(t,elo, io) -> "new "^(string_of_types t)^(String.concat "" (List.map string_of_exp (list_of_option elo) ))^(string_of_int (int_of_option io))
	| EX_Plain_array_alloc(e,el) -> (string_of_exp e)^"{"^(String.concat "," (List.map string_of_exp el))^"}"
	| EX_Plain_array_alloc(e,el) -> "inside class print"
	| EX_Class_alloc(t,elo) -> "new "^(string_of_types t)^"("^(String.concat "," (List.map string_of_exp (list_of_option elo) ))^")"
	| EX_New_alloc(so, e) -> (string_of_exp (exp_of_option so))^"."^(string_of_exp e) (* dot optional *)
	| EX_Var_decl(e, elo) -> (string_of_exp e)^" = "^(String.concat "," (List.map string_of_exp (list_of_option elo))) (* assign optional *)
	| EX_Primary(pt) -> "PLACEHOLDER ! primary expression -> primaryType"
	| EX_QualifiedName(ldt) -> "PLACEHOLDER ! qual name expression -> definedType list"
	| EX_Field_decl(fd,i) -> (string_of_field fd)^(string_mul i ";") 

and string_of_primaryType pt =
	match pt with
	| P_Qualified(dtl) ->  Printing.print_list string_of_definedType dtl ""
	| P_NotJustName(e) -> string_of_exp e

let string_of_catch_header ch =
	match ch with 
	| Catch_header(t,s) -> (string_of_types t)^" "^(string_of_exp s)

let rec string_of_stmt =
	function
	| ST_Empty -> "/* ST_empty */"
	| ST_Label x -> "/* ST_label */\n"^x
	| ST_Block(stl) -> (String.concat "/* ST_block */\n" (List.map string_of_stmt stl))
	| ST_Expression e -> "/* ST_expression */\n"^string_of_exp e
	| ST_If(e, st1, st2) -> "/* ST_if */\nif ("^(string_of_exp e)^") {"^(string_of_stmt st1)^"}"^(string_of_stmt (stms_of_option st2))
	| ST_Switch(e, sb) -> "/* ST_switch */\nswitch ("^(string_of_exp e)^") "^(string_of_stmt sb)
	| ST_While(e, st) ->  "/* ST_while */\nwhile ("^(string_of_exp e)^") {"^(string_of_stmt st)^"}"
	| ST_Case(el, st) -> (String.concat ", " (List.map string_of_exp el))^(string_of_stmt st) 
	| ST_For(e1, e2, e3, st) -> "/* ST_for */\nfor ("^(String.concat "; " (List.map string_of_stmt e1))^" "^(string_of_exp e2)^"; "^(String.concat "; " (List.map string_of_stmt e3))^")"^(string_of_stmt st)
	| ST_Efor(ef,e,s) -> "/* ST_efor */\nfor("^(string_of_enhanced_for ef)^" : "^(string_of_exp e)^") "^(string_of_stmt s)
	| ST_Do_while(st, e) -> "/* ST_do_while */\ndo {"^(String.concat "; " (List.map string_of_stmt st))^"} while ("^(string_of_exp e)^");"
	| ST_Break(e) -> "/* ST_break */\nbreak "^e
	| ST_Continue(e) -> "/* ST_continue */\ncontinue "^e
	| ST_Return(e) -> "/* ST_return */\nreturn "^(string_of_exp e)
	| ST_Throw(e) -> "/* ST_throw */\nthrow "^(string_of_exp e)
	| ST_Lvar_decl(e) -> "/* ST_lvar_decl */\n"^(string_of_exp e)
	| ST_Synch(e1,e2) -> "/* ST_synch */\nsynchronized "^(string_of_exp e1)^" : "^(string_of_stmt e2)
	| ST_Try(st1,stl,st2) ->  "/* ST_try */\ntry {"^(string_of_stmt st1)^(String.concat "; " (List.map string_of_stmt stl))^(string_of_stmt st2)^"}"
	| ST_Catch(ch, st) ->  "/* ST_catch */\ncatch ("^(string_of_catch_header ch)^")"^(string_of_stmt st)
	| ST_Catches(stl) -> "/* ST_catches */\n"^(String.concat "; " (List.map string_of_stmt stl))
	| ST_Finally(st) -> "/* ST_finally */\nfinally "^(string_of_stmt st)
	| ST_Assert(e1,e2) -> "/* ST_assert */\nassert ("^(string_of_exp e1)^") : ("^(string_of_exp(exp_of_option e2))^");"
	| ST_Var_decl(so,t, e) -> "/* ST_var_decl */\n"^(str_of_option so)^" "^(string_of_allTypes t)^" "^(String.concat ", " (List.map string_of_exp e))^";" 