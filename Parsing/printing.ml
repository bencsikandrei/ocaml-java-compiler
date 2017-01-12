open String
open Char
open Ast
let indent var =
	let size = length var in 
		let rec iterate pos=
			if pos=size then 
				""
			else
				if (String.get var pos)='\n' then 
					"\n\t"^(iterate (pos+1)) 
				else 
					(Char.escaped (String.get var pos))^(iterate (pos+1))
		in
			"\t"^iterate 0;;

let rec print_list print_f  list separator = match list with
	| [] ->  ""
	| head::tail -> (print_f head)^separator^(print_list print_f tail separator);;


let string_of_primitive var = match var with
	| PT_Float -> "float"
	| PT_Boolean ->  "bool"
	| PT_Byte ->  "byte"
	| PT_Char ->  "char"
	| PT_Int ->  "int"
	| PT_Long ->  "long"
	| PT_Short ->  "short"
	| PT_Double ->  "double"

let rec string_of_allTypes var = match var with
	| AL_Types a -> string_of_types a
	| AL_Array (a,dim) -> "["^(string_of_types a)^"]dims="^(string_of_int dim)
and string_of_types var = match var with
	| T_Primitive v -> string_of_primitive v
	| T_Qualified v -> print_list string_of_definedType v "-"
and string_of_definedType var = match var with
	| DT_Id id -> id
	| DT_Generic(t,al) -> t^"<"^(string_of_allTypes al)^">"