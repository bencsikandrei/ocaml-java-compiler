{
	(* ocaml code *)
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let integer = (digit)+
let long = (digit)+['l''L']
let double = (digit)+('.' digit*)?
let float = (digit)+('.' digit*)?['f''F']
let space = [' ' '\t' '\r\n' '\n']
let identifier = ('_'|'$'|letter)(letter|digit|'_')* 

rule nexttoken = parse 
	| space+ { nexttoken lexbuf }
	| eof { EOF }
	| '+' { PLUS }
	| '-' { MINUS }
	| '/' { DIV } 
	| '*' { MUL }
	| '%' { MOD }
	| integer as i { INT(int_of_string i) }
	| double as d { DOUBLE(float_of_string d) }
	| identifier as id { IDENTIFIER id }
{

	

}
