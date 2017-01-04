{
	(* ocaml code *)
	open Parser
	open Printf
	open Lexing

	exception Syntax_error of string
	
	(* increment the line number *)
	let incr_lineno lexbuf =
		let pos = lexbuf.lex_curr_p in
		lexbuf.lex_curr_p <- 
			{ 
				pos with 
				pos_lnum = pos.pos_lnum + 1;
				pos_bol = pos.pos_cnum; 
			}
	(* use a hashtable to store keywords *)
	let create_hashtable size init = 
		let tbl = Hashtbl.create size in 
		List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
		tbl

	(* use a list to store the keywords *)
	let kw_table = 
		[
	 	 "abstract", ABSTRACT ;
		 "assert", ASSERT ;
		 "boolean", BOOLEAN ;
		 "break", BREAK ;
		 "byte", BYTE ;
		 "case", CASE ;
		 "catch", CATCH ;
		 "char", CHAR ;
		 "class", CLASS ;
		 "const", CONST ;
		 "continue", CONTINUE ;
		 "default", DEFAULT ;
		 "do", DO ;
		 "double", DOUBLE ;
		 "else", ELSE ;
		 "enum", ENUM ;
		 "extends", EXTENDS ;
		 "final", FINAL ;
		 "finally", FINALLY ;
		 "float", FLOAT ;
		 "for", FOR ;
		 "if", IF ;
		 "goto", GOTO ;
		 "implements", IMPLEMENTS ;
		 "import", IMPORT ;
		 "instanceof", INSTANCEOF ;
		 "int", INT ;
		 "interface", INTERFACE ;
		 "long", LONG ;
		 "native", NATIVE ;
		 "new", NEW ;
		 "switch", SWITCH ;
		 "synchronized", SYNCHRONIZED ;
		 "package", PACKAGE ;
		 "private", PRIVATE ;
		 "this", THIS ;
		 "protected", PROTECTED ;
		 "throw", THROW ;
		 "public", PUBLIC ;
		 "throws", THROWS ;
		 "return", RETURN ;
		 "transient", TRANSIENT ;
		 "short", SHORT ;
		 "try", TRY ;
		 "static", STATIC ;
		 "void", VOID ;
		 "strictfp", STRICTFP ;
		 "volatile", VOLATILE ;
		 "super", SUPER ;
		 "while", WHILE ;
		] 
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let space = [' ' '\t']
let newline = ['\010' '\013']
let others = ['+''-''*''/''=''"'':''('')''{''}''['']''!''&''|'';''.'',''<''>']

let identifier = ('_'|'$'|letter)(letter|digit|'_')*
(* comments *)
let comment_one_line = "//" ([^'\010' '\013'])* newline
(* /\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+/ *)
let comment_multiple_lines = "/*" ([^'*'] | newline | ('*'*[^'/']))* "*/" 

let string_literal = '"' ([^'"'])* '"'

(* types *)
let integer = (digit)+
let long = (digit)+['l''L']
let float = (digit)+('.' digit*)?['f''F']
let double = (digit)+('.' digit*)?
(* the function to scan *)
rule nexttoken = parse 
	| newline { incr_lineno lexbuf; nexttoken lexbuf }
	| space+ { nexttoken lexbuf }
	
	| comment_one_line { print_endline "comments start";incr_lineno lexbuf; nexttoken lexbuf }
	| comment_multiple_lines { print_endline "multiline comments start"; nexttoken lexbuf }

	| string_literal as slit { STRLIT slit }
	(* operators *)
	| '+' { PLUS } 
	| '-' { MINUS }
	| '/' { DIV } 
	| '*' { MUL }
	| '%' { MOD }
	
	| '!' { NOT }
	| '|' { BOR }
	| '&' { BAND }
	| '~' { BNOT }
	
	| "++" { INCREMENT }
	| "--" { DECREMENT }
	
	| "&&" { AND }
	| "||" { OR }
	| '^' { XOR }
	| "==" { EQUAL }

	| '=' { ASSIGN }
	
	| "+="  { PEQUAL }
	| "-=" { MINUSEQUAL }
	| "*=" { MULEQUAL }
	| "/=" { DIVEQUAL }
	| "%=" { MODEQUAL }
	| "&=" { ANDEQUAL }
	| "|=" { OREQUAL }
	| "^=" { XOREQUAL }
	| ">>=" { RSHIFTEQUAL }
	| "<<=" { LSHIFTEQUAL }
	| ">>>=" { LOGSHIFTEQUAL }
		
	| '?' { QM }
	| "<" { LANG }
	| ">" { RANG }
	| "]" { RBRAC }
	| "[" { LBRAC }
	| "(" { LPAR }
	| ")" { RPAR }
	| "}" { RCURL }
	| "{" { LCURL }
	
	| ';' { SEMI }
	| '.' { DOT }
	| ',' { COMM }

	| integer as i { INTLIT(int_of_string i) }
	| double as d { FLOATLIT(float_of_string d) }

	| identifier as id { 
		(* try keywords if not found then it's an identifier *)
        let l = String.lowercase id in
        try List.assoc l kw_table
        with Not_found -> IDENTIFIER id
		}
	| _ { print_endline "unknown character"; nexttoken lexbuf }
	| eof { EOF; exit 0 }


{
	let print_token = function 
		| IDENTIFIER(id) -> print_string ( " id : " ^ id )
		| STRLIT(slit) -> print_string ( " stlit : " ^ slit )
		| EOF -> print_string "EOF"
		| PLUS -> print_string "PLUS"
		| ABSTRACT -> print_string "ABSTRACT"
		| ASSERT -> print_string "ASSERT"
		| BOOLEAN -> print_string "BOOLEAN"
		| BREAK -> print_string "BREAK"
		| BYTE -> print_string "BYTE"
		| CASE -> print_string "CASE"
		| CATCH -> print_string "CATCH"
		| CHAR -> print_string "CHAR"
		| CLASS -> print_string "CLASS"
		| CONST -> print_string "CONST"
		| CONTINUE -> print_string "CONTINUE"
		| DEFAULT -> print_string "DEFAULT"
		| DO -> print_string "DO"
		| DOUBLE  -> print_string "DOUBLE"
		| ELSE -> print_string "ELSE"
		| ENUM  -> print_string "ENUM"
		| EXTENDS -> print_string "EXTENDS"
		| FINAL -> print_string "FINAL"
		| FINALLY -> print_string "FINALLY"
		| FLOAT -> print_string "FLOAT"
		| FOR -> print_string "FOR"
		| IF -> print_string "IF"
		| GOTO -> print_string "GOTO"
		| IMPLEMENTS -> print_string "IMPLEMENTS"
		| IMPORT -> print_string "IMPORT"
		| INSTANCEOF -> print_string "INSTANCEOF"
		| INT -> print_string "INT"
		| INTERFACE -> print_string "INTERFACE"
		| LONG -> print_string "LONG"
		| NATIVE -> print_string "NATIVE"
		| NEW -> print_string "NEW"
		| PACKAGE -> print_string "PACKAGE"
		| PRIVATE -> print_string "PRIVATE"
		| PROTECTED -> print_string "PROTECTED"
		| PUBLIC  -> print_string "PUBLIC "
		| RETURN -> print_string "RETURN"
		| SHORT -> print_string "SHORT"
		| STATIC -> print_string "STATIC"
		| STRICTFP -> print_string "STRICTFP"
		| SUPER -> print_string "SUPER"
		| SWITCH -> print_string "SWITCH"
		| SYNCHRONIZED -> print_string "SYNCHRONIZED"
		| THIS -> print_string "THIS"
		| THROW -> print_string "THROW"
		| THROWS -> print_string "THROWS"
		| TRANSIENT -> print_string "TRANSIENT"
		| TRY -> print_string "TRY"
		| VOID -> print_string "VOID"
		| VOLATILE -> print_string "VOLATILE"
		| WHILE -> print_string "WHILE"
		| LANG -> print_string "LANG"
		| RANG-> print_string "RANG"
		| LPAR -> print_string "LPAR"
		| RPAR -> print_string "RPAR"
		| LBRAC -> print_string "LBRAC"
		| RBRAC -> print_string "RBRAC"
		| LCURL -> print_string "LCURL"
		| RCURL-> print_string "RCURL"
		| SEMI -> print_string "SEMI"
		| COL-> print_string "COL"
		| DOT -> print_string "DOT"
		| COMM -> print_string "COMM"
		| SQ -> print_string "SQ"
		| DQ  -> print_string "DQ"
		| QM -> print_string "QM"
		| PLUS -> print_string "PLUS"
		| MINUS-> print_string "MINUS"
		| DIV -> print_string "DIV"
		| MUL -> print_string "MUL"
		| MOD  -> print_string "MOD"
		| INCREMENT -> print_string "INCREMENT"
		| DECREMENT-> print_string "DECREMENT"
		| AND -> print_string "AND"
		| OR-> print_string "OR"
		| NOT -> print_string "NOT"
		| LSHIFT -> print_string "LSHIFT"
		| RSHIFT-> print_string "RSHIFT"
		| LOGSHIFT-> print_string "LOGSHIFT"
		| BAND -> print_string "BAND"
		| BOR-> print_string "BOR"
		| XOR -> print_string "XOR"
		| BNOT -> print_string "BNOT"
		| ANOT -> print_string "ANOT"
		| EQUAL -> print_string "EQUAL"
		| ASSIGN -> print_string "ASSIGN"
		| PEQUAL -> print_string "PEQUAL"
		| MINUSEQUAL-> print_string "MINUSEQUAL"
		| MULEQUAL -> print_string "MULEQUAL"
		| DIVEQUAL -> print_string "DIVEQUAL"
		| MODEQUAL -> print_string "MODEQUAL"
		| ANDEQUAL -> print_string "ANDEQUAL"
		| OREQUAL -> print_string "OREQUAL"
		| XOREQUAL -> print_string "XOREQUAL"
		| RSHIFTEQUAL -> print_string "RSHIFTEQUAL"
		| LSHIFTEQUAL -> print_string "LSHIFTEQUAL"
		| LOGSHIFTEQUAL -> print_string "LOGSHIFTEQUAL"

		| _ -> print_string "Something else"
}
