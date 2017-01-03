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
	| comment_multiple_lines { nexttoken lexbuf }
	| eof { EOF; exit 0 }
	| '+' { PLUS }
	| '-' { MINUS }
	| '/' { DIV } 
	| '*' { MUL }
	| '%' { MOD }
	| integer as i { INTLIT(int_of_string i) }
	| double as d { FLOATLIT(float_of_string d) }
	| identifier as id { 
		(* try keywords if not found then it's an identifier *)
        let l = String.lowercase id in
        try List.assoc l kw_table
        with Not_found -> IDENTIFIER id
		}
	| _ { print_endline "unknown character"; nexttoken lexbuf }
{
	let print_token = function 
		| EOF -> print_string "EOF"
		| PLUS -> print_string "PLUS"
		| _ -> print_string "Something else"
}
