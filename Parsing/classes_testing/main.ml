open JavaLexer
open JavaParser
open Lexing 
open Definitions

let position lexbuf=
	let pos=lexeme_start_p lexbuf in 
	let error=Lexing.lexeme lexbuf in
	"Unexpected: \""^error^"\" in line: "^string_of_int pos.pos_lnum^" char:"^string_of_int(pos.pos_cnum-pos.pos_bol+1);;

let print arg=	match arg with
	| STR s -> print_string (s);
	| JML j -> print_string (print_list print_java_method j "\n");;

let fakeDict str= match str with 
	| "file" -> javaFile
	| "method" -> javaMethods 
	| "class" -> javaClass
	| _ -> javaFile;;

let compile mode file =
		print_string ("File "^file^" is being treated! ");
		try
		let input_file = open_in file in
		let lexbuf = Lexing.from_channel input_file in
		try
			print( (fakeDict mode) nexttoken lexbuf );
			close_in (input_file);
			print_endline("OK");
		with 
				|SyntaxError s -> print_endline (s^" BAD");
				|JavaParser.Error -> print_endline ("Parsing error  "^(position lexbuf)^" BAD");
		with	Sys_error s -> print_endline ("Can't find file ' " ^ file ^ "'");;

try 
	compile Sys.argv.(1) Sys.argv.(2)
with
	| _  -> print_endline("Usage: main <parser> <file>");;