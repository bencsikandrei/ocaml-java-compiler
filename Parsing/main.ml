open JavaLexer
open JavaParser
open Lexing 
open Ast
open Array
open Printing 

let position lexbuf=
	let pos=lexeme_start_p lexbuf in 
	let error=Lexing.lexeme lexbuf in
	"Unexpected: \""^error^"\" in line: "^string_of_int pos.pos_lnum^" char:"^string_of_int(pos.pos_cnum-pos.pos_bol+1);;

let print arg vervose=	
	if vervose then 
		match arg with
			| STR s -> print_string (s);
			| JML j -> print_string (Printing.print_list print_java_method j "\n");
			| STATE s -> print_string ( string_of_stmt s );
			| EXPR e -> print_string (string_of_exp e);
			| JCLASS c -> print_string (print_java_class c);
			| JFILE f -> print_string ""

let fakeDict str= match str with 
	| "file" -> javaFile
	| "method" -> javaMethods 
	| "class" -> javaClass
	| "statement" -> compilationUnit
	| "expression" -> anExpression
	| _ -> javaFile;;

let compile mode file vervose =
		print_string ("File "^file^" is being treated! ");
		try
		let input_file = open_in file in
		let lexbuf = Lexing.from_channel input_file in
			try
				print( (fakeDict mode) nexttoken lexbuf ) vervose;
				close_in (input_file);
				print_endline("OK");
			with 
					|SyntaxError s -> print_endline (s^" "^(position lexbuf)^" BAD");
					|JavaParser.Error -> print_endline ("Parsing error  "^(position lexbuf)^" BAD");
					|e -> print_endline ("Unexpected error while parsing - "^(position lexbuf)^" "^(Printexc.to_string e)^" BAD");
		with	
				|Sys_error s -> print_endline ("Can't find file ' " ^ file ^ "'");
				|_ -> print_endline ("Unexpected error with the file");;

try 
	if length Sys.argv=3 then
		compile Sys.argv.(1) Sys.argv.(2) false
	else if length Sys.argv=4 then
		compile Sys.argv.(1) Sys.argv.(2) true
with
	| _  -> print_endline("Usage: main <parser> <file>");;