open JavaLexer
open JavaParser
open Lexing 
open Ast
open Array
open Printing
open Preprocess 

let verbose = ref false
let bad_test = ref false
let filename = ref ".java"
let modes = ref "file"

let set_mode m = modes := m
let set_file f = filename := f

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
			| JFILE f -> print_string (prit_java_file f)

let fakeDict str= match str with 
	| "file" -> javaFile
	| "method" -> javaMethods 
	| "class" -> javaClass
	| "statement" -> compilationUnit
	| "expression" -> anExpression
	| _ -> javaFile;;

let ok_nok bad =
	match bad with
	| true -> " OK "
	| false -> " BAD "

let compile mode file vervose bad =
		print_string ("File "^file^" is being treated! ");
		try
		let input_file = open_in file in
		let tmp_file = open_out "./build/tmp" in 
		output_string tmp_file (preprocess input_file );
		close_out tmp_file;
		let input_file = open_in "./build/tmp" in

		let lexbuf = Lexing.from_channel input_file in
			try
				print( (fakeDict mode) nexttoken lexbuf ) vervose;
				close_in (input_file);
				print_endline("OK");
			with 
				|SyntaxError s -> print_endline (s^" "^(position lexbuf)^(ok_nok !bad_test));
				|JavaParser.Error -> print_endline ("Parsing error  "^(position lexbuf)^(ok_nok !bad_test));
				|e -> print_endline ("Unexpected error while parsing - "^(position lexbuf)^" "^(Printexc.to_string e)^(ok_nok !bad_test));
		with	
				|Sys_error s -> print_endline ("Can't find file ' " ^ file ^ "'");
				|_ -> print_endline ("Unexpected error with the file");;

let main =
	let speclist = [("-v", Arg.Set verbose, "Enables verbose mode");
	("-f", Arg.String (set_file), "File name to be parsed");
	("-m", Arg.String (set_mode), "Which parser to use: file, method, class, statement, expression; default: file");
	("-b", Arg.Set bad_test, "Test expected to fail");
	]
	in let usage_msg = "miniJavaCompiler. Options available:"
	in Arg.parse speclist print_endline usage_msg;
	print_endline ("Verbose mode: " ^ string_of_bool !verbose);
	print_endline ("Parser to be used: " ^ !modes);
	print_endline ("Bad test expected: " ^ string_of_bool !bad_test);
	try 
		compile !modes !filename !verbose !bad_test
	with
		| _  -> print_endline("Usage: main <parser> <file> [-v] [-b]");;

let () = 
	main