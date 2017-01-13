(* the main function *)
open Lexing
open JavaLexer
open JavaParser 
open Expressions

let test_parser lexbuf =
	try 
		let res = JavaParser.compilationUnit JavaLexer.nexttoken lexbuf in
		print_endline (Expressions.string_of_stmt res)
	with
	| JavaException(s) -> print_endline s 

let test_lexer lexbuf = 
	let res = JavaLexer.nexttoken lexbuf in
	print_string "Reading token in line ";
	print_int lexbuf.lex_curr_p.pos_lnum;
	print_string " : ";
	print_string "character ";
	print_int (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol);
	print_string " : ";
	JavaLexer.print_token res;
	print_string "\n"

let main () =
	let lp = 
		if Array.length Sys.argv > 2
			then Sys.argv.(2)
		else "p" 
	in 
	let cin =
		if Array.length Sys.argv > 1
			then open_in Sys.argv.(1)
		else stdin
	in
	let lexbuf = Lexing.from_channel cin in
	try 	
		while true do 
			if lp = "l" then
				test_lexer lexbuf
			else 
				test_parser lexbuf
    	done
	with End_of_file -> exit 0

let _ = Printexc.print main ()
