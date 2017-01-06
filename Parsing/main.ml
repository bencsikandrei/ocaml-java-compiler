(* the main function *)
open Lexing
open Lexer
open Parser 

let test_parser lexbuf =
	let res = Parser.compilationUnit Lexer.nexttoken lexbuf in
	print_endline res

let test_lexer lexbuf = 
	let res = Lexer.nexttoken lexbuf in
	print_string "Reading token in line ";
	print_int lexbuf.lex_curr_p.pos_lnum;
	print_string " : ";
	print_string "character ";
	print_int (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol);
	print_string " : ";    	
	Lexer.print_token res;
	print_string "\n"

let main () =
	let cin =
		if Array.length Sys.argv > 1
			then open_in Sys.argv.(1)
		else stdin
	in
	let lexbuf = Lexing.from_channel cin in
	try 	
		while true do 
			test_parser lexbuf
    	done
	with End_of_file -> exit 0

let _ = Printexc.print main ()