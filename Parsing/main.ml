(* the main function *)
open Lexing
open Lexer
open Parser 

let main () =
	let cin =
		if Array.length Sys.argv > 1
			then open_in Sys.argv.(1)
		else stdin
	in
	let lexbuf = Lexing.from_channel cin in
		while true do 
		let res = Lexer.nexttoken lexbuf in
		Parser.goal res lexbuf;
		print_string "Reading token in line ";
    	print_int lexbuf.lex_curr_p.pos_lnum;
    	print_string " : ";
    	Lexer.print_token res;
    	print_string "\n";
    	res
    	done

let _ = Printexc.print main ()
