(* the main function *)
let main () =
	let cin =
		if Array.length Sys.argv > 1
			then open_in Sys.argv.(1)
		else stdin
	in
	let lexbuf = Lexing.from_channel cin in
		let res = Lexer.nexttoken lexbuf in
		print_string "Reading token in line ";
    	print_int lexbuf.lex_curr_p.pos_lnum;
    	print_string " : ";
    	Lexer.print_token res;
    	print_string "\n";
    	res

let _ = Printexc.print main ()
