(* visibility *)
let visible = ref true;;

(* initialize logs *)
let init (verbose : bool) =
	visible := verbose

let logs (header : string) (text : string) =
	(* just prints the text if verbose output is set *)
	if !visible = true then
		print_endline ("["^header^"] : "^text)


(* a logger helper *)
let debug (text : string) = 
	logs  "DEBUG" text

(* information printing *)
let info (text : string) =
	logs  "INFO" text

