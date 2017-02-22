(* a logger helper *)
let debug (visible : bool) (text : string) = 
	(* just prints the text if verbose output is set *)
	if visible = true then
		print_endline ("[DEBUG] : "^text)