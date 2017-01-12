open String
open Char

let indent var =
	let size = length var in 
		let rec iterate pos=
			if pos=size then 
				""
			else
				if (String.get var pos)='\n' then 
					"\n\t"^(iterate (pos+1)) 
				else 
					(Char.escaped (String.get var pos))^(iterate (pos+1))
		in
			"\t"^iterate 0;;

let rec print_list print_f  list separator = match list with
	| [] ->  ""
	| head::tail -> (print_f head)^separator^(print_list print_f tail separator);;

