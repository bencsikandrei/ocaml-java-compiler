open String
open Str

let isValid c =
	match c with
	| ' ' -> true
	| '\t' -> true
	| '\n' -> true
	| '.' -> true
	| '(' -> true
	| ')' -> true
	| '[' -> true
	| ']'	-> true
	| ','	-> true
	| _ -> if c >= 'a' && c<='z' || c>='A' && c<='Z' || c>='0' && c<='9' then true else false
	
 
let rec check_gen str mid = 
	match str with | ""->("<"^mid,"") | _ ->
		let c = String.get str 0 in
		let tail = String.sub str 1 ((String.length str) -1) in	
		match c with
		|'<' -> let (be,en) = check_gen tail "" in check_gen en (mid^be)
		|'>' -> ("<|"^mid^">|",tail)
		| _ -> if (isValid c) then
				check_gen tail (mid^ (String.make 1 c) )
			else			
				("<"^mid^(String.make 1 c),tail)

and check str =
	match str with | ""->"" | _ ->
		let c = String.make 1 (String.get str 0) in
		let tail = String.sub str 1 ((String.length str) -1) in
		match c with
		| "<" -> if (String.length tail > 1) then 
					let c2=String.get tail 0 in 
					match c2 with 
						|'<' -> "<<"^(String.sub str 2 ((String.length str) -2))
						| _ -> let (be,en)=(check_gen tail "") in be ^ (check en)
				else 
					let (be,en)=(check_gen tail "") in be ^ (check en)
		| _  -> c^(check tail)


let rec removeComments str=
	match str with | ""->"" | _ ->
		let c = String.get str 0 in
		let tail = String.sub str 1 ((String.length str) -1) in	
		match c with 
		|'/' -> if (String.length tail > 1) then let c2=String.get tail 0 in 
				match c2 with
				| '/' -> removeLineComment  (String.sub str 2 ((String.length str) -2))
				| '*' -> removeMultiLineComment (String.sub str 2 ((String.length str) -2))
				| _ -> (String.make 1 c)^(removeComments tail)
  		else "/"^(removeComments tail)
		| _ -> (String.make 1 c)^(removeComments tail)

and removeLineComment str=
	match str with | ""->"" | _ ->
		let c = String.get str 0 in
		let tail = String.sub str 1 ((String.length str) -1) in	
		match c with 
		|'\n' -> "\n"^(removeComments tail)
		| _ -> removeLineComment tail

and removeMultiLineComment str=
	match str with | ""->"" | _ ->
		let c = String.get str 0 in
		let tail = String.sub str 1 ((String.length str) -1) in	
		match c with 
		|'*' -> if (String.length tail > 1) then let c2=String.get tail 0 in if(c2='/') then removeComments (String.sub str 2 ((String.length str) -2)) else removeMultiLineComment tail else  ""
		| _ -> removeMultiLineComment tail;;


let read_lines name =
  let try_read() = try Some (input_line name) with End_of_file -> None in
  let rec loop acc = match try_read() with
    | Some s -> loop (acc^s^"\n");
    | None -> acc^"" in
  loop ""
;;
  

let preprocess str = 
	let tmp = read_lines str in 
	(*let tmp = removeComments tmp in *)
	check tmp
	
