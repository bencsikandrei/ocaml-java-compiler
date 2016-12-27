{
	open JavaParser
	exception SyntaxError of string
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let real = digit * ('.' digit *)?
let ident = letter (letter | digit | '_')*
let annot = "@"ident
let space = [' ' '\t']

rule nexttoken = parse
	| space+ { nexttoken lexbuf }
  	| "\n" {Lexing.new_line lexbuf;nexttoken lexbuf}
	| eof { EOF }
	| "+" { PLUS }
	| "-" { MINUS }
	| "/" { DIV }
	| "," {COMM}
	| ";" { SEMI }
	| "@" {ANOT}
	| "void" {VOID}
	| "static" {STATIC}
	| "(" {LPAR}
	| ")" {RPAR}
	| "}" {RCURL}
	| "{" {LCURL}
	| "return" {RETURN}
	| "public" {PUBLIC}
	| "protected" {PROTECTED}
	| "private" {PRIVATE}
	| real as nb { FLOATLIT (float_of_string nb) }
	| ident as str { IDENTIFIER str }
	| _	{ raise (SyntaxError("Unexected: "^Lexing.lexeme lexbuf))}
