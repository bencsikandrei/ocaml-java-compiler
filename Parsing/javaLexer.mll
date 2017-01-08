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
	| "abstract" {ABSTRACT}
	| "final" {FINAL}
	| "synchronized" {SYNCHRONIZED}
	| "native" {NATIVE}
	| "strictfp" {STRICTFP}
	| "<" {LANG}
	| ">" {RANG}
	| "(" {LPAR}
	| ")" {RPAR}
	| "}" {RCURL}
	| "{" {LCURL}
	| "return" {RETURN}
	| "public" {PUBLIC}
	| "protected" {PROTECTED}
	| "private" {PRIVATE}
	| "float" {FLOAT}
	| "boolean" {BOOLEAN}
	| "byte" {BYTE}
	| "char" {CHAR}
	| "int" {INT}
	| "long" {LONG}
	| "short" {SHORT}
	| "double" {DOUBLE}
	| "..." {ELIPSIS}
	| "throws" {THROWS}
	| "class" {CLASS}
	| "extends" {EXTENDS}
	| "implements" {IMPLEMENTS}
	| "package" {PACKAGE}
	| "import" {IMPORT}
	| real as nb { FLOATLIT (float_of_string nb) }
	| ident as str { IDENTIFIER str }
	| _	{ raise (SyntaxError("Unexpected: "^Lexing.lexeme lexbuf))}
