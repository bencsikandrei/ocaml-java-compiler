%{
	open Printf
	open Lexing
%}
/* brackets */
%token LANG RANG LPAR RPAR LBRAC RBRAC LCURL RCURL /* <> () [] {} */ 

/* delimiters */
%token SEMI COL DOT COMM SQ DQ QM /* ; : . ,  '' "" ? */

/* math binary operators */
%token PLUS MINUS DIV MUL MOD  /* + - / * % */

/* logical bitwiese operators */
%token BAND BOR XOR BNOT /* & | ^ ~ */ 

/* bitwise whifts */
%token LSHIFT RSHIFT LOGSHIFT/* << >> >>> */

/* unary operators */
%token INCREMENT DECREMENT /* ++ -- */

/* logical operators */
%token EQUAL /* == */
%token AND OR NOT /* && || ! */


/* assignment operators */
%token ASSIGN /* = */
%token PEQUAL MINUSEQUAL MULEQUAL DIVEQUAL MODEQUAL ANDEQUAL OREQUAL XOREQUAL RSHIFTEQUAL LSHIFTEQUAL LOGSHIFTEQUAL /* += -= *= /= %= &= |= ^= >>= <<= >>>= */

%token ANOT /* @ */

/* special eof */
%token EOF 

/* keywords that are reserved */
%token ABSTRACT
%token ASSERT
%token BOOLEAN
%token BREAK
%token BYTE
%token CASE
%token CATCH
%token CHAR
%token CLASS
%token CONST
%token CONTINUE
%token DEFAULT
%token DO
%token DOUBLE 
%token ELSE
%token ENUM 
%token EXTENDS
%token FINAL
%token FINALLY
%token FLOAT
%token FOR
%token IF
%token GOTO
%token IMPLEMENTS
%token IMPORT
%token INSTANCEOF
%token INT
%token INTERFACE
%token LONG
%token NATIVE
%token NEW
%token PACKAGE
%token PRIVATE
%token PROTECTED
%token PUBLIC 
%token RETURN
%token SHORT
%token STATIC
%token STRICTFP
%token SUPER
%token SWITCH
%token SYNCHRONIZED
%token THIS
%token THROW
%token THROWS
%token TRANSIENT
%token TRY
%token VOID
%token VOLATILE
%token WHILE

/* identifiers and literals */
%token <string> IDENTIFIER
%token <string> STRLIT
%token <int> INTLIT
%token <float> FLOATLIT
%token <char> CHARLIT 
%token <bool> BOOLEANLIT
%token <string> NULLLIT
%token ELIPSIS


/* starting point */
%start compilationUnit
%type <string> compilationUnit

%%
compilationUnit: 
	| PACKAGE qid=qualifiedIdentifier SEMI compilationUnit { qid }
	| IMPORT qid=qualifiedIdentifier SEMI compilationUnit { qid }
	| typeDeclaration compilationUnit { }
	| EOF { "EOF reached" }
	| error { print_endline "an error occured "; "an error occured " }
;

qualifiedIdentifier: 
	| id=IDENTIFIER { print_endline id; id}
	| qualifiedIdentifier DOT id=IDENTIFIER  { print_endline id; id }
	| error { print_endline "an error occured ";"an error occured " }
;

qualifiedIdentifierStar:
	| qid=qualifiedIdentifier { print_endline qid ; qid} 
	| qualifiedIdentifierStar DOT MUL { print_endline " .*"; ".*" } 
	| error { print_endline "an error occured ";"an error occured " }
;	

typeDeclaration: 
	| typeDeclarationModifiers classDeclaration { print_endline "classDeclaration ";"classDeclaration" }
	| typeDeclarationModifiers interfaceDeclaration { print_endline "classDeclaration ";"classDeclaration" }
	| SEMI { print_endline "empty declaration"; "empty declaration "}
	| error { print_endline "an error occured ";"an error occured " }
;

typeDeclarationModifiers:
	| PUBLIC { print_endline "PUBLIC"; "PUBLIC"}
	| PROTECTED { print_endline "PROTECTED"; "PROTECTED"}
	| PRIVATE { print_endline "PRIVATE"; "PRIVATE"}
	| STATIC { print_endline "STATIC"; "STATIC"}
	| ABSTRACT { print_endline "ABSTRACT"; "ABSTRACT"}
	| FINAL { print_endline "FINAL"; "FINAL"}
	| STRICTFP { print_endline "STRICTFP"; "STRICTFP"}
	| error { print_endline "an error occured ";"an error occured " }
;


%%
let parse_error s = 
	print_endline s;
	flush stdout


