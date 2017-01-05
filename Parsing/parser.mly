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
	m=modifiers { m }
	| b=block { b }
	| error { " an error has occured\n" }

modifiers:
	PUBLIC { "PUBLIC" }
	| PROTECTED { "PROTECTED" }
	| PRIVATE { "PRIVATE" }
	| STATIC { "STATIC" }
	| ABSTRACT { "ABSTRACT" }
	| FINAL { "FINAL" }
	| STRICTFP { "STRICTFP" }
	| VOLATILE { "VOLATILE" }
	| error { " an error has occured\n" }
;

block:
	LCURL lvds=localVariableDeclAndStmts RCURL { "{"^lvds^"}" }
	| LCURL RCURL { "{ }" }
	| error { " an error has occured\n" } 
;

localVariableDeclAndStmts:
	lvd=localVariableDeclOrStmt { lvd }
	| lvds=localVariableDeclAndStmts lvd=localVariableDeclOrStmt { lvds^lvd }
	| error { " an error has occured\n" } 
;

localVariableDeclOrStmt:
	lvd=localVariableDeclStmt { lvd }
	| stmt=statement { stmt }
	| error { " an error has occured\n" } 
;

localVariableDeclStmt:
	/* ts=typeSpecifier vd=variableDeclaration SEMI { ts^vd }
	| FINAL ts=typeSpecifier vd=variableDeclaration SEMI { ts^vd } */
	| error { " an error has occured\n" } 
;

statement:
	es=emptyStmt { es }
	| error { " an error has occured\n" } 
;

emptyStmt:
	SEMI { ";" }
	| error { " an error has occured\n" } 
;


%%
let parse_error s = 
	print_endline s;
	flush stdout
