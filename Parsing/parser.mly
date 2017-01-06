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
%token EQUAL NEQUAL /* == != */
%token GTHAN LTHAN GETHAN LETHAN /* > < >= <= */ 
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
%token <float> DOUBLELIT
%token <float> FLOATLIT
%token <char> CHARLIT 
%token <bool> BOOLEANLIT
%token <string> NULLLIT
%token ELIPSIS

/* priorities 
%right ASSIGN
%left OR
%left AND
%left EQUAL NEQUAL
%left GTHAN GETHAN LTHAN LETHAN
%left PLUS MINUS
%left MUL DIV MOD
%right NOT
%left DOT
*/
/* starting point */
%start compilationUnit
%type <string> compilationUnit

%%
compilationUnit:
	s=block { s }
	| error { " an error has occured\n" }
;

/* operators */
logicalUnaryOperator: 
	BNOT { "~" }
	| NOT { "!" }
;

arithmeticUnaryOperator: 
	PLUS { "+" }
	| MINUS { "-" }
;

assignmentOp: 
	ASSIGN { "=" }
	| MULEQUAL  { "*=" }
	| DIVEQUAL { " /= " }
	| MODEQUAL { "%=" }
	| PEQUAL { "+=" }
	| MINUSEQUAL { "-=" }
	| LSHIFTEQUAL { "<<=" }
	| RSHIFTEQUAL { ">>=" }
	| LOGSHIFTEQUAL { ">>>=" }
	| ANDEQUAL { "&=" }
	| XOREQUAL { "^=" }
	| OREQUAL { "|=" }
;
/* end operators */

/* modifiers */
modifiers:
	m=modifier { m }
	| ms=modifiers m=modifier { ms^m }
;

modifier:
	PUBLIC { "PUBLIC" }
	| PROTECTED { "PROTECTED" }
	| PRIVATE { "PRIVATE" }
	| STATIC { "STATIC" }
	| ABSTRACT { "ABSTRACT" }
	| FINAL { "FINAL" }
	| STRICTFP { "STRICTFP" }
	| VOLATILE { "VOLATILE" }
;
/* end modifiers */

block:
	LCURL lvds=statement RCURL { "{"^lvds^"}" }
	| LCURL RCURL { "{ }" }
;

statement:
	es=emptyStmt { es }
	| ls=labelStmt { ls }

labelStmt:
	id=IDENTIFIER COL { id^" : " }
	| CASE COL { "case : " }
	| DEFAULT COL { "default : " }
;

emptyStmt:
	SEMI { ";" }
;

/* catch */
catches
	: c=catch { c } 
	| cs=catches c=catch { cs^c }
;

catch: 
	ch=catchHeader b=block { ch^b }
;

catchHeader: 
	CATCH RPAR ts=types id=IDENTIFIER RPAR { }
	| CATCH RPAR ts=types LPAR { }
;

finally: 
	FINALLY b=block { "finally "^b }
;
/* end catch */

/* types */
types: 
	pt=primitive { pt }
	/* need classes here */
;

primitive: 
	BOOLEAN { "boolean" }
	| CHAR  { "char" }
	| BYTE { "byte" }
	| SHORT { "short" }
	| INT { "int" }
	| LONG { "long" }
	| FLOAT { "float" }
	| DOUBLE { "double" }
	| VOID { "void" }
	;

semiColons: 
	SEMI { ";" }
   	| sc=semiColons SEMI { sc^";" }
;

%%
let parse_error s = 
	print_endline s;
	flush stdout
