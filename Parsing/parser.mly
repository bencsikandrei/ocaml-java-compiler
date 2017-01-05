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
	s=statement { s }
	| error { " an error has occured\n" }

/* operators */
logicalUnaryOperator: 
	BNOT { "~" }
	| NOT { "!" }
	| error { "an error has occured" } 
	;

arithmeticUnaryOperator: 
	PLUS { "+" }
	| MINUS { "-" }
	| error { "an error has occured" } 
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
	| error { "an error has occured" } 
;
/* end operators */

/* modifiers */
modifiers:
	m=modifier { m }
	| ms=modifiers m=modifier { ms^m }
	| error { "an error has occured" } 
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
	| error { " an error has occured\n" }
;
/* end modifiers */

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

/* statements */
statement:
	es=emptyStmt { es }
	/*| ls=labelStmt { ls }
	| exs=expressionStmt SEMI { exs }
	| ss=selectStmt { ss }
	| gs=guardingStmt { gs } */
	| js=jumpStmt { js }
	| is=iterStmt { is }
	| b=block { b }
	| error { " an error has occured\n" } 
;

emptyStmt:
	SEMI { ";" }
	| error { " an error has occured\n" } 
;

labelStmt:
	id=IDENTIFIER COL { id^" : " }
	| CASE ce=ctExpression COL { "case "^ce^" : " }
	| DEFAULT COL { "default : " }
	| error { "an error has occured" } 
;

expressionStmt:
	e=expression { e }
	| error { "an error has occured" } 
;

selectStmt:
	IF LPAR e=expression RPAR s=statement { e^s}
	| IF LPAR e=expression RPAR s1=statement ELSE s2=statement { e^s1^s2 }
	| SWITCH LPAR e=expression RPAR b=block { e^b }
	| error { "an error has occured" } 
;

iterStmt: 
	WHILE LPAR e=expression RPAR s=statement { "while("^e^")"^s }
		| error { "an error has occured" } 
	;

jumpStmt: 
	BREAK id=IDENTIFIER SEMI { "break "^id^";" }
	| BREAK SEMI { "break;" }
    | CONTINUE id=IDENTIFIER SEMI { "continue "^id^";"}
	| CONTINUE SEMI { "continue;"}
	| RETURN e=expression SEMI { "return "^e^";"  }
	| RETURN SEMI { "return;"}
	| THROW e=expression SEMI { "throw "^e^";" }
	| error { "an error has occured" } 
	;
/* expressions */

expression:
	/* ae=assignmentExpression { ae } */
	| error { "expression:an error has occured" } 
;
ctExpression:
/*	ce=conditionalExpression { ce } */
	| error { "ct expression an error has occured" } 
;

/* end expressions */

/* catch */
catches
	: c=catch { c } 
	| cs=catches c=catch { cs^c }
	| error { "an error has occured" } 
;

catch: 
	ch=catchHeader b=block { ch^b }
	| error { "an error has occured" } 
;

catchHeader: 
	CATCH RPAR ts=types id=IDENTIFIER RPAR { }
	| CATCH RPAR ts=types LPAR { }
	| error { "an error has occured" } 
;

finally
	: FINALLY b=block { "finally "^b }
	| error { "an error has occured" } 
;
/* end catch */

/* types */
types: 
	pt=primitive { pt }
	/* need classes here */
	| error { "an error has occured" } 
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
	| error { "an error has occured" } 
	;

semiColons: 
	SEMI { ";" }
   	| sc=semiColons SEMI { sc^";" }
   	| error { "an error has occured" } 
;

%%
let parse_error s = 
	print_endline s;
	flush stdout
