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

/* priorities */
%right ASSIGN
%left OR
%left AND
%left EQUAL NEQUAL
%left GTHAN GETHAN LTHAN LETHAN
%left PLUS MINUS
%left MUL DIV MOD
%right NOT
%left DOT

/* starting point */
%start compilationUnit
%type <string> compilationUnit

%%
compilationUnit:
	s=statement { s }
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
	LCURL lvds=localVariableDeclAndStmts RCURL { "{"^lvds^"}" }
	| LCURL RCURL { "{ }" }
;

localVariableDeclAndStmts:
	lvd=localVariableDeclOrStmt { lvd }
	| lvds=localVariableDeclAndStmts lvd=localVariableDeclOrStmt { lvds^lvd }
;

localVariableDeclOrStmt:
	lvd=localVariableDeclStmt { lvd } 
	| stmt=statement { stmt }
;

localVariableDeclStmt:
	ts=types vd=variableDeclarations SEMI { ts^vd^";" }
	| FINAL ts=types vd=variableDeclarations SEMI { "final "^ts^" "^vd^";" }
;

variableDeclarations: 
 	vd=variableDeclaration { vd }
	| vds=variableDeclarations COMM vd=variableDeclaration { vds^" , "^vd }
;

variableDeclaration:
	dn=declaratorName { dn }
	| dn=declaratorName ASSIGN vi=varInitializer { dn^" = "^vi }
;

declaratorName: 
	id=IDENTIFIER { id }
;

varInitializer:
	e=expression { e }
;

/* statements */
statement:
	es=emptyStmt { es }
	/*| ls=labelStmt { ls }
	| ss=selectStmt { ss }
	| gs=guardingStmt { gs } */
	| exs=expressionStmt SEMI { exs }
	| js=jumpStmt { js }
	| is=iterStmt { is }
	| b=block { b }
;

labelStmt:
	id=IDENTIFIER COL { id^" : " }
	| CASE ce=ctExpression COL { "case "^ce^" : " }
	| DEFAULT COL { "default : " }
;

expressionStmt:
	e=expression { e }
	| EOF { "eof" }
;

emptyStmt:
	SEMI { ";" }
;



selectStmt:
	IF LPAR e=expression RPAR s=statement { "if("^e^")"^s }
	| IF LPAR e=expression RPAR s1=statement ELSE s2=statement { "if("^e^")"^s1^"\nelse "^s2 }
	/* | SWITCH LPAR e=expression RPAR b=block { e^b } */
;

iterStmt: 
	WHILE LPAR e=expression RPAR s=statement { "while("^e^")"^s }
	| DO s=statement WHILE LPAR e=expression RPAR SEMI { "do "^s^" while ("^e^");"}
	| FOR LPAR fi=forInit fe=forExpr fin=forIncr RPAR s=statement { "for("^fi^fe^fin^")"^s }
	| FOR LPAR fi=forInit fe=forExpr RPAR s=statement { "for("^fi^fe^")"^s }
	/* TODO add a foreach */
	;

forInit: 
	es=expressionStmts SEMI { es^";" }
	| lvds=localVariableDeclStmt { lvds }
	| SEMI { ";" }
;

forExpr: 
	e=expression SEMI { e^";" }
	| SEMI { ";" }
;

forIncr: 
	es=expressionStmts { es }
;

expressionStmts: 
	es=expressionStmt { es }
	| ess=expressionStmts COMM es=expressionStmt { ess^" , "^es } 
;

jumpStmt: 
	BREAK id=IDENTIFIER SEMI { "break "^id^";" }
	| BREAK SEMI { "break;" }
    | CONTINUE id=IDENTIFIER SEMI { "continue "^id^";"}
	| CONTINUE SEMI { "continue;"}
	| RETURN e=expression SEMI { "return "^e^";"  }
	| RETURN SEMI { "return;"}
	| THROW e=expression SEMI { "throw "^e^";" }
;

/* expressions */
expression:
	ae=assignmentExpression { ae }
;
ctExpression:
	ce=conditionalExpression { ce }
;

assignmentExpression:
	ce=conditionalExpression { ce } 
	| ue=unaryExpression ao=assignmentOp ae=assignmentExpression { ue^ao^ae }
;

conditionalExpression:
	coe=conditionalOrExpression { coe }
	| coe=conditionalOrExpression QM e=expression COL ce=conditionalExpression {coe^" ? "^e^" : "^ce }
;

conditionalOrExpression:
	cae=conditionalAndExpression { cae } 
	| coe=conditionalOrExpression OR cae=conditionalAndExpression { coe^" || "^cae }
;

conditionalAndExpression:
	ioe=inclusiveOrExpression { ioe }
	| cae=conditionalAndExpression AND ioe=inclusiveOrExpression { cae^" && "^ioe }
;

inclusiveOrExpression: 
	eoe=exclusiveOrExpression { eoe }
	| ioe=inclusiveOrExpression BOR eoe=exclusiveOrExpression { ioe^" | "^eoe }
;

exclusiveOrExpression: 
	ae=andExpression { ae }
	| eoe=exclusiveOrExpression XOR ae=andExpression { eoe^" ^ "^ae }
;

castExpression:
	ue=unaryExpression { ue }
;

mulExpression:
	ce=castExpression { ce }
	| me=mulExpression MUL ce=castExpression { me^" * "^ce }

;

addExpression:
	me=mulExpression { me }
	| ae=addExpression PLUS me=mulExpression { ae^" + "^me }
	| ae=addExpression MINUS me=mulExpression { ae^" - "^me }
;

shiftExpression:
	ae=addExpression { ae }
;

relationalExpression:
	se=shiftExpression { se }
;

equalityExpression:
	re=relationalExpression { re }
;

andExpression: 
	ee=equalityExpression { ee }
	| ae=andExpression BAND { ae^" & "} 
;

unaryExpression:
	INCREMENT ue=unaryExpression { "++"^ue }
	| DECREMENT ue=unaryExpression { "--"^ue }
/* END expressions */

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
