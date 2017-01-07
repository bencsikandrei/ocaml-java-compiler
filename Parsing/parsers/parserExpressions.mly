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
	| ex=expression SEMI { ex }
	| error { " an error has occured\n" }
;

expression:
	ae=assignmentExpression { ae }
;

assignmentExpression:
	ce=conditionalExpression { ce }
	| ue=unaryExpression ass=assignmentOperator ae=assignmentExpression { ue^ass^ae }
;

conditionalExpression:
	cor=conditionalOrExpression { cor }
	| cor=conditionalOrExpression QM ex=expression COL ce=conditionalExpression { cor^"?"^ex^":"^ce }
;

conditionalOrExpression:
	cand=conditionalAndExpression { cand }
	| cor=conditionalOrExpression OR cand=conditionalAndExpression { cor^"OR"^cand }
;

conditionalAndExpression:
	ior=inclusiveOrExpression { ior }
	| cand=conditionalAndExpression AND ior=inclusiveOrExpression { cand^"AND"^ior }
;

inclusiveOrExpression:
	eor=exclusiveOrExpression { eor }
	| ior=inclusiveOrExpression BOR eor=exclusiveOrExpression { ior^"|"^eor }
;

exclusiveOrExpression:
	a=andExpression { a }
	| eor=exclusiveOrExpression XOR a=andExpression { eor^"^"^a }
;

andExpression:
	eq=equalityExpression { eq }
	| a=andExpression BAND eq=equalityExpression { a^"BAND"^eq }
;

equalityExpression:
	rel=relationalExpression { rel }
	| eq=equalityExpression EQUAL rel=relationalExpression { eq^"=="^rel }
	| eq=equalityExpression NEQUAL rel=relationalExpression { eq^"!="^rel }
;

relationalExpression:
	sh=shiftExpression { sh }
	| rel=relationalExpression LTHAN sh=shiftExpression { rel^"<"^sh }
	| rel=relationalExpression GTHAN sh=shiftExpression { rel^">"^sh }
	| rel=relationalExpression LETHAN sh=shiftExpression { rel^"<="^sh }
	| rel=relationalExpression GETHAN sh=shiftExpression { rel^">="^sh }
	(* instance of dafuq *)
;

shiftExpression:
	add=additiveExpression { add }
	| sh=shiftExpression LSHIFT add=additiveExpression { sh^"<<"^add }
	| sh=shiftExpression RSHIFT add=additiveExpression { sh^">>"^add }
	| sh=shiftExpression LOGSHIFT add=additiveExpression { sh^">>>"^add }
;

additiveExpression:
	mul=multiplicativeExpression { mul }
	| add=additiveExpression PLUS mul=multiplicativeExpression { add^"+"^mul }
	| add=additiveExpression MINUS mul=multiplicativeExpression { add^"-"^mul }
;

multiplicativeExpression:
	cast=castExpression { cast }
	| mul=multiplicativeExpression MUL cast=castExpression { mul^"*"^cast }
	| mul=multiplicativeExpression DIV cast=castExpression { mul^"/"^cast }
	| mul=multiplicativeExpression MOD cast=castExpression { mul^"%"^cast }
;

castExpression:
	un=unaryExpression { un }
	(*| LPAR expression RPAR logicalUnaryExpression { }*)
;

unaryExpression:
	INCREMENT un=unaryExpression { "++"^un }
	| DECREMENT un=unaryExpression { "--"^un }
	| aop=arithmeticUnaryOperator ct=castExpression { aop^ct }
	| logu=logicalUnaryExpression { logu }
;

logicalUnaryExpression:
	post=postfixExpression { post }
	| loguop=logicalUnaryOperator un=unaryExpression { loguop^un }
;

postfixExpression:
	pri=primaryExpression { pri }
	| rpost=realPostfixExpression { rpost } 
;

realPostfixExpression:
	post=postfixExpression INCREMENT { post^"++" }
	| post=postfixExpression DECREMENT { post^"--" }
;

primaryExpression:
	qn=qualifiedName { qn }
	| njs=notJustName {njs }
;

qualifiedName:
	id=IDENTIFIER { id }
	| qn=qualifiedName DOT id=IDENTIFIER { qn^"."^id }
;

notJustName:
	spn=specialName { spn }
	| all=newAllocationExpression { all }
	| cpri=complexPrimary { cpri }
;

specialName:
	THIS { "this" }
	| SUPER { "super" }
	| NULLLIT { "null" }
;

newAllocationExpression:
	pall=plainNewAllocationExpression { pall }
	| qn=qualifiedName DOT pall=plainNewAllocationExpression { qn^"."^pall }
;

plainNewAllocationExpression:
	arrall=arrayAllocationExpression { arrall }
    	| call=classAllocationExpression { call }
    	| arrall=arrayAllocationExpression LCURL RCURL { arrall^"{"^"}" }
    	| call=classAllocationExpression LCURL RCURL { call^"{"^"}" }
    	| arrall=arrayAllocationExpression LCURL arri=arrayInitializers RCURL { arrall^"{"^arri^"}" }
    	(*| call=classAllocationExpression LCURL fdec=fieldDeclarations RCURL { call^"{"^fdec^"}" }*)
;

arrayInitializers:
	vinit=variableInitializer { vinit }
	| arri=arrayInitializers COMM vinit=variableInitializer { arri^","^vinit }
	| arri=arrayInitializers COMM  { arri^"," }
;

variableInitializer:
	ex=expression { ex }
	| LCURL RCURL { "{"^"}" }
	| LCURL arri=arrayInitializers RCURL { "{"^arri^"}" }
;

classAllocationExpression:
	NEW tn=typeName LPAR args=argumentList RPAR { "new"^tn^"("^args^")" }
	| NEW tn=typeName LPAR RPAR { "new"^tn^"("^")" }
;

arrayAllocationExpression:
	NEW tn=typeName de=dimExprs d=dims { "new"^tn^de^d }
	| NEW tn=typeName de=dimExprs { "new"^tn^de }
        | NEW tn=typeName d=dims { "new"^tn^d }
;

dimExprs:
	de=dimExpr { de }
	| ds=dimExprs d=dimExpr { ds^d }
;

dimExpr:
	LBRAC ex=expression RBRAC { "["^ex^"]" }
;

dims:
	EOF { "BL" }
	(*| d=dims OP_DIM { d^ }*)
;

complexPrimary:
	LPAR ex=expression RPAR { "("^ex^")" }
	| cprin=complexPrimaryNoParenthesis { cprin }
;

complexPrimaryNoParenthesis:
	stlit=STRLIT { stlit }
	| blit=BOOLEANLIT { string_of_bool blit }
	| ilit=INTLIT { string_of_int ilit }
(* for now they are strings *)
;

argumentList:
	ex=expression { ex }
	| args=argumentList DOT ex=expression { args^"."^ex }
;

typeName:
	pri=primitiveType { pri }
	| qn=qualifiedName { qn }
;

primitiveType:
	BOOLEAN { "boolean" }
	| CHAR { "char" }
	| BYTE { "byte" }
	| SHORT { "short" }
	| INT { "int" }
	| LONG { "long" }
	| FLOAT { "float" }
	| DOUBLE { "double" }
	| VOID { "void" }
;

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

assignmentOperator:
	ASSIGN { "=" }
	| PEQUAL { "+=" }
	| MINUSEQUAL { "-=" }
	| MULEQUAL { "*=" }
	| DIVEQUAL { "/=" }
	| MODEQUAL { "%=" }
	| ANDEQUAL { "&=" }
	| OREQUAL { "|=" }
	| XOREQUAL { "^=" }
	| RSHIFTEQUAL { ">>=" }
	| LSHIFTEQUAL { "<<=" }
	| LOGSHIFTEQUAL { ">>>=" }
;

arithmeticUnaryOperator:
	PLUS { "+" }
	| MINUS { "-" }
;

logicalUnaryOperator:
	BNOT { "~" }
	| NOT { "!" }
;

%%
let parse_error s = 
	print_endline s;
	flush stdout
