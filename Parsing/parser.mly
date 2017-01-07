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

/* dimensions */
%token DIM /* [ ] */

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
	/* | error { " an error has occured\n" } */
;
/* block */
block:
	LCURL lvds=localVariableDeclAndStmts RCURL { "{\n"^lvds^"\n}\n" }
	| LCURL RCURL { "{\n \n}\n" }
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
	ts=typeName vd=variableDeclarators SEMI { ts^vd^";" }
	| FINAL ts=typeName vd=variableDeclarators SEMI { "final "^ts^" "^vd^";" }
;

/* statements */
statement:
	es=emptyStmt { es }
	| ls=labelStmt { ls }
	| exs=expressionStmt SEMI { exs^";\n"}
 	| ss=selectStmt { ss }
	| is=iterStmt { is }
	| js=jumpStmt { js }
	| gs=guardingStmt { gs }
	| b=block { b }
	| error { " an error has occured\n" }

labelStmt:
	id=IDENTIFIER COL { id^" : " }
	| CASE ce=constantExpression COL { "case "^ce^": " }
	| DEFAULT COL { "default : " }
;

expressionStmt:
	e=expression { e }
;

selectStmt:
	IF LPAR e=expression RPAR s=statement { "if("^e^") "^s }
	| IF LPAR e=expression RPAR s1=statement ELSE s2=statement { "if("^e^") "^s1^"\nelse "^s2 }
	| SWITCH LPAR e=expression RPAR b=block { "switch ("^e^") "^b } 
;

jumpStmt: 
	BREAK id=IDENTIFIER SEMI { "break "^id^"; " }
	| BREAK SEMI { "break;" }
    | CONTINUE id=IDENTIFIER SEMI { "continue "^id^"; "}
	| CONTINUE SEMI { "continue;"}
	| RETURN e=expression SEMI { "return "^e^"; "  }
	| RETURN SEMI { "return;"} 
	| THROW e=expression SEMI { "throw "^e^"; " }
;

iterStmt: 
	WHILE LPAR e=expression RPAR s=statement { "while("^e^")"^s }
	| DO s=statement WHILE LPAR e=expression RPAR SEMI { "do "^s^" while ("^e^"); "} 
	| FOR LPAR fi=forInit fe=forExpr fin=forIncr RPAR s=statement { "for("^fi^fe^fin^")"^s }
	| FOR LPAR fi=forInit fe=forExpr RPAR s=statement { "for("^fi^fe^")"^s } 
	/* TODO add a foreach */
;

forInit: 
	lvds=localVariableDeclStmt { lvds }
	| SEMI { ";" }
;

forExpr: 
	e=expression SEMI { e^";" }
	| SEMI { ";" }
;


forIncr: 
	es=expressionStmts { es }
;

expressionStmts
	: es=expressionStmt { es }
	| ess=expressionStmts COMM es=expressionStmt { ess^" , "^es}
	;

guardingStmt: 
	SYNCHRONIZED LPAR e=expression RPAR s=statement { "synchronized ("^e^") "^s }
	| TRY b=block f=finally { "try "^b^f }
	| TRY b=block c=catches { "try "^b^c }
	| TRY b=block c=catches f=finally { "try "^b^c^f }
;
/* end statements */

/* catch */
catches: 
	c=catch { c } 
	| cs=catches c=catch { cs^c }
;

catch: 
	ch=catchHeader b=block { ch^b }
;

catchHeader: 
	CATCH LPAR ts=typeName id=IDENTIFIER RPAR { "catch ( "^ts^id^" ) "}
	| CATCH LPAR ts=typeName RPAR { "catch ( "^ts^" ) " }
;

finally: 
	FINALLY b=block { "finally "^b }
;
/* end catch */

/* variable declarators */
variableDeclarators: 
 	vd=variableDeclarator { vd }
	| vds=variableDeclarators COMM vd=variableDeclarator { vds^" , "^vd }
;

variableDeclarator:
	dn=declaratorName { dn }
	| dn=declaratorName ASSIGN vi=variableInitializer { dn^" = "^vi }
;

declaratorName: 
	id=IDENTIFIER { id }
;

variableInitializer:
	ex=expression { ex }
	| LCURL RCURL { "{"^"}" }
	/* | LCURL arri=arrayInitializers RCURL { "{"^arri^"}" } */
;
/* end variable declarators */
emptyStmt:
	SEMI { ";\n" }
;

/* expressions */
expression: 
	/* { " |some expression| " } */
	/* pe=primaryExpression { pe } */
	ce=conditionalExpression { ce }
;


constantExpression:
	/* { " |some constant expression| " } */
	ce=conditionalExpression { ce }
;

conditionalExpression:
	cor=conditionalOrExpression { cor }
	/* | cor=conditionalOrExpression QM ex=expression COL ce=conditionalExpression { cor^" ? "^ex^" : "^ce } */
;

conditionalOrExpression:
	cand=conditionalAndExpression { cand }
	/* | cor=conditionalOrExpression OR cand=conditionalAndExpression { cor^"OR"^cand } */
;

conditionalAndExpression:
	ior=inclusiveOrExpression { ior }
	/* | cand=conditionalAndExpression AND ior=inclusiveOrExpression { cand^"AND"^ior } */
;

inclusiveOrExpression:
	eor=exclusiveOrExpression { eor }
	/* | ior=inclusiveOrExpression BOR eor=exclusiveOrExpression { ior^"|"^eor } */
;

exclusiveOrExpression:
	a=andExpression { a }
	/* | eor=exclusiveOrExpression XOR a=andExpression { eor^"^"^a } */
;

andExpression:
	eq=equalityExpression { eq }
	/* | a=andExpression BAND eq=equalityExpression { a^"BAND"^eq } */
;

equalityExpression:
	rel=relationalExpression { rel }
	/* | eq=equalityExpression EQUAL rel=relationalExpression { eq^"=="^rel }
	| eq=equalityExpression NEQUAL rel=relationalExpression { eq^"!="^rel } */
;

relationalExpression:
	sh=shiftExpression { sh }
	/* | rel=relationalExpression LTHAN sh=shiftExpression { rel^"<"^sh }
	| rel=relationalExpression GTHAN sh=shiftExpression { rel^">"^sh }
	| rel=relationalExpression LETHAN sh=shiftExpression { rel^"<="^sh }
	| rel=relationalExpression GETHAN sh=shiftExpression { rel^">="^sh } */
	(* instance of dafuq *)
;

shiftExpression:
	add=additiveExpression { add }
	| sh=shiftExpression LSHIFT add=additiveExpression { sh^"<<"^add }
	| sh=shiftExpression RSHIFT add=additiveExpression { sh^">>"^add }
	| sh=shiftExpression LOGSHIFT add=additiveExpression { sh^">>>"^add }
	/* all shifts work */ 
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
	| LPAR e=expression RPAR lue=logicalUnaryExpression { " ("^e^") "^lue }
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

/* typeName */
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

qualifiedName:
	id=IDENTIFIER { id }
	| qn=qualifiedName DOT id=IDENTIFIER { qn^"."^id }
;

notJustName:
	spn=specialName { spn }
	/* | all=newAllocationExpression { all } */
	| cpri=complexPrimary { cpri }
;

complexPrimary:
	LPAR ex=expression RPAR { "("^ex^")" }
	| cprin=complexPrimaryNoParenthesis { cprin }
;

complexPrimaryNoParenthesis:
	stlit=STRLIT { stlit }
	| blit=BOOLEANLIT { string_of_bool blit }
	| ilit=INTLIT { string_of_int ilit }
	| clit=CHARLIT { "'"^(String.make 1 clit)^"'" }
(* for now they are strings *)
;

specialName:
	THIS { "this" }
	| SUPER { "super" }
	| NULLLIT { "null" }
;

/* operators */
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
