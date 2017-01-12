%{
	open Printf
	open Lexing
	open Expressions
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
%nonassoc DANGLING_ELSE
%nonassoc ELSE

/* starting point */
%start compilationUnit
%type <string> compilationUnit

%%
compilationUnit:
	s=block { s }
	| error { "ERROR -> statement" }
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
	ts=typeSpecifier vd=variableDeclarators SEMI { ts^vd^";" }
	| FINAL ts=typeSpecifier vd=variableDeclarators SEMI { "final "^ts^" "^vd^";" }
;

/* statements */
statement:
	es=emptyStmt { es }
	| ls=labelStmt { ls }
	| ass=assertStmt { ass }
	| exs=expressionStmt SEMI { exs^";\n"}
 	| ss=selectStmt { ss }
	| is=iterStmt { is }
	| js=jumpStmt { js }
	| gs=guardingStmt { gs }
	| b=block { b }
;

labelStmt:
	id=IDENTIFIER COL { id^" : " }
	/* | CASE ce=constantExpression COL { "case "^ce^": " }
	| DEFAULT COL { "default : " } */
;

assertStmt:
	ASSERT e=expression SEMI { "assert "^e }
	| ASSERT e1=expression COL e2=expression SEMI { "assert "^e1^" : "^e2 }
;

expressionStmt:
	e=expression { e }
;

selectStmt:
	IF LPAR e=expression RPAR s=statement %prec DANGLING_ELSE { "if("^e^") "^s }
	| IF LPAR e=expression RPAR s1=statement ELSE s2=statement { "if("^e^") "^s1^"\nelse "^s2 }
	| SWITCH LPAR e=expression RPAR sb=switchBlock { "switch ("^e^") "^sb }
;

/* switch blocks */
switchBlock:
	LCURL RCURL { "{ }" }
	| LCURL sbsgs=switchBlockStmtGroups RCURL { "{ "^sbsgs^"}" }
;

switchBlockStmtGroups:
	sbsg=switchBlockStmtGroup { sbsg }
	| sbsgs=switchBlockStmtGroups sbsg=switchBlockStmtGroup { sbsgs^"\n"^sbsg }
;

switchBlockStmtGroup:
	sls=switchLabels bss=block { sls^"\n"^bss }
;

switchLabels:
	sl=switchLabel { sl }
	| sls=switchLabels sl=switchLabel { sls^"\n"^sl }
;

switchLabel:
	CASE ce=constantExpression COL { "case "^ce^" :" }
	| DEFAULT COL { "default : " }
;
/* end switch blocks */

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
	| FOR LPAR fvo=forVarOpt COL e=expression RPAR s=statement { "for("^fvo^":"^e^")"^s }
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

forVarOpt:
	ts=typeSpecifier id=IDENTIFIER { ts^" "^id }
	/* | ms=modifiers ts=typeSpecifier id=IDENTIFIER { ms^" "^ts^" "^id } */
	/* TODO add modifiers here */
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
	CATCH LPAR ts=typeSpecifier id=IDENTIFIER RPAR { "catch ( "^ts^id^" ) "}
	| CATCH LPAR ts=typeSpecifier RPAR { "catch ( "^ts^" ) " }
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
	| LCURL RCURL { " { "^" } " }
	| LCURL arri=arrayInitializers RCURL { " { "^arri^" } " }
;

arrayInitializers:
	vi=variableInitializer { vi }
	| ai=arrayInitializers COMM vi=variableInitializer  { ai^" , "^vi }
	| ai=arrayInitializers COMM { ai^" , " }
;
/* end variable declarators */

emptyStmt:
	SEMI { ";\n" }
;

/* expressions */
expression: 
	ae=assignmentExpression { ae }
	| error { " an error has occured\n" }
;

assignmentExpression:
	ce=conditionalExpression { ce }
	| ue=unaryExpression ass=assignmentOperator ae=assignmentExpression { ue^ass^ae }
;

constantExpression:
	ce=conditionalExpression { ce }
;

/* conditional expressions */
conditionalExpression:
	cor=conditionalOrExpression { cor }
	| cor=conditionalOrExpression QM ex=expression COL ce=conditionalExpression { cor^" ? "^ex^" : "^ce }
;

conditionalOrExpression:
	cand=conditionalAndExpression { cand }
	| cor=conditionalOrExpression OR cand=conditionalAndExpression { cor^" || "^cand }
;

conditionalAndExpression:
	ior=inclusiveOrExpression { ior }
	| cand=conditionalAndExpression AND ior=inclusiveOrExpression { cand^"  && "^ior }
;

inclusiveOrExpression:
	eor=exclusiveOrExpression { eor }
	| ior=inclusiveOrExpression BOR eor=exclusiveOrExpression { ior^" | "^eor }
;

exclusiveOrExpression:
	a=andExpression { a }
	| eor=exclusiveOrExpression XOR a=andExpression { eor^" ^ "^a }
;

andExpression:
	eq=equalityExpression { eq }
	| a=andExpression BAND eq=equalityExpression { a^" & "^eq }
;

equalityExpression:
	rel=relationalExpression { rel }
	| eq=equalityExpression EQUAL rel=relationalExpression { eq^"=="^rel }
	| eq=equalityExpression NEQUAL rel=relationalExpression { eq^"!="^rel }
;

relationalExpression:
	sh=shiftExpression { sh }
	| rel=relationalExpression LTHAN sh=shiftExpression { rel^" < "^sh }
	| rel=relationalExpression GTHAN sh=shiftExpression { rel^" > "^sh }
	| rel=relationalExpression LETHAN sh=shiftExpression { rel^" <= "^sh }
	| rel=relationalExpression GETHAN sh=shiftExpression { rel^" >= "^sh } 
	| rel=relationalExpression INSTANCEOF ts=typeSpecifier { rel^" instanceof "^ts }
;
/* end conditional expressions */


/* operation expressions */
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
	| LPAR pte=primitiveTypeExpression RPAR ce=castExpression { " ("^pte^") "^ce }
	| LPAR cte=classTypeExpression RPAR ce=castExpression { " ("^cte^") "^ce }
	| LPAR e=expression RPAR lue=logicalUnaryExpression { " ("^e^") "^lue }
;

primitiveTypeExpression: 
	pt=primitiveType { pt }
    | pt=primitiveType d=dims { pt^d } 
;

classTypeExpression: 
	qn=qualifiedName d=dims { qn^d }
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
/* end operationg expressions */

/* allocations */
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

classAllocationExpression:
	NEW tn=typeName LPAR args=argumentList RPAR { "new"^tn^"("^args^")" }
	| NEW tn=typeName LPAR RPAR { "new"^tn^"("^")" }
;

argumentList:
	ex=expression { ex }
	| args=argumentList COMM ex=expression { args^" , "^ex }
;

arrayAllocationExpression:
	NEW tn=typeName de=dimExprs d=dims { "new "^tn^de^d }
	| NEW tn=typeName de=dimExprs { "new "^tn^de }
    | NEW tn=typeName d=dims { "new "^tn^d }
;

dimExprs:
	de=dimExpr { de }
	| ds=dimExprs d=dimExpr { ds^d }
;

dimExpr:
	LBRAC ex=expression RBRAC { "["^ex^"]" }
;

dims:
	DIM { " [ ] " }
	| ds=dims DIM { ds^" [ ] " }
;
/* end allocations */

primaryExpression:
	qn=qualifiedName { qn }
	| njs=notJustName {njs }
;

/* typeName */
typeName:
	pri=primitiveType { pri }
	| qn=qualifiedName { qn }
;

typeSpecifier:
	tn=typeName { tn }
	| tn=typeName ds=dims { tn^ds }
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
/* end typeName */

complexPrimary:
	LPAR ex=expression RPAR { "("^ex^")" }
	| cprin=complexPrimaryNoParenthesis { cprin }
;

complexPrimaryNoParenthesis:
	stlit=STRLIT { stlit }
	| blit=BOOLEANLIT { string_of_bool blit }
	| ilit=INTLIT { string_of_int ilit }
	| clit=CHARLIT { "'"^(String.make 1 clit)^"'" }
	| dlit=DOUBLELIT { string_of_float dlit }
	| flit=FLOATLIT { string_of_float flit }
	/* | nlit=NULLLIT { nlit } */
	| aa=arrayAccess { aa }
	| fa=fieldAccess { fa }
	| mc=methodCall { mc } 
(* for now they are strings *)
;

arrayAccess
	: qn=qualifiedName LBRAC e=expression RBRAC { qn^" [ "^e^" ] " }
	| cp=complexPrimary LBRAC e=expression RBRAC { cp^" [ "^e^" ] "}
	;

fieldAccess
	: njn=notJustName DOT id=IDENTIFIER { njn^"."^id }
	| rpe=realPostfixExpression DOT id=IDENTIFIER { rpe^"."^id }
    | qn=qualifiedName DOT THIS { qn^".this " }
    | qn=qualifiedName DOT CLASS { qn^".class " }
    | pt=primitiveType DOT CLASS { pt^".class " }
	;

methodCall
	: ma=methodAccess LPAR al=argumentList RPAR { ma^"( "^al^" ) "}
	| ma=methodAccess LPAR RPAR { ma^"(  ) "}
	;

methodAccess
	: cpnp=complexPrimaryNoParenthesis { cpnp }
	| sn=specialName { sn }
	| qn=qualifiedName { qn }
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

/* types */
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

/* modifiers */
modifiers: 
	m=modifier { m }
	| ms=modifiers m=modifier { ms^m }
;

modifier: 
	ABSTRACT { "abstract " }
	| FINAL { "final " }
	| PUBLIC { "public " }
	| PROTECTED { "protected " }
	| PRIVATE { "private " }
	| STATIC { "static " }
	| TRANSIENT { "transient " }
	| VOLATILE { "volatile " }
	| NATIVE { "native " }
	| SYNCHRONIZED { "synchronized " }
;

%%
let parse_error s = 
	print_endline s;
	flush stdout
