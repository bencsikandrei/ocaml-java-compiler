%{
	open Printf
	open Lexing
	open Expressions
%}
/* starting point */
%start compilationUnit
%type <string> compilationUnit

%%
compilationUnit:
	s=block { s }
	| error { "ERROR -> statement" }
;
/* block */
%public block:
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

%public localVariableDeclStmt:
	ts=typeSpecifier vd=variableDeclarators SEMI { ts^" "^vd^";" }
	| FINAL ts=typeSpecifier vd=variableDeclarators SEMI { "final "^ts^" "^vd^";" }
;

/* variable declarators */
variableDeclarators: 
 	vd=variableDeclarator { vd }
	| vds=variableDeclarators COMM vd=variableDeclarator { vds^", "^vd }
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
	NEW tn=typeName LPAR args=argumentList RPAR { "new "^tn^"("^args^")" }
	| NEW tn=typeName LPAR RPAR { "new "^tn^"("^")" }
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
/* end allocations */

%public primaryExpression:
	qn=qualifiedName { qn }
	| njs=notJustName {njs }
;

/* typeName */
typeName:
	pri=primitiveType { pri }
	| qn=qualifiedName { qn }
;

%public typeSpecifier:
	tn=typeName { tn }
	| tn=typeName ds=dims { tn^ds }
;

%public qualifiedName:
	id=IDENTIFIER { id }
	| qn=qualifiedName DOT id=IDENTIFIER { qn^"."^id }
;

%public notJustName:
	spn=specialName { spn }
	| all=newAllocationExpression { all }
	| cpri=complexPrimary { cpri }
;
/* end typeName */

complexPrimary:
	LPAR ex=expression RPAR { "("^ex^")" }
	| cprin=complexPrimaryNoParenthesis { cprin }
;

%public complexPrimaryNoParenthesis:
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

/* modifiers */
%public modifiers: 
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
