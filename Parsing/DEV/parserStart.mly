%{
	open Printf
	open Lexing
	open Expressions
%}
/* starting point */
%start compilationUnit
%type <Expressions.statement> compilationUnit

%%
compilationUnit:
	s=block { s }
	| error { raise (JavaException "ERROR") }
;
/* block */
%public block:
	LCURL lvds=localVariableDeclAndStmts RCURL { ST_block(lvds) }
	| LCURL RCURL { ST_block(List.append [] [ST_empty]) }
;

localVariableDeclAndStmts:
	lvd=localVariableDeclOrStmt { []@[lvd] }
	| lvds=localVariableDeclAndStmts lvd=localVariableDeclOrStmt { lvds@[lvd] }
;

localVariableDeclOrStmt:
	lvd=localVariableDeclStmt { lvd } 
	| stmt=statement { stmt }
;

%public localVariableDeclStmt:
	ts=typeSpecifier vd=variableDeclarators SEMI { ST_var_decl(None,ts,vd) }
	| FINAL ts=typeSpecifier vd=variableDeclarators SEMI { ST_var_decl(Some("final "),ts,vd) }
;

/* variable declarators */
variableDeclarators: 
 	vd=variableDeclarator { []@[vd] }
	| vds=variableDeclarators COMM vd=variableDeclarator { vds@[vd] }
;

variableDeclarator:
	dn=declaratorName { EX_Var_decl(dn, None) }
	| dn=declaratorName ASSIGN vi=variableInitializer { EX_Var_decl(dn,Some(vi)) }
;

declaratorName: 
	id=IDENTIFIER { Identifier(id) }
;

variableInitializer:
	ex=expression { []@[ex] }
	| LCURL RCURL { [] } /* AAAAAAAAAH */
	| LCURL arri=arrayInitializers RCURL { arri }
;

arrayInitializers:
	vi=variableInitializer { vi }
	| ai=arrayInitializers COMM vi=variableInitializer  { ai@vi }
	| ai=arrayInitializers COMM { ai }
;
/* end variable declarators */

/* allocations */
newAllocationExpression:
	pall=plainNewAllocationExpression { EX_New_alloc(None, pall) }
	| qn=qualifiedName DOT pall=plainNewAllocationExpression { EX_New_alloc(Some(qn),pall) }
;

plainNewAllocationExpression:
	arrall=arrayAllocationExpression { arrall }
    	| call=classAllocationExpression { call }
    	| arrall=arrayAllocationExpression LCURL RCURL { arrall }
    	| call=classAllocationExpression LCURL RCURL { call }
    	| arrall=arrayAllocationExpression LCURL arri=arrayInitializers RCURL { EX_Plain_array_alloc(arrall,arri) }
    	(*| call=classAllocationExpression LCURL fdec=fieldDeclarations RCURL { call^"{"^fdec^"}" }*)
;

classAllocationExpression:
	NEW tn=typeName LPAR args=argumentList RPAR { EX_Class_alloc(tn,Some(args)) }
	| NEW tn=typeName LPAR RPAR { EX_Class_alloc(tn, None) }
;

argumentList:
	ex=expression { []@[ex] }
	| args=argumentList COMM ex=expression { args@[ex] }
;

arrayAllocationExpression:
	NEW tn=typeName de=dimExprs d=dims { EX_Array_alloc(tn,Some(de),Some(d)) }
	| NEW tn=typeName de=dimExprs { EX_Array_alloc(tn,Some(de),None) }
    | NEW tn=typeName d=dims { EX_Array_alloc(tn,None,Some(d)) }
;

dimExprs:
	de=dimExpr { []@[de] }
	| ds=dimExprs d=dimExpr { ds@[d] }
;

dimExpr:
	LBRAC ex=expression RBRAC { ex }
;
/* end allocations */

%public primaryExpression:
	qn=qualifiedName { Identifier(qn) }
	| njs=notJustName { njs }
;

%public notJustName:
	spn=specialName { Identifier(spn) }
	| all=newAllocationExpression { all }
	| cpri=complexPrimary { cpri }
;
/* end typeName */

complexPrimary:
	LPAR ex=expression RPAR { ex }
	| cprin=complexPrimaryNoParenthesis { cprin }
;

%public complexPrimaryNoParenthesis:
	stlit=STRLIT { Literal(L_Str stlit) }
	| blit=BOOLEANLIT { Literal(L_Boolean blit) }
	| ilit=INTLIT { Literal(L_Int ilit) }
	| clit=CHARLIT { Literal(L_Char clit) }
	| dlit=DOUBLELIT { Literal(L_Double dlit) }
	| flit=FLOATLIT { Literal(L_Float flit) }
	| nlit=NULLLIT { Literal(L_Null) }
	| aa=arrayAccess { aa }
	| fa=fieldAccess { fa }
	| mc=methodCall { mc }
;

arrayAccess
	: qn=qualifiedName LBRAC e=expression RBRAC { EX_Array_access(Identifier(qn),e) }
	| cp=complexPrimary LBRAC e=expression RBRAC { EX_Array_access(cp,e) }
	;

fieldAccess
	: njn=notJustName DOT id=IDENTIFIER { EX_Field_access(njn, Some(Identifier(id))) }
	| rpe=realPostfixExpression DOT id=IDENTIFIER { EX_Field_access(rpe, Some(Identifier(id))) }
    | qn=qualifiedName DOT THIS { EX_Field_access(Identifier(qn), Some(Identifier("this "))) }
    | qn=qualifiedName DOT CLASS { EX_Field_access(Identifier(qn), Some(Identifier("class "))) }
    | pt=primitiveType DOT CLASS { EX_Field_access(EX_Primitive(pt, None), Some(Identifier("class "))) }
	;

methodCall
	: ma=methodAccess LPAR al=argumentList RPAR { EX_Method_access(ma,al)}
	| ma=methodAccess LPAR RPAR { EX_Method_access(ma, []) }
	;

methodAccess
	: cpnp=complexPrimaryNoParenthesis { cpnp }
	| sn=specialName { Identifier(sn) }
	| qn=qualifiedName { Identifier(qn) }
	;

specialName:
	THIS { "this" }
	| SUPER { "super" }
	/* | NULLLIT { "null" } */
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
