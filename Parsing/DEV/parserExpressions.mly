%{
	open Expressions
%}

%start expression
%type < Expressions.expression > expression

%%

/* expressions */
%public expression: 
	ae=assignmentExpression { ae }
	| error { Identifier(" ERROR -> expression\n") }
;

assignmentExpression:
	ce=conditionalExpression { ce }
	| ue=unaryExpression ass=assignmentOperator ae=assignmentExpression { EX_Assign(ass,ue,ae) }
;

/* conditional expressions */
%public conditionalExpression:
	cor=conditionalOrExpression { cor }
	| cor=conditionalOrExpression QM ex=expression COL ce=conditionalExpression { EX_Ternary(cor,ex,ce) }
;

conditionalOrExpression:
	cand=conditionalAndExpression { cand }
	| cor=conditionalOrExpression OR cand=conditionalAndExpression { EX_Logbinop(LBO_or,cor, cand) }
;

conditionalAndExpression:
	ior=inclusiveOrExpression { ior }
	| cand=conditionalAndExpression AND ior=inclusiveOrExpression { EX_Logbinop(LBO_and,cand, ior) }
;

inclusiveOrExpression:
	eor=exclusiveOrExpression { eor }
	| ior=inclusiveOrExpression BOR eor=exclusiveOrExpression { EX_Bitop(SO_Or,ior,eor) }
;

exclusiveOrExpression:
	a=andExpression { a }
	| eor=exclusiveOrExpression XOR a=andExpression { EX_Bitop(SO_Xor,eor,a) }
;

andExpression:
	eq=equalityExpression { eq }
	| a=andExpression BAND eq=equalityExpression { EX_Bitop(SO_And,a,eq) }
;

equalityExpression:
	rel=relationalExpression { rel }
	| eq=equalityExpression EQUAL rel=relationalExpression { EX_Compop(BO_eq,eq,rel) }
	| eq=equalityExpression NEQUAL rel=relationalExpression { EX_Compop(BO_neq,eq,rel) }
;

relationalExpression:
	sh=shiftExpression { sh }
	| rel=relationalExpression LTHAN sh=shiftExpression { EX_Compop(BO_lt,rel,sh) }
	| rel=relationalExpression GTHAN sh=shiftExpression { EX_Compop(BO_gt,rel,sh) }
	| rel=relationalExpression LETHAN sh=shiftExpression { EX_Compop(BO_le,rel,sh) }
	| rel=relationalExpression GETHAN sh=shiftExpression { EX_Compop(BO_ge,rel,sh) } 
	| rel=relationalExpression INSTANCEOF ts=typeSpecifier { EX_Instanceof(BO_instanceof,rel,ts) }
;
/* end conditional expressions */

/* operation expressions */
shiftExpression:
	add=additiveExpression { add }
	| sh=shiftExpression LSHIFT add=additiveExpression { EX_Bitop(SO_lshift, sh, add) }
	| sh=shiftExpression RSHIFT add=additiveExpression { EX_Bitop(SO_rshift, sh, add) }
	| sh=shiftExpression LOGSHIFT add=additiveExpression { EX_Bitop(SO_logshift, sh, add) }
;

additiveExpression:
	mul=multiplicativeExpression { mul }
	| add=additiveExpression PLUS mul=multiplicativeExpression { EX_Binop(BO_Add,add, mul) } 
	| add=additiveExpression MINUS mul=multiplicativeExpression { EX_Binop(BO_Minus,add, mul) }
;

multiplicativeExpression:
	cast=castExpression { cast }
	| mul=multiplicativeExpression MUL cast=castExpression { EX_Binop(BO_Mul,mul,cast) }
	| mul=multiplicativeExpression DIV cast=castExpression { EX_Binop(BO_Div,mul,cast) }
	| mul=multiplicativeExpression MOD cast=castExpression { EX_Binop(BO_Mod,mul,cast) }
	
;

castExpression:
	un=unaryExpression { un }
	| LPAR pte=primitiveTypeExpression RPAR ce=castExpression { EX_Cast(pte, ce) }
	| LPAR cte=classTypeExpression RPAR ce=castExpression { EX_Cast(cte, ce) }
	| LPAR e=expression RPAR lue=logicalUnaryExpression { EX_Cast(e, lue) }
;

primitiveTypeExpression: 
	pt=primitiveType { EX_Primitive(pt,"") }
    | pt=primitiveType d=dims { EX_Primitive(pt,d) } 
;

classTypeExpression: 
	qn=qualifiedName d=dims { EX_Class(qn^d) }
;

unaryExpression:
	INCREMENT un=unaryExpression { EX_Unop(UO_Increment,un) }
	| DECREMENT un=unaryExpression { EX_Unop(UO_Decrement,un) } 
	| aop=arithmeticUnaryOperator ct=castExpression { EX_Unop(aop,ct) }
	| logu=logicalUnaryExpression { logu }
;

logicalUnaryExpression:
	post=postfixExpression { post }
	| loguop=logicalUnaryOperator un=unaryExpression { EX_Loguop(loguop,un) }
;

postfixExpression:
	pri=primaryExpression { pri }
	| rpost=realPostfixExpression { rpost }
;

%public realPostfixExpression:
	post=postfixExpression INCREMENT { EX_Postfix(UO_Increment, post) }
	| post=postfixExpression DECREMENT { EX_Postfix(UO_Decrement, post) }
;
/* end operationg expressions */

%%