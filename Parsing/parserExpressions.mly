%%

/* expressions */
%public expression: 
	ae=assignmentExpression { ae }
	| error { " ERROR -> expression\n" }
;

assignmentExpression:
	ce=conditionalExpression { ce }
	| ue=unaryExpression ass=assignmentOperator ae=assignmentExpression { ue^ass^ae }
;

/* conditional expressions */
%public conditionalExpression:
	cor=conditionalOrExpression { cor }
	| cor=conditionalOrExpression QM ex=expression COL ce=conditionalExpression { cor^" ? "^ex^" : "^ce }
;

conditionalOrExpression:
	cand=conditionalAndExpression { cand }
	| cor=conditionalOrExpression OR cand=conditionalAndExpression { cor^" || "^cand }
;

conditionalAndExpression:
	ior=inclusiveOrExpression { ior }
	| cand=conditionalAndExpression AND ior=inclusiveOrExpression { cand^" && "^ior }
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
	| eq=equalityExpression EQUAL rel=relationalExpression { eq^" == "^rel }
	| eq=equalityExpression NEQUAL rel=relationalExpression { eq^" != "^rel }
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
	| sh=shiftExpression LSHIFT add=additiveExpression { sh^" << "^add }
	| sh=shiftExpression RSHIFT add=additiveExpression { sh^" >> "^add }
	| sh=shiftExpression LOGSHIFT add=additiveExpression { sh^" >>> "^add }
;

additiveExpression:
	mul=multiplicativeExpression { mul }
	| add=additiveExpression PLUS mul=multiplicativeExpression { add^" + "^mul } 
	| add=additiveExpression MINUS mul=multiplicativeExpression { add^" - "^mul }
;

multiplicativeExpression:
	cast=castExpression { cast }
	| mul=multiplicativeExpression MUL cast=castExpression { mul^" * "^cast }
	| mul=multiplicativeExpression DIV cast=castExpression { mul^" / "^cast }
	| mul=multiplicativeExpression MOD cast=castExpression { mul^" % "^cast }
	
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

%public realPostfixExpression:
	post=postfixExpression INCREMENT { post^"++" }
	| post=postfixExpression DECREMENT { post^"--" }
;
/* end operationg expressions */

%%