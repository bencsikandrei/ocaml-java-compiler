%{
%}

%%

/* statements */
%public statement:
	es=emptyStmt { es }
	| ls=labelStmt { ls }
	| ass=assertStmt { ass }
	| exs=expressionStmt SEMI { exs^";\n"}
 	| ss=selectStmt { ss }
	| js=jumpStmt { js }
	| is=iterStmt { is }
	| gs=guardingStmt { gs }
	| b=block { b }
;

emptyStmt:
	SEMI { ";\n" }
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

expressionStmts:
	es=expressionStmt { es }
	| ess=expressionStmts COMM es=expressionStmt { ess^" , "^es}
;

guardingStmt: 
	SYNCHRONIZED LPAR e=expression RPAR s=statement { "synchronized ("^e^") "^s }
	| TRY b=block f=finally { "try "^b^f }
	| TRY b=block c=catches { "try "^b^c }
	| TRY b=block c=catches f=finally { "try "^b^c^f }
;

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

constantExpression:
	ce=conditionalExpression { ce }
;

%%