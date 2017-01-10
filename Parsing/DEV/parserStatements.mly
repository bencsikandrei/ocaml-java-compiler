%{
	open Expressions
%}

%start statement
%type < Expressions.statement > statement

%%
/* statements */
%public statement:
	es=emptyStmt { ST_empty }
	| ls=labelStmt { ls }
	| ass=assertStmt { ass }
	| exs=expressionStmt SEMI { exs }
 	| ss=selectStmt { ss }
	| js=jumpStmt { js }
	| is=iterStmt { is }
	| gs=guardingStmt { gs }
	| b=block { b }
;

emptyStmt:
	SEMI { ST_empty }
;

labelStmt:
	id=IDENTIFIER COL { ST_label(id) }
	/* | CASE ce=constantExpression COL { "case "^ce^": " }
	| DEFAULT COL { "default : " } */
;

assertStmt:
	ASSERT e=expression SEMI { ST_assert(e) }
	| ASSERT e1=expression COL e2=expression SEMI { ST_expression(e1, Some(e2)) }
;

expressionStmt:
	e=expression { e }
;

selectStmt:
	IF LPAR e=expression RPAR s=statement %prec DANGLING_ELSE { ST_if(e, s, None) }
	| IF LPAR e=expression RPAR s1=statement ELSE s2=statement { ST_if(e, s1, Some(s2)) }
	| SWITCH LPAR e=expression RPAR sb=block { ST_switch(e, sb) }
;

/* switch blocks */
switchBlock:
	LCURL RCURL { Empty }
	| LCURL sbsgs=list(switchBlockStmtGroups) RCURL { Switch_block(sbsgs) }
;

switchBlockStmtGroups:
	sbsg=list(switchBlockStmtGroup) { sbsg }
	| sbsgs=nonempty_list(switchBlockStmtGroups) sbsg=switchBlockStmtGroup { sbsgs::sbsg }
;

switchBlockStmtGroup:
	sls=switchLabels bss=block { Case_block(sls,bss) }
;

switchLabels:
	sl=switchLabel { [sl] }
	| sls=switchLabels sl=switchLabel { [sls::sl] }
;

switchLabel:
	CASE ce=constantExpression COL { Case(ce) }
	| DEFAULT COL { Default }
;
/* end switch blocks */

jumpStmt: 
	BREAK id=IDENTIFIER SEMI { ST_break(id) }
	| BREAK SEMI { ST_break("") }
    | CONTINUE id=IDENTIFIER SEMI { ST_continue(id) }
	| CONTINUE SEMI { ST_continue("") }
	| RETURN e=expression SEMI { ST_return(e) }
	| RETURN SEMI { ST_return(Identifier("")) } /* not an identifier either */ 
	| THROW e=expression SEMI { ST_throw(e) }
;

iterStmt: 
	WHILE LPAR e=expression RPAR s=statement { ST_while(e,s) }
	| DO s=statement WHILE LPAR e=expression RPAR SEMI { ST_do_while(s,e) } 
	| FOR LPAR fi=forInit fe=forExpr fin=list(forIncr) RPAR s=statement { ST_for(fi,fe,fin, s) }
	| FOR LPAR fi=forInit fe=forExpr RPAR s=statement { ST_for(fi,fe,None,s) } 
	| FOR LPAR fvo=forVarOpt COL e=expression RPAR s=statement { ST_efor(fvo,e,s) }
	/* TODO add a foreach */
;

forInit: 
	lvds=localVariableDeclStmt { lvds }
	| SEMI { Identifier(";") }
;

forExpr: 
	e=expression SEMI { e }
	| SEMI { Identifier(";") } /* not supposed to be ident */
;

forIncr: 
	es=expressionStmts { es }
;

forVarOpt:
	ts=typeSpecifier id=IDENTIFIER { Enhanced_for(ts,id) }
	/*| ms=modifiers ts=typeSpecifier id=IDENTIFIER { ms^" "^ts^" "^id }
*/
;

expressionStmts:
	es=expressionStmt { ST_expression(es) }
	| ess=expressionStmts COMM es=expressionStmt { ST_expression(ess::es)}
;

guardingStmt: 
	SYNCHRONIZED LPAR e=expression RPAR s=statement { ST_synch(e,s) }
	| TRY b=block f=finally { ST_try(b,f) }
	| TRY b=block c=catches { ST_try(b,c) }
	| TRY b=block c=catches f=finally { ST_try(b,c,f) }
;

/* catch */
catches: 
	c=catch { c } 
	| cs=catches c=catch { cs^c }
;

catch: 
	ch=catchHeader b=block { ST_catch(ch,b) }
;

catchHeader: 
	CATCH LPAR ts=typeSpecifier id=IDENTIFIER RPAR { Catch_header(ts,id) }
	| CATCH LPAR ts=typeSpecifier RPAR { Catch_header(ts,"") }
;

finally: 
	FINALLY b=block { ST_finally(b) }
;
/* end catch */

constantExpression:
	ce=conditionalExpression { ce }
;
%%