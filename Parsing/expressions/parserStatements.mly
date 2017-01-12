%{
	open Expressions
%}

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
	ASSERT e=expression SEMI { ST_assert(e, None) }
	| ASSERT e1=expression COL e2=expression SEMI { ST_assert(e1, Some(e2)) }
;

expressionStmt:
	e=expression { ST_expression(e) }
;

selectStmt:
	IF LPAR e=expression RPAR s=statement %prec DANGLING_ELSE { ST_if(e, s, None) }
	| IF LPAR e=expression RPAR s1=statement ELSE s2=statement { ST_if(e, s1, Some(s2)) }
	| SWITCH LPAR e=expression RPAR sb=switchBlock { ST_switch(e, sb) }
;

/* switch blocks */
switchBlock:
	LCURL RCURL { ST_empty } /* Empty */
	| sbsgs=switchBlockStmtGroups { ST_block(sbsgs) } /* sw_block */
	| LCURL sbsgs=switchBlockStmtGroups RCURL { ST_block(sbsgs) } /* sw_block */
;

switchBlockStmtGroups:
	sbsg=switchBlockStmtGroup { []@[sbsg] }
	| sbsgs=switchBlockStmtGroups sbsg=switchBlockStmtGroup { sbsgs@[sbsg] } /* concatenate two lists @ */
;

statementCase:
	es=emptyStmt { ST_empty }
	/*
	| ls=labelStmt { ls }
	| ass=assertStmt { ass }
	| exs=expressionStmt SEMI { exs }
 	| ss=selectStmt { ss }
 	*/
	| js=jumpStmt { js }
	/* | is=iterStmt { is }
	| gs=guardingStmt { gs } */
;

switchBlockStmtGroup:
	sls=switchLabels bss=block { ST_case(sls,bss) } /* nonempty_list  case_block */
	/* | sls=switchLabels bss=statementCase { ST_empty (* ST_case(sls,bss) *) } */
;

switchLabels:
	sl=switchLabel { []@[sl] }
	| sls=switchLabels sl=switchLabel { sls@[sl] }
;

switchLabel:
	CASE ce=constantExpression COL { EX_Case(ce) } /* Case */
	| DEFAULT COL { EX_Default }
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
	| DO s=statement WHILE LPAR e=expression RPAR SEMI { ST_do_while((List.append [] [s]),e) } 
	| FOR LPAR fi=forInit fe=forExpr fin=forIncr RPAR s=statement { ST_for(fi,fe,fin, s) }
	| FOR LPAR fi=forInit fe=forExpr RPAR s=statement { ST_for(fi,fe,[],s) } 
	| FOR LPAR fvo=forVarOpt COL e=expression RPAR s=statement { ST_efor(fvo,e,s) }
	/* TODO add a foreach */
;

forInit: 
	lvds=localVariableDeclStmt { []@[lvds] }
	| SEMI { ST_empty::[] }
;

forExpr: 
	e=expression SEMI { e }
	| SEMI { Identifier(";") } /* not supposed to be ident */
;

forIncr: 
	es=expressionStmts { es }
;

forVarOpt:
	ts=types id=IDENTIFIER { Enhanced_for(ts,id) }
	/*| ms=modifiers ts=types id=IDENTIFIER { ms^" "^ts^" "^id }
*/
;

expressionStmts:
	es=expressionStmt { []@[es] }
	| ess=expressionStmts COMM es=expressionStmt { ess@[es] }
;

guardingStmt: 
	SYNCHRONIZED LPAR e=expression RPAR s=statement { ST_synch(e,s) }
	| TRY b=block f=finally { ST_try(b,[],f) }
	| TRY b=block c=catch { ST_try(b,[]@[c],ST_empty) }
	| TRY b=block c=catch f=finally { ST_try(b,[]@[c],f) }
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
	CATCH LPAR ts=types id=IDENTIFIER RPAR { Catch_header(ts,id) }
	| CATCH LPAR ts=types RPAR { Catch_header(ts,"") }
;

finally: 
	FINALLY b=block { ST_finally(b) }
;
/* end catch */

constantExpression:
	ce=conditionalExpression { ce }
;
%%