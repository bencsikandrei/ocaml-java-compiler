%{
	open Expressions
%}

%%
/* statements */
%public statement:
	es=emptyStmt { ST_Empty }
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
	SEMI { ST_Empty }
;

labelStmt:
	id=IDENTIFIER COL { ST_Label(id) }
	/* | CASE ce=constantExpression COL { "case "^ce^": " }
	| DEFAULT COL { "default : " } */
;

assertStmt:
	ASSERT e=expression SEMI { ST_Assert(e, None) }
	| ASSERT e1=expression COL e2=expression SEMI { ST_Assert(e1, Some(e2)) }
;

expressionStmt:
	e=expression { ST_Expression(e) }
;

selectStmt:
	IF LPAR e=expression RPAR s=statement %prec DANGLING_ELSE { ST_If(e, s, None) }
	| IF LPAR e=expression RPAR s1=statement ELSE s2=statement { ST_If(e, s1, Some(s2)) }
	| SWITCH LPAR e=expression RPAR sb=switchBlock { ST_Switch(e, sb) }
;

/* switch blocks */
switchBlock:
	LCURL RCURL { ST_Empty } /* Empty */
	| sbsgs=switchBlockStmtGroups { ST_Block(sbsgs) } /* sw_block */
	| LCURL sbsgs=switchBlockStmtGroups RCURL { ST_Block(sbsgs) } /* sw_block */
;

switchBlockStmtGroups:
	sbsg=switchBlockStmtGroup { []@[sbsg] }
	| sbsgs=switchBlockStmtGroups sbsg=switchBlockStmtGroup { sbsgs@[sbsg] } /* concatenate two lists @ */
;

(* 
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
*)

switchBlockStmtGroup:
	sls=switchLabels bss=block { ST_Case(sls,bss) } /* nonempty_list  case_block */
	/* | sls=switchLabels bss=statementCase { ST_empty (* ST_case(sls,bss) *) } */
;

switchLabels:
	sl=switchLabel { []@[sl] }
	| sls=switchLabels sl=switchLabel { sls@[sl] }
;

switchLabel:
	CASE ce=constantExpression COL { EX_Case(ce) }
	| DEFAULT COL { EX_Default }
;
/* end switch blocks */

jumpStmt: 
	BREAK id=IDENTIFIER SEMI { ST_Break(id) }
	| BREAK SEMI { ST_Break("") }
    | CONTINUE id=IDENTIFIER SEMI { ST_Continue(id) }
	| CONTINUE SEMI { ST_Continue("") }
	| RETURN e=expression SEMI { ST_Return(e) }
	| RETURN SEMI { ST_Return(EX_Empty) } /* changed to empty */ 
	| THROW e=expression SEMI { ST_Throw(e) }
;

iterStmt: 
	WHILE LPAR e=expression RPAR s=statement { ST_While(e,s) }
	| DO s=statement WHILE LPAR e=expression RPAR SEMI { ST_Do_while((List.append [] [s]),e) } 
	| FOR LPAR fi=forInit fe=forExpr fin=forIncr RPAR s=statement { ST_For(fi,fe,fin, s) }
	| FOR LPAR fi=forInit fe=forExpr RPAR s=statement { ST_For(fi,fe,[],s) } 
	| FOR LPAR fvo=forVarOpt COL e=expression RPAR s=statement { ST_Efor(fvo,e,s) }
	/* TODO add a foreach */
;

forInit: 
	lvds=localVariableDeclStmt { []@[lvds] }
	| SEMI { ST_Empty::[] }
;

forExpr: 
	e=expression SEMI { e }
	| SEMI { EX_Empty } /* changed to empty */
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
	SYNCHRONIZED LPAR e=expression RPAR s=statement { ST_Synch(e,s) }
	| TRY b=block f=finally { ST_Try(b,[],f) }
	| TRY b=block c=catch { ST_Try(b,[]@[c],ST_Empty) }
	| TRY b=block c=catch f=finally { ST_Try(b,[]@[c],f) }
;

/* catch */
(* 
catches: 
	c=catch { c } 
	| cs=catches c=catch { cs^c }
;
*)
catch: 
	ch=catchHeader b=block { ST_Catch(ch,b) }
;

catchHeader: 
	CATCH LPAR ts=types id=IDENTIFIER RPAR { Catch_header(ts,Identifier(id)) }
	| CATCH LPAR ts=types RPAR { Catch_header(ts,EX_Empty) }
;

finally: 
	FINALLY b=block { ST_Finally(b) }
;
/* end catch */

constantExpression:
	ce=conditionalExpression { ce }
;
%%