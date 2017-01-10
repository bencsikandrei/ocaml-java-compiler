
%%
%public primaryExpression:
	{ Identifier("primaryExpression") } 
;

%public localVariableDeclStmt:
	{ Identifier("localVariableDeclStmt") } 
;

%public block:
	{ ST_block([]) }
;
%%