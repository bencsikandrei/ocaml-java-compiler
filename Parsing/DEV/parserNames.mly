%{
	open Expressions
%}

%%

%public qualifiedName:
	id=IDENTIFIER { id }
	/*| qn=qualifiedName DOT id=IDENTIFIER { qn^"."^id }*/
;
%%
