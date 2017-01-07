%{
	
%}
%%
%public type_params_defin:
 	| LANG t=type_param_l RANG { t }

type_param_l:
	| t=type_parameter	{t::[]}
	| t1=type_parameter COMM t2=type_param_l {t1::t2}
	

type_parameter:
	| t=IDENTIFIER {TPL_Ident t}
	| t1=IDENTIFIER EXTENDS t2=IDENTIFIER {TPL_Extend(t1,t2)}

%%