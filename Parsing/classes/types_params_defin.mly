%{
	
%}
%%
%public type_params_defin:
 	| LANG type_param_l RANG { }

type_param_l:
	| type_parameter	{}
	| type_parameter COMM type_param_l {}
	

type_parameter:
	| types {}
	| types EXTENDS types {}

%%