%{
	open Expressions
%}

%%

/* operators */
%public assignmentOperator:
	ASSIGN { ASS_Equal }
	| PEQUAL { ASS_Plus }
	| MINUSEQUAL { ASS_Minus }
	| MULEQUAL { ASS_Mul }
	| DIVEQUAL { ASS_Div }
	| MODEQUAL { ASS_Mod }
	| ANDEQUAL { ASS_And }
	| OREQUAL { ASS_Or }
	| XOREQUAL { ASS_Xor }
	| RSHIFTEQUAL { ASS_RShift }
	| LSHIFTEQUAL { ASS_LShift }
	| LOGSHIFTEQUAL { ASS_LogShift }
;

%public arithmeticUnaryOperator:
	PLUS { UO_Plus }
	| MINUS { UO_Minus }
;

%public logicalUnaryOperator:
	BNOT { UO_BNot }
	| NOT { LUO_Not }
;

%%