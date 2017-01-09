%%

/* operators */
%public assignmentOperator:
	ASSIGN { " = " }
	| PEQUAL { " += " }
	| MINUSEQUAL { " -= " }
	| MULEQUAL { " *= " }
	| DIVEQUAL { " /= " }
	| MODEQUAL { " %= " }
	| ANDEQUAL { " &= " }
	| OREQUAL { " |= " }
	| XOREQUAL { " ^= " }
	| RSHIFTEQUAL { " >>= " }
	| LSHIFTEQUAL { " <<= " }
	| LOGSHIFTEQUAL { " >>>= " }
;

%public arithmeticUnaryOperator:
	PLUS { "+" }
	| MINUS { "-" }
;

%public logicalUnaryOperator:
	BNOT { "~" }
	| NOT { "!" }
;

%%