%{
	
%}

%%



%public modifiers:
	| m=modifier { m::[] }
	| ms=modifiers m=modifier { ms@[m] }

modifier:
	| ABSTRACT {M_Abstract}
	| PUBLIC {M_Public }
	| PRIVATE {M_Private }
	| PROTECTED {M_Protected }
	| a=Annotation {M_Annot a}
	| STATIC {M_Static }
	| FINAL {M_Final }
	| STRICTFP {M_Strictfp }
	| SYNCHRONIZED {M_Synchronized}
	| NATIVE {M_Native}

%public variableModifiers:
	| m=variableModifier { m::[] }
	| ms=variableModifiers m=variableModifier { ms@[m] }

variableModifier:
	| VOLATILE{VM_Volatile}
	| TRANSIENT{VM_Transient}
	| FINAL {VM_Final}

%%