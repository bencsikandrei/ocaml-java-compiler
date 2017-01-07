%{
	
%}

%%



%public modifiers:
	| m=modifier { m::[] }
	| m=modifier ms=modifiers { m::ms }

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

%public Annotation: (* TODO *)
	|ANOT i=IDENTIFIER{  { aname=i ; aoth="" } }

%%