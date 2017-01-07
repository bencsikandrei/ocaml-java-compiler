%{

%}

%start javaFile
%start <string> javaFile

%%
javaFile:
	| option(package) option(imports) option(file_content) EOF { "" }

package:
	| PACKAGE pack_name SEMI { }

pack_name:
	| IDENTIFIER { }
	| pack_name DOT IDENTIFIER { }

imports:
	| import { }
	| imports import { }

import:
	| IMPORT pack_name SEMI { }
	| IMPORT pack_name DOT MUL SEMI { }

file_content:
	| j_class { }