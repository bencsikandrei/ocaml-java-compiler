%{
open Definitions
%}

%start javaFile
%start <abstractSyntaxTree> javaFile

%%
javaFile:
	| option(package) option(imports) option(file_content) EOF { STR "" }

package:
	| PACKAGE pack_name SEMI { }

pack_name:
	| IDENTIFIER { }
	| pack_name DOT IDENTIFIER { }

imports:
	| import { }
	| imports import { }

import:
	| IMPORT option(STATIC) pack_name SEMI { }
	| IMPORT option(STATIC) pack_name DOT MUL SEMI { }

file_content:
	| j_class { }
	| j_interface { }