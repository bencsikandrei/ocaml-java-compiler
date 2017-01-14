%{
open Ast
%}

%start javaFile
%start <abstractSyntaxTree> javaFile

%%
javaFile:
	| f=javaCompFile { JFILE f }
	
javaCompFile:
	| p=option(package) i=option(imports) c=option(file_content_l) EOF { 
			let p = match p with | None -> [] | Some p -> p in
			let i = match i with | None -> [] | Some i -> i in
			let c = match c with | None -> [] | Some c -> c in
				{fPackage=p; fImports=i; fContent=c;}
		}

package:
	| PACKAGE p=pack_name SEMI { p }

pack_name:
	| i=IDENTIFIER { i::[] }
	| i=IDENTIFIER DOT p=pack_name { i::p }

pack_import:
	| i=IDENTIFIER { i::[] }
	| i=IDENTIFIER DOT MUL { i::[] }
	| i=IDENTIFIER DOT p=pack_import { i::p }

imports:
	| i=import { i::[] }
	| i=import l=imports { i::l }

import:
	| IMPORT s=option(STATIC) p=pack_import SEMI { 
		match s with | None -> {impStatic=false; impPack=p; impAll=false}
					 | Some s -> {impStatic=true; impPack=p; impAll=false} }

file_content_l:
	| c=file_content { c::[] }
	| c=file_content l=file_content_l { c::l }

file_content:
	| c=j_class { F_Class c }
	| i=j_interface { F_Interface i }