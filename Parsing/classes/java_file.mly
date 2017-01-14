%{
open Definitions
%}

%start javaFile
%start <abstractSyntaxTree> javaFile

%%
javaFile:
	| p=option(package) i=option(imports) c=option(file_content_l) EOF { 
			let p = match p with | None -> [] | Some p -> p in
			let i = match i with | None -> [] | Some i -> i in
			let c = match c with | None -> [] | Some c -> c in
				{fPackage=p;
				fImports=i;
				fContent=c;}
		}

package:
	| PACKAGE p=pack_name SEMI { p }

pack_name:
	| i=IDENTIFIER { i::[] }
	| i=IDENTIFIER DOT p=pack_name { i::p }

imports:
	| i=import { i::[] }
	| i=import l=imports { i::l }

import:
	| IMPORT s=option(STATIC) p=pack_name SEMI { 
		match s with | None -> {impStatic=false; impPack=p; impAll=false}
					 | Some s -> {impStatic=true; impPack=p; impAll=false} }
	| IMPORT s=option(STATIC) p=pack_name DOT MUL SEMI { 
		match s with | None -> {impStatic=false; impPack=p; impAll=true}
					 | Some s -> {impStatic=true; impPack=p; impAll=true} }

file_content_l:
	| c=file_content { c::[] }
	| c=file_content l=file_content_l { c::l }

file_content:
	| c=j_class { F_Class c }
	| i=j_interface { F_Interfaces i }