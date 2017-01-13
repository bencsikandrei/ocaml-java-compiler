
# build all files
OCAML=ocamlbuild 
YACCFLAGS=-use-menhir -yaccflag -v
MYBUILDFOLDER=_build

MYBUILD = ./build
COPYPATH = "./classes"
COPYPATH_EXPR = "./expressions"
COPYPATH_TEST="./classes_testing"

PARSING =  types.mly tokens.mly  class_methods.mly class_definition.mly types_params_defin.mly modifiers.mly java_file.mly interface_definition.mly parserStart.mly parserStatements.mly parserOperators.mly parserExpressions.mly
all: clean main

$(MYBUILD)/javaParser.cmo: 
	cd $(MYBUILD); menhir $(PARSING)  -v --base javaParser
	cd $(MYBUILD); rm javaParser.mli
	cd $(MYBUILD); ocamlc -c javaParser.ml

$(MYBUILD)/javaLexer.cmo: $(MYBUILD)/javaLexer.mll
	cd $(MYBUILD); ocamllex javaLexer.mll
	cd $(MYBUILD); ocamlc -c javaLexer.ml

$(MYBUILD)/definitions.cmo:
	cd $(MYBUILD); ocamlc -c definitions.ml

$(MYBUILD)/expressions.cmo:
	cd $(MYBUILD); ocamlc -c expressions.ml

$(MYBUILD)/printing.cmo:
	cd $(MYBUILD); ocamlc -c printing.ml

$(MYBUILD)/ast.cmo:
	cd $(MYBUILD); ocamlc -c ast.ml


main: $(MYBUILD) $(MYBUILD)/ast.cmo $(MYBUILD)/printing.cmo $(MYBUILD)/expressions.cmo $(MYBUILD)/definitions.cmo $(MYBUILD)/javaParser.cmo $(MYBUILD)/javaLexer.cmo $(MYBUILD)/main.ml 
	cd $(MYBUILD); ocamlc -c main.ml
	cd $(MYBUILD); ocamlc -pp "#load"   ast.cmo  printing.cmo expressions.cmo definitions.cmo  javaParser.cmo javaLexer.cmo main.cmo -o main

$(MYBUILD): 
	mkdir -p $(MYBUILD)
	cp $(COPYPATH)/* $(MYBUILD)/
	cp $(COPYPATH_EXPR)/* $(MYBUILD)/
	cp ./main.ml $(MYBUILD)/
	cp ast.ml $(MYBUILD)/
	cp printing.ml $(MYBUILD)/
	cp ./tokens.mly $(MYBUILD)/
	cp ./javaLexer.mll $(MYBUILD)/


clean:
	rm -rf $(MYBUILD)
