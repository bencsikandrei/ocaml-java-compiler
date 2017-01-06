BUILD = ./build

PARSING =  types.mly expr.mly tokens.mly  class_methods.mly class_definition.mly types_params_defin.mly

COPYPATH = ./classes
COPYPATH_TEST = ./classes_testing

all: clean main

$(BUILD)/javaParser.cmo: 
	cd $(BUILD); menhir $(PARSING)  -v --base javaParser
	cd $(BUILD); rm javaParser.mli
	cd $(BUILD); ocamlc -c javaParser.ml

$(BUILD)/javaLexer.cmo: $(BUILD)/javaLexer.mll
	cd $(BUILD); ocamllex javaLexer.mll
	cd $(BUILD); ocamlc -c javaLexer.ml

$(BUILD)/definitions.cmo:
	cd $(BUILD); ocamlc -c definitions.ml

main: $(BUILD) $(BUILD)/definitions.cmo $(BUILD)/javaParser.cmo $(BUILD)/javaLexer.cmo $(BUILD)/main.ml
	cd $(BUILD); ocamlc -c main.ml
	cd $(BUILD); ocamlc -pp "#load"  javaParser.cmo javaLexer.cmo main.cmo -o main

$(BUILD): 
	mkdir -p $(BUILD)
	cp $(COPYPATH)/* $(BUILD)/
	cp $(COPYPATH_TEST)/* $(BUILD)/
	cp ./tokens.mly $(BUILD)/
	cp ./javaLexer.mll $(BUILD)/

clean:
	rm -fr $(BUILD)
	clear
