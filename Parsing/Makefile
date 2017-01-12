
# build all files
OCAML=ocamlbuild 
YACCFLAGS=-use-menhir -yaccflag -v
BUILDFOLDER=_build

BUILD = ./build

PARSING =  types.mly expr.mly tokens.mly  class_methods.mly class_definition.mly types_params_defin.mly modifiers.mly java_file.mly interface_definition.mly

all: clean main

$(BUILD)/javaParser.cmo: 
	cd $(BUILD); menhir $(PARSING) --strict  -v --base javaParser
	cd $(BUILD); rm javaParser.mli
	cd $(BUILD); ocamlc -c javaParser.ml

$(BUILD)/javaLexer.cmo: $(BUILD)/javaLexer.mll
	cd $(BUILD); ocamllex javaLexer.mll
	cd $(BUILD); ocamlc -c javaLexer.ml

$(BUILD)/definitions.cmo:
	cd $(BUILD); ocamlc -c definitions.ml

main: $(BUILD) $(BUILD)/definitions.cmo $(BUILD)/javaParser.cmo $(BUILD)/javaLexer.cmo $(BUILD)/main.ml
	cd $(BUILD); ocamlc -c main.ml
	cd $(BUILD); ocamlc -pp "#load"  definitions.cmo javaParser.cmo javaLexer.cmo main.cmo -o main

$(BUILD): 
	mkdir -p $(BUILD)
	cp $(COPYPATH)/* $(BUILD)/
	cp $(COPYPATH_TEST)/main.ml $(BUILD)/
	cp ./tokens.mly $(BUILD)/
	cp ./javaLexer.mll $(BUILD)/

clean:
	rm -rf $(EXEC) 
	
mrproper: clean
	rm -rf $(BUILDFOLDER)

.PHONY: all clean mrproper
