# build all files
OCAML=ocamlbuild 
YACCFLAGS=-use-menhir -yaccflag -v
BUILDFOLDER=_build

# copy path
THIS="expression_testing"
EXPRPARSERS="../expressions"
CLASSPARSERS="../classes"
MAIN="../"

EXEC=main.native

copy: 
	cp $(EXPRPARSERS)/* .
	cp $(CLASSPARSERS)/* .
	cd $(MAIN); cp javaLexer.mll tokens.mly printing.ml ast.ml $(THIS)

del:
	rm *.mly
	rm javaLexer.mll printing.ml ast.ml main_other.ml expressions.ml definitions.ml
	
all: 
	$(OCAML) $(EXEC) $(YACCFLAGS) 

clean:
	rm -rf $(EXEC) 
	
mrproper: clean
	rm -rf $(BUILDFOLDER)
	

.PHONY: all clean mrproper
