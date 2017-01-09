# build all files
OCAML=ocamlbuild 
YACCFLAGS=-use-menhir -yaccflag -v
BUILDFOLDER=_build

EXEC=main.native

all: 
	$(OCAML) $(EXEC) $(YACCFLAGS) 

clean:
	rm -rf $(EXEC) 
	
mrproper: clean
	rm -rf $(BUILDFOLDER)

.PHONY: all clean mrproper
