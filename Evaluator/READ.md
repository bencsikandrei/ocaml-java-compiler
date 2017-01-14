# Authors: 
	Javier Alejandro ATADIA
	Florencia ALVAREZ
	Paulina ALVAREZ
	Andrei-Florin BENCSIK

# Project Name

JAVA Compiler using Ocaml - Phase I

## Installation

To be able to run the first phase, please go to Parsing, and do:

	make

This will compile all the necessary files and move them to the build folder

## Usage

After the make, you can test the compiler by running the python script:

	python test.py

This will test all files present in the predefined test folders
	
	classes_testing/test_*
	expressions_testing/test_*

The wildcard match specifies the part of the compiler being tested and is used 
by the script

To add your own classes to auto-testing, you just need to add that .java file to:
	./classes_testing/test_file

## History

The compiler is a merge between a classes compiler and a statement/expression 
compiler
Both parts are available by switching to dedicated branches

	master -> whole compiler
	expressions -> only statements and expressions
	classes -> only classes

## Credits

	Javier Alejandro ATADIA
	Florencia ALVAREZ
	Paulina ALVAREZ
	Andrei-Florin BENCSIK

## License

