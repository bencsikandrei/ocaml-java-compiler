*******************************************************************************
*******************************************************************************
JFAP - MiniJava Compiler for F2B304 TELECOM Bretagne
2016-2017

Authors: 
	Javier Alejandro ATADIA
	Florencia ALVAREZ
	Paulina ALVAREZ
	Andrei-Florin BENCSIK

*******************************************************************************
*******************************************************************************

1. Project Name

JAVA Compiler using Ocaml - Phase I


*******************************************************************************
*******************************************************************************

2. Installation

First make sure to clone the repository with the command and input your credentials:

	git clone https://redmine-df.telecom-bretagne.eu/git/f2b304-minijava-jfap

To be able to run the first phase, please go to PROJECT_ROOT/Parsing, and do:

	make

This will compile all the necessary files and move them to the build folder


*******************************************************************************
*******************************************************************************

3. Testing

*******************************************************************************

3.1 Testing the parser

After the (2) Installation, you can test the compiler by running the python script:

	python test.py

This will test all files present in the predefined test folders
	
	classes_testing/test_*
	expressions_testing/test_*
	expressions_testing/bad_test_*

The wildcard match specifies the part of the compiler being tested and is used 
by the script. That match for the wildcard means UNIT TESTS! So in those folders,
please put only valid .java files. 

In statement, only statements (blocks, ifs, fors..)
Keep in mind that because those parsers are used for testing they start with Blocks, 
so include your statements into blocks

In expression, only valid expressions (additions, multiplications,.. ). Please
do not put semicolons 

In methods, only methods (no need for classes around them)

In class, any classes, no imports, no packages

In file, well here you can put what you please! FULL JAVA FILES! (they actually MUST
be full valid Java files)

Bad tests are tests that are supposed to fail. Any file in a folder starting 
with bad_test_* is made to fail.

*******************************************************************************

3.2 Adding more tests

 To add your own classes to auto-testing, you just need to add that .java file to:
	
	./classes_testing/test_file

*******************************************************************************

3.3 Testing only one file

If by change you want to test a file yourself, you can use..
For the parser:

	python parser.py --file Myclass.java [--mode (file|method|class|expression|statement)] [-v|--verbose]

Default mode for the parser is: file

*******************************************************************************

3.2 Testing the lexer:
	
	python parser.py --file Myclass.java


*******************************************************************************
*******************************************************************************

5. History

The compiler is a merge between a classes compiler and a statement/expression 
compiler
Both parts are available by switching to dedicated branches

	master -> whole compiler
	expressions -> only statements and expressions
	classes -> only classes


*******************************************************************************
*******************************************************************************

6. Credits

	Javier Alejandro ATADIA
	Florencia ALVAREZ
	Paulina ALVAREZ
	Andrei-Florin BENCSIK


*******************************************************************************
*******************************************************************************
