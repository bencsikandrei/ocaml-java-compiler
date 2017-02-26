*******************************************************************************
*******************************************************************************
* JFAP - MiniJava Compiler for F2B304 TELECOM Bretagne			              *
* 2016-2017			              			              			          *
*  			              			              			                  *
* Authors:        			              			                          *
* 	Javier Alejandro ATADIA       			              			          *
* 	Florencia ALVAREZ       			              			              *
* 	Paulina ALVAREZ       			              			                  *
* 	Andrei-Florin BENCSIK       			              			          *
*        			              			                                  *
*******************************************************************************
*******************************************************************************
*        			              			                                  *
* 1. Project Name                                                             *
*        			              			                                  *
* JAVA Compiler using Ocaml - Phase II                                        *
*        			              			                                  *
*        			              			                                  *
*******************************************************************************
*******************************************************************************

2. Installation

First make sure to clone the repository with the command and input your 
credentials:

	git clone https://redmine-df.telecom-bretagne.eu/git/f2b304-minijava-jfap

To be able to run the first phase, please go to PROJECT_ROOT/SecondPhase,
and do:

	ocamlbuild ./Main.native

This will compile all the necessary files and move them to the build folder


*******************************************************************************
*******************************************************************************

3. Testing

*******************************************************************************

3.1 Testing the compilator

After the (2) Installation, you can test the compiler by running :

	./Main.native [options] file

Where file is a java file with at least one public class which contains the 
same name as the file, and has a main method; option can be one or more of :

	-v verbose
	-r for seeing the execution even if typing fails

You can run the script to test typing by running:

	python test.py

It will test all the files in the folders "tests/Fail", which are supposed 
to fail and the ones in "tests/Good" which are supposed to pass.

The tests in "tests/" are tests for execution and may throw exceptions.

*******************************************************************************

3.2 Adding more tests

 To add your own classes to auto-testing, you just need to add that .java 
 file to the corresponding directory.
 	/tests/Good/	for typing test that should pass
 	/tests/Fail/	for typing test that should fail
 	/tests/			for execution tests

*******************************************************************************

4. Not implemented

 Here is a list of the things that are not implemented or not working properly:

From the typing part
	-

From the execution part
 	- Arrays : only one dimension array is implemented.
 	- Nested and Inner classes : they can me initiated but the scope of 
 	the outer class is not added, so they do not have access to its attributes.
 	- String class : only contains two constructors (empty and string given),
 	and two hardcoded methods (length, isEmpty and toString).
 	- ClassOf expression : could not find out what it was.


*******************************************************************************
*******************************************************************************

5. History

The parser (Phase I) was taken from the teachers and it was not modified.
Please read their readme for more information (Parsing/README).

The compiler is a merge between the typing and the execution part of a compiler
Both parts are available by switching to dedicated branches

	master -> whole compiler
	typing -> last work of typing before merge with master
	execution -> last work of execution before merge with master


*******************************************************************************
*******************************************************************************

6. Credits

	Javier Alejandro ATADIA
	Florencia ALVAREZ
	Paulina ALVAREZ
	Andrei-Florin BENCSIK


*******************************************************************************
*******************************************************************************
