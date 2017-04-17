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

	ocamlbuild ./Main.native (or Main.byte - as you wish)

This will compile all the necessary files and move them to the build folder


*******************************************************************************
*******************************************************************************

3. Testing

*******************************************************************************

3.1 Testing the compiler

After the (2) Installation, you can test the compiler by running :

	./Main.native [options] file

By default:
	 verbose if OFF
	 typing is ON
	 running is ON

Where file is a java file with at least one public class which contains the 
same name as the file, and has a main method; option can be one or more of :

	-v verbose - you can see the HEAP, the STACK, the classes in the JVM.. etc
	-nr for not executing (running)
	-nt for not typing

	Example: ./Main.native -v tests/IfElseTestClass.java 

You can run the script to test typing by running:

	python test.py

It will test all the files in the folders "tests/Fail", which are supposed 
to fail and the ones in "tests/Good" which are supposed to pass.

The tests in "tests/" are tests for execution and may throw exceptions. 
Check their output by yourself to see correctness.
You can always provide your own tests, but please be sure to check the known
issues and the not implemented list before trying anything exotic.

*******************************************************************************

3.2 Adding more tests

 To add your own classes to auto-testing, you just need to add that .java 
 file to the corresponding directory.
 	/tests/Good/	for typing test that should pass
 	/tests/Fail/	for typing test that should fail
 	/tests/			for execution tests # NOTE: these are not auto-executed

*******************************************************************************

4. Not implemented

 Here is a list of the things that are not implemented or not working properly:

From the typing part
	- Final modifier (in arguments and attributes). It's checked but doesn't has an effect.
	- Error: "Implicit super constructor A() is not visible. Must explicitly invoke another constructor" in son's constructor when parent's constructor is private.
	- Verify not calling a protected or private attribute of an object -> obj.var
	- Check constructor for primitive types.
	- Particular cases of casts.
	- identifier.new qualified_name

From the execution part
 	- Arrays : only one dimension array is implemented.
 	- Nested and Inner classes : they can me initiated but the scope of 
 	the outer class is not added, so they do not have access to its attributes.
 	- String class : only contains two constructors (empty and string given),
 	and two hardcoded methods (length, isEmpty and toString).
 	- ClassOf expression : could not find out what it was.
 	- Try - Catch - Finally: has a bug with scope, where the scope is not cleared
 	when an exception occurs 
 	- Types: only int, float and boolean operations are supported. 
 	- Static methods: please specify the fully qualified name when calling them,
 	won't work otherwise
 	- System.out.println: is hardcoded, please don't use another method called 
 	*.prinln()
 	- Defaults: some of the default classes are added, some aren't
 	List of defaults: Object, String, Exception, Integer, Float, Boolean
 	 (and some subclasses of Exception) - wrappers have no methods! nor attributes
 	 they are just there for completion
 	They do NOT have their methods implemented! (except toString and hardcoded ones
 	from String)
 	- Checking it twice: some parts that are already checked in the Typing are double
 	checked in execution :)


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
