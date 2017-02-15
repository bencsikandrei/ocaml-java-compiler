(* Add JavaRuntimeExceptions *)
exception ArithmeticException
exception ArrayIndexOutOfBoundsException
exception ArrayStoreException
exception ClassCastException
exception IllegalArgumentException
exception IllegalMonitorStateException
exception IllegalStateException
exception IndexOutOfBoundsException
exception IllegalThreadStateException
exception NegativeArraySizeException
exception NullPointerException
exception NumberFormatException
exception SecurityException
exception StringIndexOutOfBounds
exception UnsupportedOperationException
exception ClassNotFoundException
exception CloneNotSupportedException
exception IllegalAccessException
exception InstantiationException
exception InterruptedException
exception NoSuchFieldException
exception NoSuchMethodException
exception Exception of string 
(* 
Error: Main method not found in class XXX, please define the main method as:
   public static void main(String[] args)
or a JavaFX application class must extend javafx.application.Application 
*)
exception NoMainMethod of string

(* Compilation errors *)
(* XXX.java:1: error: class XXX is public, should be declared in a file 
named XXX.java
public class XXX {
       ^
1 error *)
exception PublicClassName of string

(* TwoMethodsOneClass.java:10: error: method m1() is already defined in class TwoMethodsOneClass
	public void m1 () {
	            ^
1 error
 *)
exception MethodAlreadyDefined of string

(* UnknownParent.java:1: error: cannot find symbol
public class UnknownParent extends SomethingUnknown {
                                   ^
  symbol: class SomethingUnknown
1 error
 *)
exception UnknownSymbol of string 