(* Code for doing the execution *)
open AST

(* Add JavaRuntimeExceptions *)
exception ArithmeticException
exception ArrayIndexOutOfBoundsException
exception ArrayStoreException
exception ClassCastException
exception IllegalArgumentException
exception IllegalMonitorStateException
exception IllegalStateException
exception IllegalThreadStateException
exception IndexOutOfBoundsException
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

(* Make a structure that contains the whole program, its heap
stack .. *)

let execute_code program = 
	(match program.package with
  	| None -> ()
	| Some pack -> AST.print_package pack );
  	List.iter (fun t -> AST.print_type "" t; print_newline()) program.type_list