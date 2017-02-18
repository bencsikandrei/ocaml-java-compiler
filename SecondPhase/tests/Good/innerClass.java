package main;

public class Main {

	public static void main(String argv[]){
		System.out.println("Hello World!");
		A a = new A();
	}
	
}

class H {
	H(){
		System.out.println("outer");
	}
}

class A{
	class H{
		H(){
			System.out.println("inner");
		}
	}
	
	A(){
		H h = new H();
	}
}