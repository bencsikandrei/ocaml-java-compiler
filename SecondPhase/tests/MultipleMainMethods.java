public class MultipleMainMethods {
	public static void main (String [] args) {
		int a, b = 10;
		b++; a--;
		
		boolean tf = true;
		float f = 10.0;
		
		System.out.println("The public class");
		System.out.println("Hello world");
		System.out.println("Bla vla");
	}
}

class AnotherClass {
	public static void main(String [] args) {
		System.out.println("The non public class");
	}
}
