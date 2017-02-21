public class ConstructorTest {
	int b = 10000;

	{
		b = 10;
	}

	ConstructorTest(int a) {
		System.out.println("Inside constructor");
		System.out.println(b);
		b = a;
		int c = 101;
		System.out.println(a);
		System.out.println(b);
		System.out.println(c);
		System.out.println("Leaving constructor");
	}

	public static void main(String[] args) {
		ConstructorTest ct;
		ct = new ConstructorTest(42);
		
		System.out.println(ct.b);
		
		
	}
}
