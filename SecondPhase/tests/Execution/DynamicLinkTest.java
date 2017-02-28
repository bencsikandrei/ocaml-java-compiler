class SomeClass {
	private int value = 42;
	public static void max() {
		System.out.println("In some class");
	}
	
	public int getValue() {
		return value;
	}
}


public class DynamicLinkTest extends SomeClass {
	int value = 10;
	
	public int getValue() {
		return value;
	}
	
	public static void max() {
		System.out.println("In this class");
	}
	
	public static void main(String[] args) {
		DynamicLinkTest dt = new DynamicLinkTest();
		
		SomeClass sc = new SomeClass();
		
				
		System.out.println(dt.getValue() + " this is a nice value");
		System.out.println(sc.getValue());
	}
}

