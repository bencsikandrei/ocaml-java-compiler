
public class TryCatchTestMultiple {
	
	public static void max() throws ArithmeticException {
			System.out.println("I will throw " + 42 + " exceptions");
			throw new ArithmeticException();
	}
	
    public static void main (String[] args) {
    	int a = 1, b = 5;
        int c = 0;
        Object o = new Object();
        o.toString();
		System.out.println("Something will happen");
		
		SomeClass sc = new SomeClass();
		
		System.out.println("Return value of getX " + sc.getX());
		
		sc.method();
		
		try {
			
			max();
			
        } catch(Exception e) {
        	System.out.println("I will catch this one");
        }
        //System.out.println(a&&c);
    }
}

class SomeClass {
	int x = 1;
	
	public SomeClass() {
		x = 10;
	}
	
	public int getX() {
		return x;
	}
	
	public void method() {
		Object o1 = new Object();
		o1.toString();
		System.out.println("This just prints something");
	}
}
