public class BlocksTest {
    public static void main (String[] args) {
    	int a = 1, b = 5;
        System.out.println(a+=10);
        System.out.println(a);
        System.out.println("Print the maximum of a and b");
        if( a > b ) {
        	
        	System.out.println("a is larger than b");
        }
        else {
        	System.out.println("It only prints if a is larger than b");
        }
        if( a > b ) {
        	System.out.println("42 is the answer");
        }
        	
		while( a > b ) {
			System.out.println(a--);
			System.out.println(b++);
		}
    }
}
