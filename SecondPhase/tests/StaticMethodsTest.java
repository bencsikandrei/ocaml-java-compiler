
public class StaticMethodsTest {

	public static void printSomeStuff() {
        System.out.println("Done my job!");
	}
    public static void main (String[] args) {
    	int a = 1, b = 5;
        
        System.out.println("Starting!");
        
        int min = MinMax.min(a, b);
        int max = MinMax.max(a, b);
        int eql = MinMax.eql(a, b);
        
        System.out.println("The minimum is " + min);
        System.out.println("The maximum is " + max);
        System.out.println("The diference between them is " + eql);
     	
     	printSomeStuff();               
    }
}

class MinMax {
    public static int min (int a, int b) {
		return (a < b ? a : b);
    }
    
    public static int max (int a, int b) {
    	return (a > b ? a : b);
    }
    
    public static int eql (int a, int b) {
    	System.out.println("Need to see if equal");
    	return ( (a == b) ? 0 : (a-b));
    }
}
