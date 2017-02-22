public class BlocksVariablesTest {
    public static void main (String[] args) {
    	int a = 1, b = 5;
        System.out.println(a);
        System.out.println(b);
        System.out.println("Print the maximum of a and b");
        if( a > b ) {
        	
        	System.out.println("a is larger than b");
        }
        else {
        	System.out.println("b is larger than a");
        }
        int j;
        if( a < b ) {
        	int j = 10;
        	System.out.println("The value of J inside a block");
        	System.out.println(j);
        	System.out.println("End of if");
        }
        System.out.println("The value of J outside");
        System.out.println(j);
		
		
    }
}
