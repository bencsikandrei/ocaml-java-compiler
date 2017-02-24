
public class TryCatchTestMultiple {
	
    public static void main (String[] args) {
    	int a = 1, b = 5;
        int c = 0;
        Object o = new Object();
        o.toString();
        try {
		    try {
		    	Object b;
		    	Object a = new Object();
		    	b.toString();
		    	System.out.println("No exceptions here");
		    } catch (ArrayIndexOutOfBoundsException e) {
		    	System.out.println("An array index out of bounds");
		    }
        } catch (Exception e) {
        	System.out.println("Gotta catch em all !");
        }
        //System.out.println(a&&c);
    }
}
