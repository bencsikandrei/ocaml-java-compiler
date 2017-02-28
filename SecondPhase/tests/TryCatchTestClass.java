
public class TryCatchTestClass {
	
    public static void main (String[] args) {
    	int a = 1, b = 5;
        int c = 0;
        Object o = new Object();
        o.toString();
        try {
        	Object d = null;
        	Object j = new Object();
        	d.toString();
        	System.out.println("No exceptions here");
        } catch(NullPointerException e) {
        	System.out.println("A null pointer exception occured");
        } catch(Exception e) {
        	System.out.println("An exception occured");
        } finally {
        	System.out.println("Finlly is executed");
        }
        //System.out.println(a&&c);
    }
}
