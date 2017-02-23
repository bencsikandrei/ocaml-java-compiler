public class MethodTestReturn extends ToExtendFrom {
	int a = 10;

	public static int max() {
		System.out.println("In max");
		int a = 10;
		int b = 15;
		return (a > b ? a : b);
	}
	
	public void meth() {
		System.out.println("this method is from this class");
		
	}
	
	public static void main(String[] args) {
		int y = max();
		MethodTestReturn m = new MethodTestReturn();
		m.meth();
		m.meth(42);
	}
}

class ToExtendFrom {

	private int p = 5;	
	
	public void meth() {
		System.out.println("this method is from the parent");
	}
	
	public void meth(int a) {
		System.out.println("this method is from the parent and prints an int");
		System.out.println(a);
		priv();
	}
	
	private void priv () {
		System.out.println("This is the parent's private method");
	}
}

