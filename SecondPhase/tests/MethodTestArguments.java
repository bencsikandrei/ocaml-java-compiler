public class MethodTestArguments {
	int a = 10;
	
	public static int max() {
		// the max of everything
		return 42;
	}
	
	public static int max(int a, int b) {
		System.out.println("In max");
		return (a > b ? a : b);
	}
	
	public static void main(String[] args) {
		int y = max(10, 15);
		int z = max();
		
		
		
	}
}
