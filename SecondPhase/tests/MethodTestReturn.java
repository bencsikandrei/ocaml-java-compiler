public class MethodTestReturn {
	int a = 10;

	public static int max() {
		System.out.println("In max");
		int a = 10;
		int b = 15;
		return (a > b ? a : b);
	}
	
	public static void main(String[] args) {
		int y = max();
		MethodTestReturn m = new MethodTestReturn();
		
	}
}
