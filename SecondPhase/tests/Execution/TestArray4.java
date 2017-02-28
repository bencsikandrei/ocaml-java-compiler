
public class TestArray4 {

	public static void main(String[] args) {
		// This should throw an exception
		int[] a = new int[3];
		a[0] = true;
		System.out.println("this should not be seen");

	}

}