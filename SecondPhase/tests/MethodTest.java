public class MethodTest {
	int a = 10;

	public void max() {
		System.out.println("In max");
	}

	public static void main(String[] args) {
		MethodTest m;
		m = new MethodTest();
		max();
		println("hello world");
		System.out.println(m.a);
	}
}
