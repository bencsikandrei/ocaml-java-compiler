public class InstanceTest {
	InstanceTest() {}

	public static void main(String[] args) {
		InstanceTest a;
		System.out.println(a instanceof InstanceTest);
		a = new InstanceTest();
		System.out.println(a instanceof InstanceTest);
	}
}