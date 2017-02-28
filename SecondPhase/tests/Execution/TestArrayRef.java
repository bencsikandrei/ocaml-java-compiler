
public class TestArrayRef {
	int a = 10;

	public static void main(String[] args) {
		TestArrayRef[] arr = new TestArrayRef[2];
		System.out.println(arr[0]);
		arr[0] = new TestArrayRef();
		System.out.println(arr[0].a);

		TestArrayRef cc = new TestArrayRef();
		TestArrayRef[] arreglito = new TestArrayRef[] {cc};
	}
}
