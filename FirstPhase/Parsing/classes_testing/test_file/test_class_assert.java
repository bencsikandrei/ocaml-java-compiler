public class Foo {
	public static void main(String[] args) {
		Baz.testAsserts();
		// Will execute after Baz is initialized.
		}
	}
class Bar {
	static {
		Baz.testAsserts();
		// Will execute before Baz is initialized!
	}
}
class Baz extends Bar {
	static void testAsserts(){
		boolean enabled = false;
		assert enabled = true;
		System.out.println("Asserts " +
			(enabled ? "enabled" : "disabled"));
	}
}
