
public class ClassTest {
	int a = 10;

	public ClassTest() {
		a = 69;
	}

	public void change() {
		System.out.println(a);
		Classesita cc = new Classesita();
		System.out.println(this.a);
		System.out.println(cc.a);
		a=2000;
		System.out.println(this.a);
		System.out.println(a);
	}

	public static void main(String[] args) {
		ClassTest ca = new ClassTest();
		Classesita cb = new Classesita();
		ca.change();
		System.out.println(ca.a);
		ca.a = 8000;
		System.out.println(ca.a);
	}
}

class Classesita {
	int a = 42;
}