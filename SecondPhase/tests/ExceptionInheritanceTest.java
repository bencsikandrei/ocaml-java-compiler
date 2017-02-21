class MyException extends Exception {
	public String toString() {
		return "";
	}
}

class NotThrowable {
	public int hashCode() {
		return 1;
	}
}

public class ExceptionInheritanceTest {
	int a = 10;
	boolean b;

	public static void main(String[] args) {
		MyException ct = new MyException();
		System.out.println(4);
		System.out.println(2+7*3/0);
		throw new MyException();
	}
}
