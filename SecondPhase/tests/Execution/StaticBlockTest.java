
public class StaticBlockTest {
	int a;

	{
		 System.out.println("Non-static block");
		 a = 10;
	}

	public static void main(String[] args) {
		StaticBlockTest sb = new StaticBlockTest();
		System.out.println(sb.a);
	}
}