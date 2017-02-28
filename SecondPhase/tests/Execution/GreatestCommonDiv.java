public class GreatestCommonDiv {
	public static void main(String[] args) {
		int a = 18, b = 6;

		if (a == 0)
			System.out.println(b);
        

    	while (b != 0) {
        	if (a > b)
            	a -= b;
        	else
            	b -= a;
    	}
		System.out.println(a);

	}
}
