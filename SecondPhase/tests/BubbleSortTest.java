public class BubbleSortTest {

    public static void sort(int size) {
		int [] a = { 5, 5, 6, 7, 8, 1 };
		
        for (int out = size - 1; out > 0; out --) {
            for (int in = 0; in < out; in ++) {
                if( (a[in + 1] < a[in] ? true : false) ) {
                    int temp = a[i];
					a[i] = a[j];
					a[j] = temp;
                }
            }
        }
        
        for(int i = 0; i < size; i++) {
        	System.out.println(a[i]);
        }

    }

    public static void main(String[] args) {

        int Nb = 6;
	
        
        sort(Nb);
        

    }

}
