import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.Random;

public class MyUtils {
    /**
     * Copy too comparable arrays
     * @param dst
     * @param src
     */
    public static void copy(Comparable [] dst, Comparable [] src) {
        if(dst.length != src.length) {
            System.err.println("Unequal arrays");
            return;
        }

        for(int i = 0; dst.length > i; i++) {
            dst[i] = src[i];
        }
    }

    /**
     *
     * @param N
     * @return
     */
    public static Integer[] generateRandomNums(int N) {
        //System.out.println("called");
        Integer [] arr = new Integer[N];

        Random r = new Random(System.currentTimeMillis() * arr.hashCode());

        for( int i = 0; N > i; i++) {

            int g = r.nextInt(10);
            //System.out.println("random nr" + g);
            arr[i] = g;
        }

        return arr;
    }

    public static void generateRandomNums(int N, String filename) {
        try {

            OutputStream os = new FileOutputStream("numbers.txt");

            PrintWriter pw = new PrintWriter(os);


            Random r = new Random(System.currentTimeMillis());

            for( int i = 0; N > i; i++) {
                int g = r.nextInt(10);
                pw.println(g);
                //System.out.println(g);
            }

            pw.flush();
        } catch (IOException ioe) {
            ioe.printStackTrace();
        }
    }
}
