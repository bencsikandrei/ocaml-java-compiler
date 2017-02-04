import com.sun.deploy.util.ArrayUtil;

import java.io.*;
import java.util.Random;
import java.util.Scanner;

public class Selection {

    private static void print(Comparable [] a) {

        for( Comparable d : a) {

            //System.out.print(d + " ");

        }
        //System.out.println();
    }

    private static void swap(Comparable [] a, int i, int j) {
        Comparable temp = a[i];
        a[i] = a[j];
        a[j] = temp;
    }

    /**
     * Selection sort goes through the array, starting at pos 0 and searches
     * for the minimum element, once found it puts it in the good position
     *
     * The invariant is: left side is always sorted !
     *
     * @param a
     */
    public static void sort(Comparable [] a) {
        int minPos = 0;

        for( int out = 0; a.length - 1 > out; out++) {
            // set the min to be the element we are point in the outer loop
            Comparable min = a[out];
            minPos = out;
            for( int in = out + 1; a.length > in; in++) {
                //System.out.println("Compare " + a[in] + " and " + min);
                if( a[in].compareTo(min) < 0 ) {
                   // System.out.println("We found smaller " + a[in]);
                    minPos = in;
                    min = a[in];
                }
            }
            /*System.out.println("Min is " + min);
            System.out.println("Swapping " + out + " and " + minPos);;*/
            swap(a, out, minPos);

        }
    }

    private static void generateRandomNums(int N) {
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

    public static void main(String[] args) {

        int N = Integer.parseInt(args[0]);

        generateRandomNums(N);


        long start = 0, end = 0;

        try {
            InputStream is = new FileInputStream("numbers.txt");

            Scanner sc = new Scanner(is);

            Integer [] a = new Integer[N];
            for( int i =0 ; sc.hasNext(); i++) {
                a[i] = new Integer(sc.nextInt());
            }
            start = System.currentTimeMillis();

            Selection.sort(a);

            end = System.currentTimeMillis();
            //print(a);


        } catch (Exception ex) {
            ex.printStackTrace();
        }

        System.out.println("Time ellapsed = " + ((end-start)));
    }

}
