import java.io.*;
import java.util.Random;
import java.util.Scanner;

public class Insertion {

    private static void print(Comparable [] a) {

        for( Comparable d : a) {

            System.out.print(d + " ");

        }
        System.out.println();
    }

    private static boolean less(Comparable a, Comparable b) {
        return (a.compareTo(b) < 0);
    }

    private static void swap(Comparable [] a, int i, int j) {
        Comparable temp = a[i];
        a[i] = a[j];
        a[j] = temp;
    }

    /**
     * Insertion sort finds an element that must be swapped by comparing it to all the elems on its left
     * we then move all elements to the right until a position for that element is found
     *
     * The invariant is: left side is partially sorted!
     *
     * @param a
     */
    public static void sort(Comparable [] a) {

        Comparable tmp = 0;
        int pos = -1, in = -1;
        for ( int out = 1; a.length > out; out++) {

            tmp = a[out];

            in = out;

            while ( (in > 0) && less(tmp, a[in-1])) {
                a[in] = a[in-1];
                in--;
            }

            a[in] = tmp;

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

            Insertion.sort(a);

            end = System.currentTimeMillis();
            print(a);


        } catch (Exception ex) {
            ex.printStackTrace();
        }

        System.out.println("Time ellapsed = " + ((end-start)));
    }

}
