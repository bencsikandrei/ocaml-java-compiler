import java.io.*;
import java.util.Random;
import java.util.Scanner;

public class Quick {

    private static void print(Comparable [] a) {

        for( Comparable d : a) {

            System.out.print(d + " ");

        }
        System.out.println();
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
    private static int partition(Comparable [] a, int left, int right, Comparable pivot) {
        // depends how we choose the pivot
        // simple case, we chose the pivot as the last element to the right
        int rightMarker = right;
        int leftMarker = left - 1;
        // sorting loop
        while( true ) {

            // we commence out search for elements not in place
            while( a[++leftMarker].compareTo(pivot) < 0) {
                ; // nop
            }

            // we commence our search for the right part -> elements smaller than pivot
            while( rightMarker > left && a[--rightMarker].compareTo(pivot) > 0) {
                ; // nop
            }

            if ( leftMarker >= rightMarker )
                // parition is done
                break;

            // otherwise we have not finished and we need to swap the elements
            swap(a, leftMarker, rightMarker);

        }
        // put the pivot in it's place
        swap(a, right, leftMarker);

        return leftMarker;
    }

    public static void sort(Comparable [] a) {
        sort(a, 0, a.length - 1);
    }
    private static void sort(Comparable [] a, int left, int right) {
        // base case
        if( right - left <= 0)
            return;

        else {
            Comparable pivot = a[right];
            int part = partition(a, left, right, pivot);

            // call recursive
            sort(a, left, part - 1);
            sort(a, part + 1, right);
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

            Quick.sort(a);

            end = System.currentTimeMillis();
            //print(a);


        } catch (Exception ex) {
            ex.printStackTrace();
        }

        System.out.println("Time ellapsed = " + ((end-start)));
    }

}
