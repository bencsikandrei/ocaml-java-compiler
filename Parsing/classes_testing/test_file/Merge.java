import java.io.*;
import java.util.Random;
import java.util.Scanner;


public class Merge {
	int x;
	Merge() {
		x = 10;
	}
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
        Comparable [] aux = new Comparable[a.length];
        // put the values in the vector
        sort(aux, a, 0, a.length - 1);
        //MyUtils.copy(a, aux);

    }

    private static void sort(Comparable [] aux, Comparable [] a, int left, int right) {
       // base case
        if( left >= right )
            return;
        else {
            int mid = left + (right - left) / 2;
            // sort two separate parts
            sort(aux, a, left, mid);
            sort(aux, a, mid + 1, right);
            // merge together
            merge(aux, a, left, mid, right);
        }
    }


    private static void merge(Comparable [] a, Comparable [] orig, int low, int mid, int hi) {
        int i = low;
        int j = mid + 1;

        // copy the needed numbers from original to aux
        for( int k = low; k <=  hi; k++)
            a[k] = orig[k];

        for( int k = low; k <= hi; k++) {
            if( i > mid )
                orig[k] = a[j++];
            else if ( j > hi )
                orig[k] = a[i++];
            else if ( less(a[i], a[j]) )
                orig[k] = a[i++];
             else
                 orig[k] = a[j++];

        }
    }


    public static void main(String[] args) {

        int N = Integer.parseInt(args[0]);
        //mergeTest();
        MyUtils.generateRandomNums(N, "numbers.txt");
        long start = 0, end = 0;

        try {
            InputStream is = new FileInputStream("numbers.txt");

            Scanner sc = new Scanner(is);

            Integer [] a = new Integer[N];
            for( int i =0 ; sc.hasNext(); i++) {
                a[i] = new Integer(sc.nextInt());
            }
            start = System.currentTimeMillis();

            Merge.sort(a);

            end = System.currentTimeMillis();
            print(a);


        } catch (Exception ex) {
            ex.printStackTrace();
        }

        System.out.println("Time ellapsed = " + ((end-start)));
    }

}

