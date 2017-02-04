import java.io.BufferedInputStream;
import java.net.Socket;
import java.net.ServerSocket;
import java.net.InetAddress;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Scanner;

public class JavaClient {

    public static void main (String [] args) {

        if ( args.length < 2 ) {
            System.out.println("Please enter an address & port ");
            System.exit(-1);
        }
        // create socket
        try {
            System.out.println("Creating socket ..");
            InetAddress servAddr = InetAddress.getByName("localhost");

            Socket clSock = new Socket(servAddr, Integer.parseInt(args[1]));

            InputStream is = clSock.getInputStream();

            BufferedInputStream bs = new BufferedInputStream(is);
            StringBuilder sb = new StringBuilder("");
            int d = bs.read();

            while( d != -1 ){
                sb.append((char) d);
                //System.out.println((char) d);
                d = bs.read();
            }

            System.out.println(sb.toString());

        } catch (Exception ex) {
            System.out.println("There was a problem ..");
            ex.printStackTrace();
        }

    }

}
