import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;

public class JavaUDPClient {
    public static void main(String[] args) {


        try {
            // create dgram sock
            DatagramSocket ds = new DatagramSocket();
            //
            InetAddress addr = InetAddress.getByName("localhost");
            DatagramPacket dp = new DatagramPacket("hello".getBytes(), 0, "hello".length(), addr, 12345);

            ds.send(dp);

        } catch(Exception ex) {
            ex.printStackTrace();
        }
    }
}
