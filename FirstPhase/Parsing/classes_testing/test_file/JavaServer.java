import java.io.BufferedOutputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.net.ServerSocket;
import java.net.Socket;

public class JavaServer {
    public static void main(String[] args) {
        try {
            System.out.println("Created server..");

            ServerSocket serverSocket = new ServerSocket(12345, 5);
            System.out.println("Server waiting for connection!");
            // accepting
            Socket cl =
                    serverSocket.accept();

            // use the cl to write smth
            OutputStream os = cl.getOutputStream();
            // wrap around it
            BufferedOutputStream bs = new BufferedOutputStream(os);
            // get a print stream
            bs.write("hello".getBytes());
            bs.flush();

        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
}

