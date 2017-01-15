
import java.awt.Color;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;

import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.TransferHandler;
import javax.swing.text.JTextComponent;

public class DnDDemo2 {
  public static void main(String[] args) {
    JFrame frame = new JFrame();
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.setContentPane(new JPanel());

    JTextField textField = new JTextField(25);
    textField.setText("Let's swing higher");
    frame.add(textField);

    JTextArea textArea = new JTextArea("Demonstrating\ndrag and drop");
    textArea.setForeground(Color.red);
    frame.add(new JScrollPane(textArea));

    textArea.setDragEnabled(true);
    textField.setDragEnabled(true);
    TextColorTransferHandler transferHandler = new TextColorTransferHandler();
    textArea.setTransferHandler(transferHandler);
    textField.setTransferHandler(transferHandler);
    frame.pack();
    frame.setVisible(true);
  }
}

class TextColorTransferHandler extends TransferHandler {

  public int getSourceActions(JComponent c) {
    return COPY_OR_MOVE;
  }

  protected Transferable createTransferable(JComponent component) {
    String text = ((JTextComponent) component).getText();
    Color color = component.getForeground();
    TextColor transferable = new TextColor(text, color);
    return transferable;
  }

  public boolean canImport(JComponent c, DataFlavor[] flavors) {
    return true;
  }

  public boolean importData(JComponent component, Transferable transferable) {
    String colorMimeType = DataFlavor.javaJVMLocalObjectMimeType + ";class=java.awt.Color";
    JTextComponent textComponent = (JTextComponent) component;
    try {
      DataFlavor colorFlavor = new DataFlavor(colorMimeType);
      Color color = (Color) transferable.getTransferData(colorFlavor);
      String text = (String) transferable.getTransferData(DataFlavor.stringFlavor);
      textComponent.setForeground(color);
      textComponent.setText(text);
    } catch (Exception e) {
      e.printStackTrace();
    }
    return true;
  }
}

class TextColor implements Transferable {
  private String text;

  private Color color;

  private DataFlavor[] flavors;

  public TextColor(String text, Color color) {
    String colorMimeType = DataFlavor.javaJVMLocalObjectMimeType + ";class=java.awt.Color";
    DataFlavor colorFlavor = null;
    try {
      colorFlavor = new DataFlavor(colorMimeType);
    } catch (ClassNotFoundException e) {
    }
    flavors = new DataFlavor[2];
    flavors[0] = DataFlavor.stringFlavor;
    flavors[1] = colorFlavor;
    this.text = text;
    this.color = color;
  }

  public DataFlavor[] getTransferDataFlavors() {
    return (DataFlavor[]) flavors.clone();
  }

  public boolean isDataFlavorSupported(DataFlavor flavor) {
    for (int i = 0; i < flavors.length; i++) {
      if (flavor.equals(flavors[i])) {
        return true;
      }
    }
    return false;
  }

  public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException, IOException {
    if (flavor.equals(flavors[0])) {
      return text;
    } else if (flavor.equals(flavors[1])) {
      return color;
    } else {
      throw new UnsupportedFlavorException(flavor);
    }
  }
}

//Reference:
//Java 6 New Features: A Tutorial
//by Budi Kurniawan 
//Brainy Software Corp. 2006
//Chapter 4 - Networking
//# ISBN-10: 0975212885
//# ISBN-13: 978-0975212882

        