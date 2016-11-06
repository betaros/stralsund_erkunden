package src;

import java.awt.EventQueue;

import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;

import gui.TestGui;
import prolog.PrologConnector;

public class Test {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		PrologConnector plc = new PrologConnector();
		plc.calcDistance("Citti", "Hansedom");
		plc.getCategoriesByProlog();

		// Starte GUI
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				try {
					UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
				} 
				catch (UnsupportedLookAndFeelException e) {
					// handle exception
					try {
						// Set cross-platform Java L&F (also called "Metal")
						UIManager.setLookAndFeel(UIManager.getCrossPlatformLookAndFeelClassName());
					} catch (ClassNotFoundException | InstantiationException | IllegalAccessException
							| UnsupportedLookAndFeelException e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					}
				}
				catch (ClassNotFoundException e) {
					// handle exception
				}
				catch (InstantiationException e) {
					// handle exception
				}
				catch (IllegalAccessException e) {
					// handle exception
				}

				try {
					TestGui window = new TestGui();
					window.testFrame.setVisible(true);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		});
	}

}
