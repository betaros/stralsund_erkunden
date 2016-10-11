package src;

/**
 * Prolog <-> Java
 * http://www.swi-prolog.org/packages/jpl/
 */

import java.awt.EventQueue;

import gui.MainGui;

public class Main {
	
	public static void main(String[] args) {
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				try {
					MainGui window = new MainGui();
					window.frmStralsundErkunden.setVisible(true);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		});
	}

}
