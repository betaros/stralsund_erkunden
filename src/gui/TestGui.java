package gui;

import javax.swing.JFrame;

public class TestGui {

	public JFrame testFrame;

	/**
	 * Create the application.
	 */
	public TestGui() {
		initialize();
	}

	/**
	 * Initialize the contents of the frame.
	 */
	private void initialize() {
		testFrame = new JFrame();
		testFrame.setBounds(100, 100, 450, 300);
		testFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	}

}
