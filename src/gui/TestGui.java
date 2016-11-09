package gui;

import javax.swing.JFrame;

import prolog.PrologConnector;

import javax.swing.DefaultListModel;
import javax.swing.JCheckBox;

import java.awt.BorderLayout;
import javax.swing.ListModel;
import java.awt.SystemColor;
import javax.swing.JPanel;

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
		
		// Hole Kategorien aus dem Profil und generiere eine Liste daraus
		DefaultListModel<JCheckBox> model = new DefaultListModel<JCheckBox>();
		PrologConnector pc = new PrologConnector();
		for(String s:pc.findEvents()){
			model.addElement(new JCheckBox(s));
		}
		JCheckBoxList checkBoxList = new JCheckBoxList((ListModel<JCheckBox>) model);
		checkBoxList.setBackground(SystemColor.menu);
		testFrame.getContentPane().add(checkBoxList, BorderLayout.CENTER);
		
		// Events anzeigen
		JPanel panel = new JPanel();
		testFrame.getContentPane().add(panel, BorderLayout.SOUTH);
		
		Result result = new Result();
		panel.add(result);
	}

}
