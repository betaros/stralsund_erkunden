package gui;

import javax.swing.JFrame;

import prolog.PrologConnector;
import src.Event;
import src.Profile;

import javax.swing.DefaultListModel;
import javax.swing.JCheckBox;

import java.awt.BorderLayout;
import javax.swing.ListModel;
import java.awt.SystemColor;
import java.util.ArrayList;
import java.util.Collections;

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
		
		ArrayList<String> eventStringList = new ArrayList<String>();
		ArrayList<String> a = new ArrayList<String>();
		a.add("Freizeit");
		a.removeAll(Collections.singleton(null));
		for(String s:pc.getEventsByPrologWithCategories(a)){
			model.addElement(new JCheckBox(s));
			eventStringList.add(s);
		}
		
		ArrayList<Event> eventList = new ArrayList<Event>();
		for(String s:eventStringList){
			eventList.add(pc.findEvent(s, false));
		}
		
		eventList.removeAll(Collections.singleton(null));
		
		JCheckBoxList checkBoxList = new JCheckBoxList((ListModel<JCheckBox>) model);
		checkBoxList.setBackground(SystemColor.menu);
		testFrame.getContentPane().add(checkBoxList, BorderLayout.CENTER);
		
		// Events anzeigen
		JPanel panel = new JPanel();
		testFrame.getContentPane().add(panel, BorderLayout.SOUTH);
		
		Profile p = new Profile(2, 20000, 2, 1);
		
		for(Event e:eventList){
			Result result = new Result(0, e, p, false);
			panel.add(result);
		}
		
		for(Event e1:eventList){
			System.out.println("Test EventList: " + e1.getName());
		}
	}

}
