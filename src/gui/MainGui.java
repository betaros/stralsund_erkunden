package gui;

// https://github.com/msteiger/jxmapviewer2

import javax.swing.JFrame;
import java.awt.BorderLayout;

import javax.swing.JPanel;
import javax.swing.border.TitledBorder;

import src.Event;
import src.Profile;

import javax.swing.JLabel;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.util.ArrayList;

import javax.swing.JButton;
import java.awt.Toolkit;
import javax.swing.UIManager;
import java.awt.Color;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.CardLayout;

public class MainGui {

	public JFrame frmStralsundErkunden;
	private Profile profile;
	private MainPanel mp;
	private ProfileEditor pe;
	private boolean profileVisible;

	/**
	 * Create the application.
	 */
	public MainGui() {

		initialize();
	}

	/**
	 * Initialize the contents of the frame.
	 */
	private void initialize() {
		frmStralsundErkunden = new JFrame();
		frmStralsundErkunden.setIconImage(Toolkit.getDefaultToolkit().getImage(MainGui.class.getResource("/gui/icon.ico")));
		frmStralsundErkunden.setTitle("Stralsund erkunden");
		frmStralsundErkunden.setBounds(100, 100, 969, 635);
		frmStralsundErkunden.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		boolean categories[][] = new boolean[2][2]; 
		profile = new Profile(2, 20000, 2, 1, categories); 

		mp = new MainPanel();
		pe = new ProfileEditor(profile);
		profileVisible = false;

		JPanel content = new JPanel();
		CardLayout cl = new CardLayout();
		content.setLayout(cl);
		content.add(pe, "profileeditor");
		content.add(mp, "mainpanel");
		cl.show(content, "mainpanel");

		frmStralsundErkunden.getContentPane().add(content, BorderLayout.CENTER);

		JPanel profilpanel = new JPanel();
		frmStralsundErkunden.getContentPane().add(profilpanel, BorderLayout.WEST);
		profilpanel.setBorder(new TitledBorder(null, "Profil", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		GridBagLayout gbl_profilpanel = new GridBagLayout();
		gbl_profilpanel.columnWidths = new int[]{161, 0};
		gbl_profilpanel.rowHeights = new int[]{0, 0, 0, 0};
		gbl_profilpanel.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gbl_profilpanel.rowWeights = new double[]{0.0, 1.0, 0.0, Double.MIN_VALUE};
		profilpanel.setLayout(gbl_profilpanel);

		JButton profileButton = new JButton("Profil bearbeiten");
		GridBagConstraints gbc_profileButton = new GridBagConstraints();
		gbc_profileButton.insets = new Insets(0, 0, 5, 0);
		gbc_profileButton.gridx = 0;
		gbc_profileButton.gridy = 0;
		profilpanel.add(profileButton, gbc_profileButton);

		JPanel summarypanel = new JPanel();
		summarypanel.setBorder(new TitledBorder(UIManager.getBorder("TitledBorder.border"), "Zusammenfassung", TitledBorder.LEADING, TitledBorder.TOP, null, new Color(0, 0, 0)));
		GridBagConstraints gbc_summarypanel = new GridBagConstraints();
		gbc_summarypanel.insets = new Insets(0, 0, 5, 0);
		gbc_summarypanel.fill = GridBagConstraints.BOTH;
		gbc_summarypanel.gridx = 0;
		gbc_summarypanel.gridy = 1;
		profilpanel.add(summarypanel, gbc_summarypanel);
		GridBagLayout gbl_summarypanel = new GridBagLayout();
		gbl_summarypanel.columnWidths = new int[]{63, 0};
		gbl_summarypanel.rowHeights = new int[]{23, 0, 0};
		gbl_summarypanel.columnWeights = new double[]{0.0, Double.MIN_VALUE};
		gbl_summarypanel.rowWeights = new double[]{0.0, 0.0, Double.MIN_VALUE};
		summarypanel.setLayout(gbl_summarypanel);

		JButton btnSuchen = new JButton("Suchen");
		GridBagConstraints gbc_btnSuchen = new GridBagConstraints();
		gbc_btnSuchen.fill = GridBagConstraints.HORIZONTAL;
		gbc_btnSuchen.gridx = 0;
		gbc_btnSuchen.gridy = 2;
		profilpanel.add(btnSuchen, gbc_btnSuchen);

		/**********************************************************************************************/
		/* Funktionen */

		// Profil Manager starten
		profileButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(profileVisible){
					// zeige ProfileEditor
					profileButton.setText("Ergebnisse anzeigen");
					profileVisible = false;
					cl.show(content, "mainpanel");
				} else {
					// zeige Ergebnisse
					profileButton.setText("Profil bearbeiten");
					profileVisible = true;
					cl.show(content, "profileeditor");
				}
			}
		});
	}

	/**
	 * Erstellt eine Ergebnisliste
	 * 
	 * @param eventList
	 */
	private ArrayList<JPanel> generateResultPanels(ArrayList<Event> eventList){
		ArrayList<JPanel> resultlist = new ArrayList<JPanel>();

		for(Event e:eventList){
			JPanel panel = new JPanel();

			JLabel eventTitle = new JLabel(e.getName());
			panel.add(eventTitle);

			String open = String.valueOf(e.getOpen());
			open = new StringBuilder(open).insert(open.length()-2, ".").toString();
			JLabel eventOpen = new JLabel(open);
			panel.add(eventOpen);

			String closed = String.valueOf(e.getClosed());
			closed = new StringBuilder(closed).insert(closed.length()-2, ".").toString();
			JLabel eventClose = new JLabel(closed);
			panel.add(eventClose);

			String priceInCent = String.valueOf(e.getPriceInCent());
			priceInCent = new StringBuilder(priceInCent).insert(priceInCent.length()-2, ".").toString();
			JLabel eventPrice = new JLabel(String.valueOf(e.getClosed()));
			panel.add(eventPrice);

			JButton addEventToListButton = new JButton();
			addEventToListButton.setText("+");

			resultlist.add(panel);
		}

		return resultlist;
	}
}
