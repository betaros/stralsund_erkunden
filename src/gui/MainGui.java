package gui;

// https://github.com/msteiger/jxmapviewer2

import javax.swing.JFrame;

import javax.swing.JPanel;
import javax.swing.border.TitledBorder;

import prolog.PrologConnector;
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
import javax.swing.JTextPane;
import java.awt.SystemColor;

public class MainGui {

	public JFrame frmStralsundErkunden;
	private Profile profile;
	private MainPanel mp;
	private ProfileEditor pe;
	private boolean profileVisible;

	private PrologConnector pc;
	/**
	 * Create the application.
	 */
	public MainGui(PrologConnector _pc) {
		this.pc = _pc;
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

		profile = new Profile(2, 20000, 2, 1); 
		profileVisible = false;
		GridBagLayout gridBagLayout = new GridBagLayout();
		gridBagLayout.columnWidths = new int[]{173, 780, 0};
		gridBagLayout.rowHeights = new int[]{596, 0};
		gridBagLayout.columnWeights = new double[]{0.0, 1.0, Double.MIN_VALUE};
		gridBagLayout.rowWeights = new double[]{1.0, Double.MIN_VALUE};
		frmStralsundErkunden.getContentPane().setLayout(gridBagLayout);

		JPanel profilpanel = new JPanel();
		GridBagConstraints gbc_profilpanel = new GridBagConstraints();
		gbc_profilpanel.anchor = GridBagConstraints.WEST;
		gbc_profilpanel.fill = GridBagConstraints.VERTICAL;
		gbc_profilpanel.insets = new Insets(0, 0, 0, 5);
		gbc_profilpanel.gridx = 0;
		gbc_profilpanel.gridy = 0;
		frmStralsundErkunden.getContentPane().add(profilpanel, gbc_profilpanel);
		profilpanel.setBorder(new TitledBorder(null, "Profil", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		GridBagLayout gbl_profilpanel = new GridBagLayout();
		gbl_profilpanel.columnWidths = new int[]{161, 0};
		gbl_profilpanel.rowHeights = new int[]{0, 0, 0, 0};
		gbl_profilpanel.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gbl_profilpanel.rowWeights = new double[]{0.0, 0.0, 1.0, Double.MIN_VALUE};
		profilpanel.setLayout(gbl_profilpanel);

		JButton btnSuchen = new JButton("Suchen");
		GridBagConstraints gbc_btnSuchen = new GridBagConstraints();
		gbc_btnSuchen.insets = new Insets(0, 0, 5, 0);
		gbc_btnSuchen.fill = GridBagConstraints.HORIZONTAL;
		gbc_btnSuchen.gridx = 0;
		gbc_btnSuchen.gridy = 0;
		profilpanel.add(btnSuchen, gbc_btnSuchen);

		JButton profileButton = new JButton("Profil bearbeiten");
		GridBagConstraints gbc_profileButton = new GridBagConstraints();
		gbc_profileButton.fill = GridBagConstraints.HORIZONTAL;
		gbc_profileButton.insets = new Insets(0, 0, 5, 0);
		gbc_profileButton.gridx = 0;
		gbc_profileButton.gridy = 1;
		profilpanel.add(profileButton, gbc_profileButton);

		JPanel summarypanel = new JPanel();
		summarypanel.setBorder(new TitledBorder(UIManager.getBorder("TitledBorder.border"), "Zusammenfassung", TitledBorder.LEADING, TitledBorder.TOP, null, new Color(0, 0, 0)));
		GridBagConstraints gbc_summarypanel = new GridBagConstraints();
		gbc_summarypanel.fill = GridBagConstraints.BOTH;
		gbc_summarypanel.gridx = 0;
		gbc_summarypanel.gridy = 2;
		profilpanel.add(summarypanel, gbc_summarypanel);
		GridBagLayout gbl_summarypanel = new GridBagLayout();
		gbl_summarypanel.columnWidths = new int[]{63, 0};
		gbl_summarypanel.rowHeights = new int[]{23, 0};
		gbl_summarypanel.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gbl_summarypanel.rowWeights = new double[]{1.0, Double.MIN_VALUE};
		summarypanel.setLayout(gbl_summarypanel);

		JTextPane txtpnSummary = new JTextPane();
		txtpnSummary.setForeground(SystemColor.controlDkShadow);
		txtpnSummary.setBackground(SystemColor.menu);
		txtpnSummary.setText("summaryTextPane");
		GridBagConstraints gbc_txtpnSummary = new GridBagConstraints();
		gbc_txtpnSummary.fill = GridBagConstraints.BOTH;
		gbc_txtpnSummary.gridx = 0;
		gbc_txtpnSummary.gridy = 0;
		summarypanel.add(txtpnSummary, gbc_txtpnSummary);

		mp = new MainPanel();
		pe = new ProfileEditor(profile);

		JPanel content = new JPanel();
		CardLayout cl = new CardLayout();
		content.setLayout(cl);
		content.add(pe, "profileeditor");
		content.add(mp, "mainpanel");
		cl.show(content, "mainpanel");

		GridBagConstraints gbc_content = new GridBagConstraints();
		gbc_content.fill = GridBagConstraints.BOTH;
		gbc_content.gridx = 1;
		gbc_content.gridy = 0;
		frmStralsundErkunden.getContentPane().add(content, gbc_content);

		/**********************************************************************************************/
		/* Funktionen */

		// Profil Manager starten
		profileButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(profileVisible){
					// zeige Ergebnisse
					profileButton.setText("Profil bearbeiten");
					profileVisible = false;

					profile = pe.getProfile();
					txtpnSummary.setText(generateSummary());

					cl.show(content, "mainpanel");
				} else {
					// zeige ProfileEditor
					profileButton.setText("Ergebnisse anzeigen");
					profileVisible = true;
					cl.show(content, "profileeditor");
				}
			}
		});
	}

	private String generateSummary(){
		StringBuilder summaryText = new StringBuilder();

		if(profile.getAdultCounter() > 0) {
			summaryText.append(String.valueOf(profile.getAdultCounter()));
			if(profile.getAdultCounter() == 1){
				summaryText.append(" Erwachsener");
			} else {
				summaryText.append(" Erwachsene");
			}
			summaryText.append("\n");
		}

		if(profile.getChildCounter() > 0) {
			summaryText.append(String.valueOf(profile.getChildCounter()));
			if(profile.getChildCounter() == 1){
				summaryText.append(" Kind");
			} else {
				summaryText.append(" Kinder");
			}
			summaryText.append("\n");
		}

		summaryText.append("\n");
		summaryText.append("Kategorien:");
		summaryText.append("\n");
		if(profile.getCategories().isEmpty()){
			summaryText.append("keine");
		} else {
			for(String s:profile.getCategories()){
				summaryText.append(s);
				summaryText.append("\n");
			}
		}

		return summaryText.toString();
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
