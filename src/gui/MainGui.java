package gui;

// https://github.com/msteiger/jxmapviewer2

import javax.swing.JFrame;

import javax.swing.JPanel;
import javax.swing.border.TitledBorder;

import prolog.PrologConnector;
import src.Profile;

import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.Insets;

import javax.swing.JButton;
import java.awt.Toolkit;
import javax.swing.UIManager;
import java.awt.Color;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.CardLayout;
import javax.swing.JTextPane;
import java.awt.SystemColor;

public class MainGui{

	public JFrame frmStralsundErkunden;
	private Profile profile;
	private MainPanel mp;
	private ProfileEditor pe;
	private boolean profileVisible;

	/**
	 * Create the application.
	 */
	public MainGui(PrologConnector _pc) {
		initialize();
	}

	/**
	 * Initialize the contents of the frame.
	 */
	private void initialize() {
		frmStralsundErkunden = new JFrame();
		frmStralsundErkunden.setIconImage(Toolkit.getDefaultToolkit().getImage(MainGui.class.getResource("/gui/icon.ico")));
		frmStralsundErkunden.setTitle("Stralsund erkunden");
		frmStralsundErkunden.setBounds(100, 100, 1100, 635);
		frmStralsundErkunden.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		profile = new Profile(2, 20000, 2, 1); 
		profileVisible = true;
		GridBagLayout gridBagLayout = new GridBagLayout();
		gridBagLayout.columnWidths = new int[] {173, 1000, 0};
		gridBagLayout.rowHeights = new int[] {800, 0};
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

		JButton profileButton = new JButton("Ergebnisse anzeigen");
		GridBagConstraints gbc_profileButton = new GridBagConstraints();
		gbc_profileButton.fill = GridBagConstraints.HORIZONTAL;
		gbc_profileButton.insets = new Insets(0, 0, 5, 0);
		gbc_profileButton.gridx = 0;
		gbc_profileButton.gridy = 0;
		profilpanel.add(profileButton, gbc_profileButton);

		JButton btnGenerateTimeline = new JButton("F\u00FClle Timeline");
		btnGenerateTimeline.setVisible(false);
		GridBagConstraints gbc_btnGenerateTimeline = new GridBagConstraints();
		gbc_btnGenerateTimeline.fill = GridBagConstraints.HORIZONTAL;
		gbc_btnGenerateTimeline.insets = new Insets(0, 0, 5, 0);
		gbc_btnGenerateTimeline.gridx = 0;
		gbc_btnGenerateTimeline.gridy = 1;
		profilpanel.add(btnGenerateTimeline, gbc_btnGenerateTimeline);

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
		txtpnSummary.setText(generateSummary());
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
		cl.show(content, "profileeditor");

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

					if(!(mp.showSearchResults(profile))){
						txtpnSummary.setText("Bitte Profil bearbeiten!");
					}

					cl.show(content, "mainpanel");
					btnGenerateTimeline.setVisible(true);
				} else {
					// zeige ProfileEditor
					profileButton.setText("Ergebnisse anzeigen");
					profileVisible = true;
					cl.show(content, "profileeditor");
					btnGenerateTimeline.setVisible(false);
				}
			}
		});
		
		btnGenerateTimeline.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				mp.fillTimeplan();
			}
		});
	}

	/**
	 * Erzeugt Zusammenfassung
	 * @return
	 */
	private String generateSummary(){
		StringBuilder summaryText = new StringBuilder();

		summaryText.append("Gesamtkosten: ");
		summaryText.append("\n");
		double totalCost = (double)profile.getTotalCost() / 100.0;
		summaryText.append(String.format( "%.2f", totalCost ) + " \u20AC");
		summaryText.append("\n");
		summaryText.append("\n");

		summaryText.append("Hotelkategorien:");
		summaryText.append("\n");
		for(String s:profile.getSelectedHotel()){
			summaryText.append(s);
			summaryText.append(" Sterne");
			summaryText.append("\n");
		}

		summaryText.append("\n");

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
		summaryText.append("Budget: ");
		summaryText.append(String.format( "%.2f", new Double(profile.getBudgetInCent() / 100.0) ) + " \u20AC");
		summaryText.append("\n");

		summaryText.append("\n");
		summaryText.append("Kategorien:");
		summaryText.append("\n");
		if(profile.getSelectedCategories().isEmpty()){
			summaryText.append("keine");
		} else {
			for(String s:profile.getSelectedCategories()){
				summaryText.append(s);
				summaryText.append("\n");
			}
		}

		return summaryText.toString();
	}
}
