package gui;

import javax.swing.JPanel;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import src.Profile;
import javax.swing.border.TitledBorder;
import javax.swing.JCheckBox;
import java.awt.SystemColor;

public class ProfileEditor extends JPanel {

	private static final long serialVersionUID = -1618409303637959365L;
	
	private int adultCounter;
	private int childCounter;
	private int days;
	private int budgetInCent;
	private JTextField textFieldBudget;
	
	private Profile profile;

	/**
	 * Create the panel.
	 */
	public ProfileEditor(Profile _profile) {
		this.profile = _profile;
		
		setBorder(new TitledBorder(null, "Profil Editor", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		GridBagLayout gridBagLayout = new GridBagLayout();
		gridBagLayout.columnWidths = new int[]{0, 0};
		gridBagLayout.rowHeights = new int[]{0, 0};
		gridBagLayout.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gridBagLayout.rowWeights = new double[]{1.0, Double.MIN_VALUE};
		setLayout(gridBagLayout);
		
		JPanel panel = new JPanel();
		GridBagConstraints gbc_panel = new GridBagConstraints();
		gbc_panel.fill = GridBagConstraints.BOTH;
		gbc_panel.gridx = 0;
		gbc_panel.gridy = 0;
		add(panel, gbc_panel);
		GridBagLayout gbl_panel = new GridBagLayout();
		gbl_panel.columnWidths = new int[]{0, 0, 0};
		gbl_panel.rowHeights = new int[]{0, 0, 0, 0, 0};
		gbl_panel.columnWeights = new double[]{0.0, 1.0, Double.MIN_VALUE};
		gbl_panel.rowWeights = new double[]{0.0, 0.0, 0.0, 1.0, Double.MIN_VALUE};
		panel.setLayout(gbl_panel);
		
		JLabel lblPeople = new JLabel("Personen");
		GridBagConstraints gbc_lblPeople = new GridBagConstraints();
		gbc_lblPeople.anchor = GridBagConstraints.EAST;
		gbc_lblPeople.insets = new Insets(0, 0, 5, 5);
		gbc_lblPeople.gridx = 0;
		gbc_lblPeople.gridy = 0;
		panel.add(lblPeople, gbc_lblPeople);
		
		JPanel panel_1 = new JPanel();
		GridBagConstraints gbc_panel_1 = new GridBagConstraints();
		gbc_panel_1.insets = new Insets(0, 0, 5, 0);
		gbc_panel_1.fill = GridBagConstraints.BOTH;
		gbc_panel_1.gridx = 1;
		gbc_panel_1.gridy = 0;
		panel.add(panel_1, gbc_panel_1);
		GridBagLayout gbl_panel_1 = new GridBagLayout();
		gbl_panel_1.columnWidths = new int[]{0, 0, 0, 0, 0};
		gbl_panel_1.rowHeights = new int[]{0, 0, 0};
		gbl_panel_1.columnWeights = new double[]{0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
		gbl_panel_1.rowWeights = new double[]{0.0, 0.0, Double.MIN_VALUE};
		panel_1.setLayout(gbl_panel_1);
		
		JButton buttonRemoveAdult = new JButton("-");
		GridBagConstraints gbc_buttonRemoveAdult = new GridBagConstraints();
		gbc_buttonRemoveAdult.insets = new Insets(0, 0, 5, 5);
		gbc_buttonRemoveAdult.gridx = 0;
		gbc_buttonRemoveAdult.gridy = 0;
		panel_1.add(buttonRemoveAdult, gbc_buttonRemoveAdult);
		
		JButton buttonAddAdult = new JButton("+");
		GridBagConstraints gbc_buttonAddAdult = new GridBagConstraints();
		gbc_buttonAddAdult.insets = new Insets(0, 0, 5, 5);
		gbc_buttonAddAdult.gridx = 1;
		gbc_buttonAddAdult.gridy = 0;
		panel_1.add(buttonAddAdult, gbc_buttonAddAdult);
		
		JLabel lblAdultCounter = new JLabel("0");
		GridBagConstraints gbc_lblAdultCounter = new GridBagConstraints();
		gbc_lblAdultCounter.insets = new Insets(0, 0, 5, 5);
		gbc_lblAdultCounter.gridx = 2;
		gbc_lblAdultCounter.gridy = 0;
		panel_1.add(lblAdultCounter, gbc_lblAdultCounter);
		
		JLabel lblAdult = new JLabel("Erwachsene");
		GridBagConstraints gbc_lblAdult = new GridBagConstraints();
		gbc_lblAdult.anchor = GridBagConstraints.WEST;
		gbc_lblAdult.insets = new Insets(0, 0, 5, 0);
		gbc_lblAdult.gridx = 3;
		gbc_lblAdult.gridy = 0;
		panel_1.add(lblAdult, gbc_lblAdult);
		
		JButton buttonRemoveChild = new JButton("-");
		GridBagConstraints gbc_buttonRemoveChild = new GridBagConstraints();
		gbc_buttonRemoveChild.insets = new Insets(0, 0, 0, 5);
		gbc_buttonRemoveChild.gridx = 0;
		gbc_buttonRemoveChild.gridy = 1;
		panel_1.add(buttonRemoveChild, gbc_buttonRemoveChild);
		
		JButton buttonAddChild = new JButton("+");
		GridBagConstraints gbc_buttonAddChild = new GridBagConstraints();
		gbc_buttonAddChild.insets = new Insets(0, 0, 0, 5);
		gbc_buttonAddChild.gridx = 1;
		gbc_buttonAddChild.gridy = 1;
		panel_1.add(buttonAddChild, gbc_buttonAddChild);
		
		JLabel lblChildCounter = new JLabel("0");
		GridBagConstraints gbc_lblChildCounter = new GridBagConstraints();
		gbc_lblChildCounter.insets = new Insets(0, 0, 0, 5);
		gbc_lblChildCounter.gridx = 2;
		gbc_lblChildCounter.gridy = 1;
		panel_1.add(lblChildCounter, gbc_lblChildCounter);
		
		JLabel lblChild = new JLabel("Kinder");
		GridBagConstraints gbc_lblChild = new GridBagConstraints();
		gbc_lblChild.anchor = GridBagConstraints.WEST;
		gbc_lblChild.gridx = 3;
		gbc_lblChild.gridy = 1;
		panel_1.add(lblChild, gbc_lblChild);
		
		JLabel lblBudget = new JLabel("Budget");
		GridBagConstraints gbc_lblBudget = new GridBagConstraints();
		gbc_lblBudget.anchor = GridBagConstraints.EAST;
		gbc_lblBudget.insets = new Insets(0, 0, 5, 5);
		gbc_lblBudget.gridx = 0;
		gbc_lblBudget.gridy = 1;
		panel.add(lblBudget, gbc_lblBudget);
		
		textFieldBudget = new JTextField();
		GridBagConstraints gbc_textFieldBudget = new GridBagConstraints();
		gbc_textFieldBudget.anchor = GridBagConstraints.WEST;
		gbc_textFieldBudget.insets = new Insets(0, 0, 5, 0);
		gbc_textFieldBudget.gridx = 1;
		gbc_textFieldBudget.gridy = 1;
		panel.add(textFieldBudget, gbc_textFieldBudget);
		textFieldBudget.setColumns(10);
		
		JLabel lblDuration = new JLabel("Dauer");
		GridBagConstraints gbc_lblDuration = new GridBagConstraints();
		gbc_lblDuration.anchor = GridBagConstraints.EAST;
		gbc_lblDuration.insets = new Insets(0, 0, 5, 5);
		gbc_lblDuration.gridx = 0;
		gbc_lblDuration.gridy = 2;
		panel.add(lblDuration, gbc_lblDuration);
		
		JPanel panel_5 = new JPanel();
		GridBagConstraints gbc_panel_5 = new GridBagConstraints();
		gbc_panel_5.insets = new Insets(0, 0, 5, 0);
		gbc_panel_5.fill = GridBagConstraints.BOTH;
		gbc_panel_5.gridx = 1;
		gbc_panel_5.gridy = 2;
		panel.add(panel_5, gbc_panel_5);
		GridBagLayout gbl_panel_5 = new GridBagLayout();
		gbl_panel_5.columnWidths = new int[]{0, 0, 0, 0, 0};
		gbl_panel_5.rowHeights = new int[]{0, 0};
		gbl_panel_5.columnWeights = new double[]{0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
		gbl_panel_5.rowWeights = new double[]{0.0, Double.MIN_VALUE};
		panel_5.setLayout(gbl_panel_5);
		
		JButton buttonRemoveDay = new JButton("-");
		GridBagConstraints gbc_buttonRemoveDay = new GridBagConstraints();
		gbc_buttonRemoveDay.insets = new Insets(0, 0, 0, 5);
		gbc_buttonRemoveDay.gridx = 0;
		gbc_buttonRemoveDay.gridy = 0;
		panel_5.add(buttonRemoveDay, gbc_buttonRemoveDay);
		
		JButton buttonAddDay = new JButton("+");
		GridBagConstraints gbc_buttonAddDay = new GridBagConstraints();
		gbc_buttonAddDay.insets = new Insets(0, 0, 0, 5);
		gbc_buttonAddDay.gridx = 1;
		gbc_buttonAddDay.gridy = 0;
		panel_5.add(buttonAddDay, gbc_buttonAddDay);
		
		JLabel lblDayCounter = new JLabel("2");
		GridBagConstraints gbc_lblDayCounter = new GridBagConstraints();
		gbc_lblDayCounter.insets = new Insets(0, 0, 0, 5);
		gbc_lblDayCounter.gridx = 2;
		gbc_lblDayCounter.gridy = 0;
		panel_5.add(lblDayCounter, gbc_lblDayCounter);
		
		JLabel lblDays = new JLabel("Tage");
		GridBagConstraints gbc_lblDays = new GridBagConstraints();
		gbc_lblDays.gridx = 3;
		gbc_lblDays.gridy = 0;
		panel_5.add(lblDays, gbc_lblDays);
		
		JLabel lblCategories = new JLabel("Kategorien");
		GridBagConstraints gbc_lblCategories = new GridBagConstraints();
		gbc_lblCategories.anchor = GridBagConstraints.NORTHEAST;
		gbc_lblCategories.insets = new Insets(0, 0, 0, 5);
		gbc_lblCategories.gridx = 0;
		gbc_lblCategories.gridy = 3;
		panel.add(lblCategories, gbc_lblCategories);
		
		// Hole Kategorien aus dem Profil und generiere eine Liste daraus
		DefaultListModel<JCheckBox> model = new DefaultListModel<JCheckBox>();
		for(String s:profile.getCategories()){
			model.addElement(new JCheckBox(s));
		}
		//model.addElement(new JCheckBox("Kategorie 1"));
		//model.addElement(new JCheckBox("Kategorie 2"));
		//model.addElement(new JCheckBox("Kategorie 3"));
		
		JCheckBoxList checkBoxList = new JCheckBoxList(model);
		checkBoxList.setBackground(SystemColor.menu);
		GridBagConstraints gbc_list = new GridBagConstraints();
		gbc_list.fill = GridBagConstraints.BOTH;
		gbc_list.gridx = 1;
		gbc_list.gridy = 3;
		panel.add(checkBoxList, gbc_list);
		
		/***********************************************************************************/
		// Profil laden
		adultCounter = profile.getAdultCounter();
		childCounter = profile.getChildCounter();
		days = profile.getDays();
		budgetInCent = profile.getBudgetInCent();
		
		String budgetInCentS = String.valueOf(budgetInCent);
		String budgetInEuro = new StringBuilder(budgetInCentS).insert(budgetInCentS.length()-2, ",").toString();
		
		lblAdultCounter.setText(String.valueOf(adultCounter));
		lblChildCounter.setText(String.valueOf(childCounter));
		lblDayCounter.setText(String.valueOf(days));
		textFieldBudget.setText(budgetInEuro);
		
		/***********************************************************************************/
		// Funktionen
				
		/**
		 * Erwachsenen hinzufügen
		 */
		buttonAddAdult.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				adultCounter++;
				lblAdultCounter.setText(String.valueOf(adultCounter));
			}
		});
		
		/**
		 * Kind hinzufügen
		 */
		buttonAddChild.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				childCounter++;
				lblChildCounter.setText(String.valueOf(childCounter));
			}
		});
		
		/**
		 * Erwachsenen löschen
		 */
		buttonRemoveAdult.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(adultCounter>0){
					adultCounter--;
					lblAdultCounter.setText(String.valueOf(adultCounter));
				}
			}
		});
		
		/**
		 * Kind löschen
		 */
		buttonRemoveChild.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(childCounter>0){
					childCounter--;
					lblChildCounter.setText(String.valueOf(childCounter));
				}
			}
		});
		
		/**
		 * Tag hinzufügen
		 */
		buttonAddDay.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				days++;
				lblDayCounter.setText(String.valueOf(days));
			}
		});
		
		/**
		 * Tag löschen
		 */
		buttonRemoveDay.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(days>0){
					days--;
					lblDayCounter.setText(String.valueOf(days));
				}
			}
		});
	}

	public Profile getProfile(){
		return profile;
	}
	
}
