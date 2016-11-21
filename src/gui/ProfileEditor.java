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
import java.util.ArrayList;
import java.awt.event.ActionEvent;

import src.Profile;
import javax.swing.border.TitledBorder;
import javax.swing.JCheckBox;
import java.awt.SystemColor;
import javax.swing.JComboBox;
import javax.swing.DefaultComboBoxModel;
import java.awt.Component;
import javax.swing.Box;
import javax.swing.JSlider;
import javax.swing.event.ChangeListener;
import javax.swing.event.ChangeEvent;

public class ProfileEditor extends JPanel {

	private static final long serialVersionUID = -1618409303637959365L;

	private int adultCounter;
	private int childCounter;
	private int days;
	private int dayBegin;
	private int dayEnd;
	private int budgetInCent;
	private JTextField textFieldBudget;
	private String arrival;

	private Profile profile;
	private ArrayList<String> selectedCategories;
	private ArrayList<String> selectedFood;
	private ArrayList<String> selectedHotel;
	private DefaultListModel<JCheckBox> modelCategories;
	private DefaultListModel<JCheckBox> modelFood;
	private DefaultListModel<JCheckBox> modelHotel;

	/**
	 * Create the panel.
	 */
	public ProfileEditor(Profile _profile) {
		this.profile = _profile;
		selectedCategories = new ArrayList<String>();
		selectedFood = new ArrayList<String>();
		selectedHotel = new ArrayList<String>();

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
		gbl_panel.columnWidths = new int[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
		gbl_panel.rowHeights = new int[]{0, 0, 0, 0, 0};
		gbl_panel.columnWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, Double.MIN_VALUE};
		gbl_panel.rowWeights = new double[]{0.0, 0.0, 0.0, 1.0, Double.MIN_VALUE};
		panel.setLayout(gbl_panel);

		JLabel lblPeople = new JLabel("Personen");
		GridBagConstraints gbc_lblPeople = new GridBagConstraints();
		gbc_lblPeople.anchor = GridBagConstraints.EAST;
		gbc_lblPeople.insets = new Insets(0, 0, 5, 5);
		gbc_lblPeople.gridx = 0;
		gbc_lblPeople.gridy = 0;
		panel.add(lblPeople, gbc_lblPeople);

		JPanel panelPersons = new JPanel();
		GridBagConstraints gbc_panelPersons = new GridBagConstraints();
		gbc_panelPersons.insets = new Insets(0, 0, 5, 5);
		gbc_panelPersons.fill = GridBagConstraints.BOTH;
		gbc_panelPersons.gridx = 1;
		gbc_panelPersons.gridy = 0;
		panel.add(panelPersons, gbc_panelPersons);
		GridBagLayout gbl_panelPersons = new GridBagLayout();
		gbl_panelPersons.columnWidths = new int[]{0, 0, 0, 0, 0};
		gbl_panelPersons.rowHeights = new int[]{0, 0, 0};
		gbl_panelPersons.columnWeights = new double[]{0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
		gbl_panelPersons.rowWeights = new double[]{0.0, 0.0, Double.MIN_VALUE};
		panelPersons.setLayout(gbl_panelPersons);

		JButton buttonRemoveAdult = new JButton("-");
		GridBagConstraints gbc_buttonRemoveAdult = new GridBagConstraints();
		gbc_buttonRemoveAdult.insets = new Insets(0, 0, 5, 5);
		gbc_buttonRemoveAdult.gridx = 0;
		gbc_buttonRemoveAdult.gridy = 0;
		panelPersons.add(buttonRemoveAdult, gbc_buttonRemoveAdult);

		JButton buttonAddAdult = new JButton("+");
		GridBagConstraints gbc_buttonAddAdult = new GridBagConstraints();
		gbc_buttonAddAdult.insets = new Insets(0, 0, 5, 5);
		gbc_buttonAddAdult.gridx = 1;
		gbc_buttonAddAdult.gridy = 0;
		panelPersons.add(buttonAddAdult, gbc_buttonAddAdult);

		JLabel lblAdultCounter = new JLabel("0");
		GridBagConstraints gbc_lblAdultCounter = new GridBagConstraints();
		gbc_lblAdultCounter.insets = new Insets(0, 0, 5, 5);
		gbc_lblAdultCounter.gridx = 2;
		gbc_lblAdultCounter.gridy = 0;
		panelPersons.add(lblAdultCounter, gbc_lblAdultCounter);

		JLabel lblAdult = new JLabel("Erwachsene");
		GridBagConstraints gbc_lblAdult = new GridBagConstraints();
		gbc_lblAdult.anchor = GridBagConstraints.WEST;
		gbc_lblAdult.insets = new Insets(0, 0, 5, 0);
		gbc_lblAdult.gridx = 3;
		gbc_lblAdult.gridy = 0;
		panelPersons.add(lblAdult, gbc_lblAdult);

		JButton buttonRemoveChild = new JButton("-");
		GridBagConstraints gbc_buttonRemoveChild = new GridBagConstraints();
		gbc_buttonRemoveChild.insets = new Insets(0, 0, 0, 5);
		gbc_buttonRemoveChild.gridx = 0;
		gbc_buttonRemoveChild.gridy = 1;
		panelPersons.add(buttonRemoveChild, gbc_buttonRemoveChild);

		JButton buttonAddChild = new JButton("+");
		GridBagConstraints gbc_buttonAddChild = new GridBagConstraints();
		gbc_buttonAddChild.insets = new Insets(0, 0, 0, 5);
		gbc_buttonAddChild.gridx = 1;
		gbc_buttonAddChild.gridy = 1;
		panelPersons.add(buttonAddChild, gbc_buttonAddChild);

		JLabel lblChildCounter = new JLabel("0");
		GridBagConstraints gbc_lblChildCounter = new GridBagConstraints();
		gbc_lblChildCounter.insets = new Insets(0, 0, 0, 5);
		gbc_lblChildCounter.gridx = 2;
		gbc_lblChildCounter.gridy = 1;
		panelPersons.add(lblChildCounter, gbc_lblChildCounter);

		JLabel lblChild = new JLabel("Kinder");
		GridBagConstraints gbc_lblChild = new GridBagConstraints();
		gbc_lblChild.anchor = GridBagConstraints.WEST;
		gbc_lblChild.gridx = 3;
		gbc_lblChild.gridy = 1;
		panelPersons.add(lblChild, gbc_lblChild);

		Component horizontalStrut = Box.createHorizontalStrut(20);
		GridBagConstraints gbc_horizontalStrut = new GridBagConstraints();
		gbc_horizontalStrut.insets = new Insets(0, 0, 5, 5);
		gbc_horizontalStrut.gridx = 2;
		gbc_horizontalStrut.gridy = 0;
		panel.add(horizontalStrut, gbc_horizontalStrut);

		JLabel lblArrival = new JLabel("Anfahrt");
		GridBagConstraints gbc_lblArrival = new GridBagConstraints();
		gbc_lblArrival.anchor = GridBagConstraints.EAST;
		gbc_lblArrival.insets = new Insets(0, 0, 5, 5);
		gbc_lblArrival.gridx = 3;
		gbc_lblArrival.gridy = 0;
		panel.add(lblArrival, gbc_lblArrival);

		JComboBox<String> comboBoxArrival = new JComboBox<String>();
		comboBoxArrival.setModel(new DefaultComboBoxModel<String>(new String[] {"Zu Fuss", "Bus", "Fahrrad", "Auto"}));
		GridBagConstraints gbc_comboBoxArrival = new GridBagConstraints();
		gbc_comboBoxArrival.anchor = GridBagConstraints.WEST;
		gbc_comboBoxArrival.insets = new Insets(0, 0, 5, 5);
		gbc_comboBoxArrival.gridx = 4;
		gbc_comboBoxArrival.gridy = 0;
		panel.add(comboBoxArrival, gbc_comboBoxArrival);

		Component horizontalStrut_1 = Box.createHorizontalStrut(100);
		GridBagConstraints gbc_horizontalStrut_1 = new GridBagConstraints();
		gbc_horizontalStrut_1.insets = new Insets(0, 0, 5, 5);
		gbc_horizontalStrut_1.gridx = 5;
		gbc_horizontalStrut_1.gridy = 0;
		panel.add(horizontalStrut_1, gbc_horizontalStrut_1);

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
		gbc_textFieldBudget.insets = new Insets(0, 0, 5, 5);
		gbc_textFieldBudget.gridx = 1;
		gbc_textFieldBudget.gridy = 1;
		panel.add(textFieldBudget, gbc_textFieldBudget);
		textFieldBudget.setColumns(10);

		JLabel lblDayBegin = new JLabel("Tagesbeginn");
		GridBagConstraints gbc_lblDayBegin = new GridBagConstraints();
		gbc_lblDayBegin.anchor = GridBagConstraints.EAST;
		gbc_lblDayBegin.insets = new Insets(0, 0, 5, 5);
		gbc_lblDayBegin.gridx = 3;
		gbc_lblDayBegin.gridy = 1;
		panel.add(lblDayBegin, gbc_lblDayBegin);

		JSlider sliderDayBegin = new JSlider();
		sliderDayBegin.setValue(16);
		sliderDayBegin.setMaximum(48);
		GridBagConstraints gbc_sliderDayBegin = new GridBagConstraints();
		gbc_sliderDayBegin.fill = GridBagConstraints.HORIZONTAL;
		gbc_sliderDayBegin.insets = new Insets(0, 0, 5, 5);
		gbc_sliderDayBegin.gridx = 4;
		gbc_sliderDayBegin.gridy = 1;
		panel.add(sliderDayBegin, gbc_sliderDayBegin);

		JLabel lblDayEnd = new JLabel("Tagesende");
		GridBagConstraints gbc_lblDayEnd = new GridBagConstraints();
		gbc_lblDayEnd.anchor = GridBagConstraints.EAST;
		gbc_lblDayEnd.insets = new Insets(0, 0, 5, 5);
		gbc_lblDayEnd.gridx = 6;
		gbc_lblDayEnd.gridy = 1;
		panel.add(lblDayEnd, gbc_lblDayEnd);

		JSlider sliderDayEnd = new JSlider();
		sliderDayEnd.setMaximum(48);
		sliderDayEnd.setValue(44);
		GridBagConstraints gbc_sliderDayEnd = new GridBagConstraints();
		gbc_sliderDayEnd.fill = GridBagConstraints.HORIZONTAL;
		gbc_sliderDayEnd.insets = new Insets(0, 0, 5, 5);
		gbc_sliderDayEnd.gridx = 7;
		gbc_sliderDayEnd.gridy = 1;
		panel.add(sliderDayEnd, gbc_sliderDayEnd);

		JLabel lblDuration = new JLabel("Dauer");
		GridBagConstraints gbc_lblDuration = new GridBagConstraints();
		gbc_lblDuration.anchor = GridBagConstraints.EAST;
		gbc_lblDuration.insets = new Insets(0, 0, 5, 5);
		gbc_lblDuration.gridx = 0;
		gbc_lblDuration.gridy = 2;
		panel.add(lblDuration, gbc_lblDuration);

		JPanel panelDuration = new JPanel();
		GridBagConstraints gbc_panelDuration = new GridBagConstraints();
		gbc_panelDuration.insets = new Insets(0, 0, 5, 5);
		gbc_panelDuration.fill = GridBagConstraints.BOTH;
		gbc_panelDuration.gridx = 1;
		gbc_panelDuration.gridy = 2;
		panel.add(panelDuration, gbc_panelDuration);
		GridBagLayout gbl_panelDuration = new GridBagLayout();
		gbl_panelDuration.columnWidths = new int[]{0, 0, 0, 0, 0};
		gbl_panelDuration.rowHeights = new int[]{0, 0};
		gbl_panelDuration.columnWeights = new double[]{0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
		gbl_panelDuration.rowWeights = new double[]{0.0, Double.MIN_VALUE};
		panelDuration.setLayout(gbl_panelDuration);

		JButton buttonRemoveDay = new JButton("-");
		buttonRemoveDay.setEnabled(false);
		GridBagConstraints gbc_buttonRemoveDay = new GridBagConstraints();
		gbc_buttonRemoveDay.insets = new Insets(0, 0, 0, 5);
		gbc_buttonRemoveDay.gridx = 0;
		gbc_buttonRemoveDay.gridy = 0;
		panelDuration.add(buttonRemoveDay, gbc_buttonRemoveDay);

		JButton buttonAddDay = new JButton("+");
		buttonAddDay.setEnabled(false);
		GridBagConstraints gbc_buttonAddDay = new GridBagConstraints();
		gbc_buttonAddDay.insets = new Insets(0, 0, 0, 5);
		gbc_buttonAddDay.gridx = 1;
		gbc_buttonAddDay.gridy = 0;
		panelDuration.add(buttonAddDay, gbc_buttonAddDay);

		JLabel lblDayCounter = new JLabel("2");
		GridBagConstraints gbc_lblDayCounter = new GridBagConstraints();
		gbc_lblDayCounter.insets = new Insets(0, 0, 0, 5);
		gbc_lblDayCounter.gridx = 2;
		gbc_lblDayCounter.gridy = 0;
		panelDuration.add(lblDayCounter, gbc_lblDayCounter);

		JLabel lblDays = new JLabel("Tage");
		GridBagConstraints gbc_lblDays = new GridBagConstraints();
		gbc_lblDays.gridx = 3;
		gbc_lblDays.gridy = 0;
		panelDuration.add(lblDays, gbc_lblDays);

		JLabel lblSpinnerValueDayBegin = new JLabel("8.00 Uhr");
		GridBagConstraints gbc_lblSpinnerValueDayBegin = new GridBagConstraints();
		gbc_lblSpinnerValueDayBegin.insets = new Insets(0, 0, 5, 5);
		gbc_lblSpinnerValueDayBegin.gridx = 4;
		gbc_lblSpinnerValueDayBegin.gridy = 2;
		panel.add(lblSpinnerValueDayBegin, gbc_lblSpinnerValueDayBegin);

		JLabel lblSpinnerValueDayEnd = new JLabel("22.00 Uhr");
		GridBagConstraints gbc_lblSpinnerValueDayEnd = new GridBagConstraints();
		gbc_lblSpinnerValueDayEnd.insets = new Insets(0, 0, 5, 5);
		gbc_lblSpinnerValueDayEnd.gridx = 7;
		gbc_lblSpinnerValueDayEnd.gridy = 2;
		panel.add(lblSpinnerValueDayEnd, gbc_lblSpinnerValueDayEnd);

		JLabel lblCategories = new JLabel("Kategorien");
		GridBagConstraints gbc_lblCategories = new GridBagConstraints();
		gbc_lblCategories.anchor = GridBagConstraints.NORTHEAST;
		gbc_lblCategories.insets = new Insets(0, 0, 0, 5);
		gbc_lblCategories.gridx = 0;
		gbc_lblCategories.gridy = 3;
		panel.add(lblCategories, gbc_lblCategories);

		// Hole Kategorien aus dem Profil und generiere eine Liste daraus
		modelCategories = new DefaultListModel<JCheckBox>();
		for(String s:profile.getCategories()){
			modelCategories.addElement(new JCheckBox(s));
			modelCategories.get(modelCategories.getSize()-1).setSelected(true);
		}

		JCheckBoxList checkBoxListCategories = new JCheckBoxList(modelCategories);
		checkBoxListCategories.setBackground(SystemColor.menu);
		GridBagConstraints gbc_checkBoxListCategories = new GridBagConstraints();
		gbc_checkBoxListCategories.insets = new Insets(0, 0, 0, 5);
		gbc_checkBoxListCategories.fill = GridBagConstraints.BOTH;
		gbc_checkBoxListCategories.gridx = 1;
		gbc_checkBoxListCategories.gridy = 3;
		panel.add(checkBoxListCategories, gbc_checkBoxListCategories);

		JLabel lblFood = new JLabel("Essen");
		GridBagConstraints gbc_lblFood = new GridBagConstraints();
		gbc_lblFood.anchor = GridBagConstraints.NORTHEAST;
		gbc_lblFood.insets = new Insets(0, 0, 0, 5);
		gbc_lblFood.gridx = 3;
		gbc_lblFood.gridy = 3;
		panel.add(lblFood, gbc_lblFood);
		
		// Hole Essensategorien aus dem Profil und generiere eine Liste daraus
		modelFood = new DefaultListModel<JCheckBox>();
		for(String s:profile.getFood()){
			modelFood.addElement(new JCheckBox(s));
			modelFood.get(modelFood.getSize()-1).setSelected(true);
		}

		JCheckBoxList checkBoxListFood = new JCheckBoxList(modelFood);
		checkBoxListFood.setBackground(SystemColor.menu);
		GridBagConstraints gbc_checkBoxListFood = new GridBagConstraints();
		gbc_checkBoxListFood.insets = new Insets(0, 0, 0, 5);
		gbc_checkBoxListFood.fill = GridBagConstraints.BOTH;
		gbc_checkBoxListFood.gridx = 4;
		gbc_checkBoxListFood.gridy = 3;
		panel.add(checkBoxListFood, gbc_checkBoxListFood);

		JLabel lblHotel = new JLabel("Hotel");
		GridBagConstraints gbc_lblHotel = new GridBagConstraints();
		gbc_lblHotel.anchor = GridBagConstraints.NORTHEAST;
		gbc_lblHotel.insets = new Insets(0, 0, 0, 5);
		gbc_lblHotel.gridx = 6;
		gbc_lblHotel.gridy = 3;
		panel.add(lblHotel, gbc_lblHotel);
		
		// Hole Essensategorien aus dem Profil und generiere eine Liste daraus
		modelHotel = new DefaultListModel<JCheckBox>();
		for(String s:profile.getHotel()){
			modelHotel.addElement(new JCheckBox(s + " Sterne"));
			modelHotel.get(modelHotel.getSize()-1).setSelected(true);
		}

		JCheckBoxList checkBoxListHotel = new JCheckBoxList(modelHotel);
		checkBoxListHotel.setBackground(SystemColor.menu);
		GridBagConstraints gbc_checkBoxListHotel = new GridBagConstraints();
		gbc_checkBoxListHotel.insets = new Insets(0, 0, 0, 5);
		gbc_checkBoxListHotel.fill = GridBagConstraints.BOTH;
		gbc_checkBoxListHotel.gridx = 7;
		gbc_checkBoxListHotel.gridy = 3;
		panel.add(checkBoxListHotel, gbc_checkBoxListHotel);

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
		
		arrival = profile.getArrival();
		// {"Zu Fuss", "Bus", "Fahrrad", "Auto"}
		switch(arrival){
		case "Bus":
			comboBoxArrival.setSelectedIndex(1);
			break;
		case "Fahrrad":
			comboBoxArrival.setSelectedIndex(2);
			break;
		case "Auto":
			comboBoxArrival.setSelectedIndex(3);
			break;
		default:
			comboBoxArrival.setSelectedIndex(0);
			break;
		}

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
		
		/**
		 * Tagesbeginn aendern
		 */
		sliderDayBegin.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent arg0) {
				int value = sliderDayBegin.getValue();
				int hour = value / 2;
				int min = (value % 2) * 3;
				
				lblSpinnerValueDayBegin.setText(String.valueOf(hour) + "." + String.valueOf(min) + "0 Uhr");
				
				dayBegin = ((value * 5) * 60) / 10;
			}
		});
		
		/**
		 * Tagesende aendern
		 */
		sliderDayEnd.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent arg0) {
				int value = sliderDayEnd.getValue();
				int hour = value / 2;
				int min = (value % 2) * 3;
				
				lblSpinnerValueDayEnd.setText(String.valueOf(hour) + "." + String.valueOf(min) + "0 Uhr");
				
				dayEnd = ((value * 5) * 60) / 10;
			}
		});
		
		/**
		 * Anfahrt aendern
		 */
		comboBoxArrival.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				// {"Zu Fuss", "Bus", "Fahrrad", "Auto"}
				switch(comboBoxArrival.getSelectedIndex()){
				case 0:
					arrival = "Zu Fuss";
					break;
				case 1:
					arrival = "Bus";
					break;
				case 2:
					arrival = "Fahrrad";
					break;
				default:
					arrival = "Auto";	
				}
			}
		});
	}

	public Profile getProfile(){
		Profile tempProfile = new Profile(days, budgetInCent, adultCounter, childCounter);
		if(dayBegin<dayEnd){
			tempProfile.setDayStart(dayBegin);
			tempProfile.setDayEnd(dayEnd);
		}

		tempProfile.setArrival(arrival);
		
		selectedCategories.removeAll(selectedCategories);

		for(int i = 0; i < modelCategories.getSize(); i++){
			if(modelCategories.get(i).isSelected()){
				selectedCategories.add(modelCategories.get(i).getText());
			}
		}

		tempProfile.setSelectedCategories(selectedCategories);

		selectedFood.removeAll(selectedFood);

		for(int i = 0; i < modelFood.getSize(); i++){
			if(modelFood.get(i).isSelected()){
				selectedFood.add(modelFood.get(i).getText());
			}
		}

		tempProfile.setSelectedFood(selectedFood);

		selectedHotel.removeAll(selectedHotel);

		for(int i = 0; i < modelHotel.getSize(); i++){
			if(modelHotel.get(i).isSelected()){
				selectedHotel.add(modelHotel.get(i).getText());
			}
		}

		tempProfile.setSelectedHotel(selectedHotel);

		String budget = textFieldBudget.getText();
		budget = budget.replaceAll(",", ".");
		budget = budget.replaceAll("[^0-9.]", "");
		try {
			double budgetDouble = Double.parseDouble(budget) * 100;
			int budgetInt = (int) budgetDouble;
			if(budgetInt >= 0){
				tempProfile.setBudgetInCent(budgetInt);
			}
		} catch (Exception e) {

		}

		return tempProfile;
	}

}
