package gui;

import javax.swing.JPanel;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import javax.swing.JLabel;
import java.awt.FlowLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JSlider;
import javax.swing.event.ChangeListener;

import src.Event;
import src.Profile;

import javax.swing.event.ChangeEvent;
import java.awt.Font;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

public class Result extends JPanel {

	private static final long serialVersionUID = -647953753569806672L;

	private int starttime = 0;
	private int day = 1;
	private int duration = 0;

	private JComboBox<String> comboBoxDay;
	private JSlider sliderStarttime;
	private JSlider sliderDuration;
	
	private Event event;
	private Profile profile;

	public JButton btnRemoveButton;
	public JButton btnNewButton;

	/**
	 * Create the panel.
	 */
	public Result(Event _event, Profile _profile, boolean _resultlist) {

		event = _event;
		profile = _profile;
		
		duration = event.getDuration();
		day = event.getDay();
		starttime = event.getStartTime();

		int reducedPriceInCents = event.getPriceInCentChild() * profile.getChildCounter();
		int adultPriceInCents = event.getPriceInCentAdult() * profile.getAdultCounter();
		int totalPriceInCents = reducedPriceInCents + adultPriceInCents;
		
		double reducedPrice = (double)(reducedPriceInCents) / 100.0;
		double adultPrice = (double)(adultPriceInCents) / 100.0;
		double totalPrice = (double)(totalPriceInCents) / 100.0;
		
		GridBagLayout gridBagLayout = new GridBagLayout();
		gridBagLayout.columnWidths = new int[]{0, 0};
		gridBagLayout.rowHeights = new int[]{0, 0, 0, 0, 0};
		gridBagLayout.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gridBagLayout.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
		setLayout(gridBagLayout);

		JPanel panelTitle = new JPanel();
		GridBagConstraints gbc_panelTitle = new GridBagConstraints();
		gbc_panelTitle.anchor = GridBagConstraints.WEST;
		gbc_panelTitle.insets = new Insets(0, 0, 5, 0);
		gbc_panelTitle.fill = GridBagConstraints.VERTICAL;
		gbc_panelTitle.gridx = 0;
		gbc_panelTitle.gridy = 0;
		add(panelTitle, gbc_panelTitle);

		JLabel lblTitle = new JLabel(event.getName());
		lblTitle.setFont(new Font("Tahoma", Font.BOLD, 15));
		panelTitle.add(lblTitle);

		JPanel panelCost = new JPanel();
		GridBagConstraints gbc_panelCost = new GridBagConstraints();
		gbc_panelCost.insets = new Insets(0, 0, 5, 0);
		gbc_panelCost.fill = GridBagConstraints.BOTH;
		gbc_panelCost.gridx = 0;
		gbc_panelCost.gridy = 1;
		add(panelCost, gbc_panelCost);
		GridBagLayout gbl_panelCost = new GridBagLayout();
		gbl_panelCost.columnWidths = new int[]{0, 0, 0, 0, 0, 0};
		gbl_panelCost.rowHeights = new int[]{0, 0, 0};
		gbl_panelCost.columnWeights = new double[]{0.0, 0.0, 0.0, 1.0, 1.0, Double.MIN_VALUE};
		gbl_panelCost.rowWeights = new double[]{0.0, 0.0, Double.MIN_VALUE};
		panelCost.setLayout(gbl_panelCost);

		JLabel lblChildren = new JLabel(String.valueOf(profile.getChildCounter()));
		GridBagConstraints gbc_lblChildren = new GridBagConstraints();
		gbc_lblChildren.insets = new Insets(0, 0, 5, 5);
		gbc_lblChildren.gridx = 0;
		gbc_lblChildren.gridy = 0;
		panelCost.add(lblChildren, gbc_lblChildren);

		JLabel lblKinder = new JLabel("");
		if(profile.getChildCounter()==1){
			lblKinder.setText("Kind");
		} else {
			lblKinder.setText("Kinder");
		}
		GridBagConstraints gbc_lblKinder = new GridBagConstraints();
		gbc_lblKinder.anchor = GridBagConstraints.WEST;
		gbc_lblKinder.insets = new Insets(0, 0, 5, 5);
		gbc_lblKinder.gridx = 1;
		gbc_lblKinder.gridy = 0;
		panelCost.add(lblKinder, gbc_lblKinder);

		JLabel lblReducedPrice = new JLabel(String.format( "%.2f", reducedPrice ) + " \u20AC");
		GridBagConstraints gbc_lblReducedPrice = new GridBagConstraints();
		gbc_lblReducedPrice.anchor = GridBagConstraints.WEST;
		gbc_lblReducedPrice.insets = new Insets(0, 0, 5, 5);
		gbc_lblReducedPrice.gridx = 3;
		gbc_lblReducedPrice.gridy = 0;
		panelCost.add(lblReducedPrice, gbc_lblReducedPrice);

		JLabel lblTotalCost = new JLabel("Gesamtkosten");
		GridBagConstraints gbc_lblTotalCost = new GridBagConstraints();
		gbc_lblTotalCost.insets = new Insets(0, 0, 5, 0);
		gbc_lblTotalCost.gridx = 4;
		gbc_lblTotalCost.gridy = 0;
		panelCost.add(lblTotalCost, gbc_lblTotalCost);

		JLabel lblAdult = new JLabel(String.valueOf(profile.getAdultCounter()));
		GridBagConstraints gbc_lblAdult = new GridBagConstraints();
		gbc_lblAdult.insets = new Insets(0, 0, 0, 5);
		gbc_lblAdult.gridx = 0;
		gbc_lblAdult.gridy = 1;
		panelCost.add(lblAdult, gbc_lblAdult);

		JLabel lblErwachsene = new JLabel("");
		if(profile.getAdultCounter()==1){
			lblErwachsene.setText("Erwachsener");
		} else {
			lblErwachsene.setText("Erwachsene");
		}
		GridBagConstraints gbc_lblErwachsene = new GridBagConstraints();
		gbc_lblErwachsene.insets = new Insets(0, 0, 0, 5);
		gbc_lblErwachsene.anchor = GridBagConstraints.WEST;
		gbc_lblErwachsene.gridx = 1;
		gbc_lblErwachsene.gridy = 1;
		panelCost.add(lblErwachsene, gbc_lblErwachsene);

		JLabel lblAdultPrice = new JLabel(String.format( "%.2f", adultPrice ) + " \u20AC");
		GridBagConstraints gbc_lblAdultPrice = new GridBagConstraints();
		gbc_lblAdultPrice.anchor = GridBagConstraints.WEST;
		gbc_lblAdultPrice.insets = new Insets(0, 0, 0, 5);
		gbc_lblAdultPrice.gridx = 3;
		gbc_lblAdultPrice.gridy = 1;
		panelCost.add(lblAdultPrice, gbc_lblAdultPrice);

		JLabel lblTotalCostResult = new JLabel(String.format( "%.2f", totalPrice ) + " \u20AC");
		lblTotalCostResult.setFont(new Font("Tahoma", Font.PLAIN, 16));
		GridBagConstraints gbc_lblTotalCostResult = new GridBagConstraints();
		gbc_lblTotalCostResult.gridx = 4;
		gbc_lblTotalCostResult.gridy = 1;
		panelCost.add(lblTotalCostResult, gbc_lblTotalCostResult);

		JPanel panelSettings = new JPanel();
		GridBagConstraints gbc_panelSettings = new GridBagConstraints();
		gbc_panelSettings.insets = new Insets(0, 0, 5, 0);
		gbc_panelSettings.fill = GridBagConstraints.BOTH;
		gbc_panelSettings.gridx = 0;
		gbc_panelSettings.gridy = 2;
		add(panelSettings, gbc_panelSettings);
		GridBagLayout gbl_panelSettings = new GridBagLayout();
		gbl_panelSettings.columnWidths = new int[]{52, 200, 0};
		gbl_panelSettings.rowHeights = new int[]{26, 0, 0, 0, 0, 0};
		gbl_panelSettings.columnWeights = new double[]{0.0, 1.0, Double.MIN_VALUE};
		gbl_panelSettings.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
		panelSettings.setLayout(gbl_panelSettings);

		JLabel lblTag = new JLabel("Tag");
		GridBagConstraints gbc_lblTag = new GridBagConstraints();
		gbc_lblTag.insets = new Insets(0, 0, 5, 5);
		gbc_lblTag.anchor = GridBagConstraints.WEST;
		gbc_lblTag.gridx = 0;
		gbc_lblTag.gridy = 0;
		panelSettings.add(lblTag, gbc_lblTag);

		comboBoxDay = new JComboBox<String>();
		comboBoxDay.setEnabled(_resultlist);
		comboBoxDay.setModel(new DefaultComboBoxModel<String>(new String[] {"1", "2"}));
		comboBoxDay.setSelectedIndex(event.getDay()-1);
		GridBagConstraints gbc_comboBoxDay = new GridBagConstraints();
		gbc_comboBoxDay.fill = GridBagConstraints.HORIZONTAL;
		gbc_comboBoxDay.insets = new Insets(0, 0, 5, 0);
		gbc_comboBoxDay.gridx = 1;
		gbc_comboBoxDay.gridy = 0;
		panelSettings.add(comboBoxDay, gbc_comboBoxDay);

		JLabel lblBeginn = new JLabel("Beginn");
		GridBagConstraints gbc_lblBeginn = new GridBagConstraints();
		gbc_lblBeginn.anchor = GridBagConstraints.WEST;
		gbc_lblBeginn.insets = new Insets(0, 0, 5, 5);
		gbc_lblBeginn.gridx = 0;
		gbc_lblBeginn.gridy = 1;
		panelSettings.add(lblBeginn, gbc_lblBeginn);

		sliderStarttime = new JSlider();
		sliderStarttime.setEnabled(_resultlist);
		sliderStarttime.setValue(event.getStartTime()/60);
		sliderStarttime.setMaximum(24);
		GridBagConstraints gbc_sliderStarttime = new GridBagConstraints();
		gbc_sliderStarttime.insets = new Insets(0, 0, 5, 0);
		gbc_sliderStarttime.fill = GridBagConstraints.HORIZONTAL;
		gbc_sliderStarttime.anchor = GridBagConstraints.NORTH;
		gbc_sliderStarttime.gridx = 1;
		gbc_sliderStarttime.gridy = 1;
		panelSettings.add(sliderStarttime, gbc_sliderStarttime);

		JLabel lblStarttime = new JLabel(String.valueOf(sliderStarttime.getValue()) + " Uhr");
		GridBagConstraints gbc_lblStarttime = new GridBagConstraints();
		gbc_lblStarttime.insets = new Insets(0, 0, 5, 0);
		gbc_lblStarttime.gridx = 1;
		gbc_lblStarttime.gridy = 2;
		panelSettings.add(lblStarttime, gbc_lblStarttime);

		JLabel lblDauer = new JLabel("Dauer");
		GridBagConstraints gbc_lblDauer = new GridBagConstraints();
		gbc_lblDauer.anchor = GridBagConstraints.WEST;
		gbc_lblDauer.insets = new Insets(0, 0, 5, 5);
		gbc_lblDauer.gridx = 0;
		gbc_lblDauer.gridy = 3;
		panelSettings.add(lblDauer, gbc_lblDauer);

		sliderDuration = new JSlider();
		sliderDuration.setEnabled(_resultlist);
		sliderDuration.setValue(duration/60*4);
		sliderDuration.setMaximum(96);
		GridBagConstraints gbc_sliderDuration = new GridBagConstraints();
		gbc_sliderDuration.insets = new Insets(0, 0, 5, 0);
		gbc_sliderDuration.fill = GridBagConstraints.HORIZONTAL;
		gbc_sliderDuration.gridx = 1;
		gbc_sliderDuration.gridy = 3;
		panelSettings.add(sliderDuration, gbc_sliderDuration);

		JLabel lblDuration = new JLabel(String.valueOf(event.getDuration()/60) + " h");
		GridBagConstraints gbc_lblDuration = new GridBagConstraints();
		gbc_lblDuration.gridx = 1;
		gbc_lblDuration.gridy = 4;
		panelSettings.add(lblDuration, gbc_lblDuration);

		// Funktionen
		sliderStarttime.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent arg0) {
				starttime = sliderStarttime.getValue() * 60;
				event.setStartTime(starttime);
				lblStarttime.setText(String.valueOf(starttime / 60) + " Uhr");
			}
		});

		sliderDuration.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				duration = sliderDuration.getValue() / 4 * 60;
				event.setDuration(duration);
				lblDuration.setText(String.valueOf(duration / 60) + " h");
			}
		});

		comboBoxDay.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				day = comboBoxDay.getSelectedIndex() + 1;
				event.setDay(day);
			}
		});


		JPanel panelButtons = new JPanel();
		FlowLayout fl_panelButtons = (FlowLayout) panelButtons.getLayout();
		fl_panelButtons.setAlignment(FlowLayout.RIGHT);
		GridBagConstraints gbc_panelButtons = new GridBagConstraints();
		gbc_panelButtons.fill = GridBagConstraints.BOTH;
		gbc_panelButtons.gridx = 0;
		gbc_panelButtons.gridy = 3;
		add(panelButtons, gbc_panelButtons);

		btnRemoveButton = new JButton("-");
		panelButtons.add(btnRemoveButton);

		if(_resultlist){
			btnNewButton = new JButton("+");
			panelButtons.add(btnNewButton);
		}
	}

	/**
	 * @return the starttime
	 */
	public int getStarttime() {
		return starttime;
	}

	/**
	 * @param starttime the starttime to set
	 */
	public void setStarttime(int starttime) {
		this.starttime = starttime;
	}

	/**
	 * @return the duration
	 */
	public int getDuration() {
		return duration;
	}

	/**
	 * @param duration the duration to set
	 */
	public void setDuration(int duration) {
		this.duration = duration;
	}

	/**
	 * @return the event
	 */
	public Event getEvent() {
		event.setDay(day);
		event.setDuration(duration);
		event.setStartTime(starttime);
		Event tempEvent = event;
		tempEvent.setDay(comboBoxDay.getSelectedIndex() + 1);
		tempEvent.setDuration(sliderDuration.getValue() / 4 * 60);
		tempEvent.setStartTime(sliderStarttime.getValue() * 60);
		return tempEvent;
	}

	/**
	 * @return the profile
	 */
	public Profile getProfile() {
		return profile;
	}
}
