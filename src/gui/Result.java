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

	private double duration = 0.0d;
	private Event event;
	private Profile profile;
	private JPanel mainpanel;
	
	private JButton btnRemoveButton;
	private JButton btnNewButton;

	/**
	 * Create the panel.
	 */
	public Result(Event _event, Profile _profile, boolean _resultlist, JPanel _mainpanel) {

		event = _event;
		profile = _profile;
		mainpanel = _mainpanel;

		double reducedPrice = (double)(event.getPriceInCentChild() * profile.getChildCounter()) / 100.0;
		double adultPrice = (double)(event.getPriceInCentAdult() * profile.getAdultCounter()) / 100.0;

		GridBagLayout gridBagLayout = new GridBagLayout();
		gridBagLayout.columnWidths = new int[]{0, 0};
		gridBagLayout.rowHeights = new int[]{0, 0, 0, 0, 0};
		gridBagLayout.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gridBagLayout.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
		setLayout(gridBagLayout);

		JPanel panel = new JPanel();
		GridBagConstraints gbc_panel = new GridBagConstraints();
		gbc_panel.anchor = GridBagConstraints.WEST;
		gbc_panel.insets = new Insets(0, 0, 5, 0);
		gbc_panel.fill = GridBagConstraints.VERTICAL;
		gbc_panel.gridx = 0;
		gbc_panel.gridy = 0;
		add(panel, gbc_panel);

		JLabel lblTitle = new JLabel(event.getName());
		lblTitle.setFont(new Font("Tahoma", Font.BOLD, 15));
		panel.add(lblTitle);

		JPanel panel_1 = new JPanel();
		GridBagConstraints gbc_panel_1 = new GridBagConstraints();
		gbc_panel_1.insets = new Insets(0, 0, 5, 0);
		gbc_panel_1.fill = GridBagConstraints.BOTH;
		gbc_panel_1.gridx = 0;
		gbc_panel_1.gridy = 1;
		add(panel_1, gbc_panel_1);
		GridBagLayout gbl_panel_1 = new GridBagLayout();
		gbl_panel_1.columnWidths = new int[]{0, 0, 0, 0, 0, 0};
		gbl_panel_1.rowHeights = new int[]{0, 0, 0};
		gbl_panel_1.columnWeights = new double[]{0.0, 0.0, 0.0, 1.0, 1.0, Double.MIN_VALUE};
		gbl_panel_1.rowWeights = new double[]{0.0, 0.0, Double.MIN_VALUE};
		panel_1.setLayout(gbl_panel_1);

		JLabel lblKinder = new JLabel("Kinder");
		GridBagConstraints gbc_lblKinder = new GridBagConstraints();
		gbc_lblKinder.anchor = GridBagConstraints.WEST;
		gbc_lblKinder.insets = new Insets(0, 0, 5, 5);
		gbc_lblKinder.gridx = 0;
		gbc_lblKinder.gridy = 0;
		panel_1.add(lblKinder, gbc_lblKinder);

		JLabel lblChildren = new JLabel(String.valueOf(profile.getChildCounter()));
		GridBagConstraints gbc_lblChildren = new GridBagConstraints();
		gbc_lblChildren.insets = new Insets(0, 0, 5, 5);
		gbc_lblChildren.gridx = 1;
		gbc_lblChildren.gridy = 0;
		panel_1.add(lblChildren, gbc_lblChildren);

		JLabel lblReducedPrice = new JLabel(String.format( "%.2f", reducedPrice ) + " \u20AC");
		GridBagConstraints gbc_lblReducedPrice = new GridBagConstraints();
		gbc_lblReducedPrice.anchor = GridBagConstraints.WEST;
		gbc_lblReducedPrice.insets = new Insets(0, 0, 5, 5);
		gbc_lblReducedPrice.gridx = 3;
		gbc_lblReducedPrice.gridy = 0;
		panel_1.add(lblReducedPrice, gbc_lblReducedPrice);

		JLabel lblOpeningHours = new JLabel("\u00D6ffnungszeiten");
		GridBagConstraints gbc_lblOpeningHours = new GridBagConstraints();
		gbc_lblOpeningHours.insets = new Insets(0, 0, 5, 0);
		gbc_lblOpeningHours.gridx = 4;
		gbc_lblOpeningHours.gridy = 0;
		panel_1.add(lblOpeningHours, gbc_lblOpeningHours);

		JLabel lblErwachsene = new JLabel("Erwachsene");
		GridBagConstraints gbc_lblErwachsene = new GridBagConstraints();
		gbc_lblErwachsene.insets = new Insets(0, 0, 0, 5);
		gbc_lblErwachsene.anchor = GridBagConstraints.WEST;
		gbc_lblErwachsene.gridx = 0;
		gbc_lblErwachsene.gridy = 1;
		panel_1.add(lblErwachsene, gbc_lblErwachsene);

		JLabel lblAdult = new JLabel(String.valueOf(profile.getAdultCounter()));
		GridBagConstraints gbc_lblAdult = new GridBagConstraints();
		gbc_lblAdult.insets = new Insets(0, 0, 0, 5);
		gbc_lblAdult.gridx = 1;
		gbc_lblAdult.gridy = 1;
		panel_1.add(lblAdult, gbc_lblAdult);

		JLabel lblAdultPrice = new JLabel(String.format( "%.2f", adultPrice ) + " \u20AC");
		GridBagConstraints gbc_lblAdultPrice = new GridBagConstraints();
		gbc_lblAdultPrice.anchor = GridBagConstraints.WEST;
		gbc_lblAdultPrice.insets = new Insets(0, 0, 0, 5);
		gbc_lblAdultPrice.gridx = 3;
		gbc_lblAdultPrice.gridy = 1;
		panel_1.add(lblAdultPrice, gbc_lblAdultPrice);

		JPanel panel_3 = new JPanel();
		GridBagConstraints gbc_panel_3 = new GridBagConstraints();
		gbc_panel_3.insets = new Insets(0, 0, 5, 0);
		gbc_panel_3.fill = GridBagConstraints.BOTH;
		gbc_panel_3.gridx = 0;
		gbc_panel_3.gridy = 2;
		add(panel_3, gbc_panel_3);
		GridBagLayout gbl_panel_3 = new GridBagLayout();
		gbl_panel_3.columnWidths = new int[]{52, 200, 0};
		gbl_panel_3.rowHeights = new int[]{26, 0, 0, 0, 0, 0, 0};
		gbl_panel_3.columnWeights = new double[]{0.0, 1.0, Double.MIN_VALUE};
		gbl_panel_3.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
		panel_3.setLayout(gbl_panel_3);

		if(_resultlist){
			JLabel lblTag = new JLabel("Tag");
			GridBagConstraints gbc_lblTag = new GridBagConstraints();
			gbc_lblTag.insets = new Insets(0, 0, 5, 5);
			gbc_lblTag.anchor = GridBagConstraints.WEST;
			gbc_lblTag.gridx = 0;
			gbc_lblTag.gridy = 0;
			panel_3.add(lblTag, gbc_lblTag);

			JComboBox<String> comboBox = new JComboBox<String>();
			comboBox.setModel(new DefaultComboBoxModel<String>(new String[] {"Tag 1", "Tag 2"}));
			GridBagConstraints gbc_comboBox = new GridBagConstraints();
			gbc_comboBox.fill = GridBagConstraints.HORIZONTAL;
			gbc_comboBox.insets = new Insets(0, 0, 5, 0);
			gbc_comboBox.gridx = 1;
			gbc_comboBox.gridy = 0;
			panel_3.add(comboBox, gbc_comboBox);

			JLabel lblAnkunft = new JLabel("Ankunft");
			GridBagConstraints gbc_lblAnkunft = new GridBagConstraints();
			gbc_lblAnkunft.insets = new Insets(0, 0, 5, 5);
			gbc_lblAnkunft.anchor = GridBagConstraints.WEST;
			gbc_lblAnkunft.gridx = 0;
			gbc_lblAnkunft.gridy = 1;
			panel_3.add(lblAnkunft, gbc_lblAnkunft);

			JComboBox<String> comboBoxArrival = new JComboBox<String>();
			comboBoxArrival.setModel(new DefaultComboBoxModel<String>(new String[] {"Zu Fuss", "Fahrrad", "\u00D6PNV", "Auto"}));
			GridBagConstraints gbc_comboBoxArrival = new GridBagConstraints();
			gbc_comboBoxArrival.fill = GridBagConstraints.HORIZONTAL;
			gbc_comboBoxArrival.insets = new Insets(0, 0, 5, 0);
			gbc_comboBoxArrival.gridx = 1;
			gbc_comboBoxArrival.gridy = 1;
			panel_3.add(comboBoxArrival, gbc_comboBoxArrival);

			JLabel lblBeginn = new JLabel("Beginn");
			GridBagConstraints gbc_lblBeginn = new GridBagConstraints();
			gbc_lblBeginn.anchor = GridBagConstraints.WEST;
			gbc_lblBeginn.insets = new Insets(0, 0, 5, 5);
			gbc_lblBeginn.gridx = 0;
			gbc_lblBeginn.gridy = 2;
			panel_3.add(lblBeginn, gbc_lblBeginn);

			JSlider sliderStarttime = new JSlider();
			sliderStarttime.setValue(9);
			sliderStarttime.setMaximum(24);
			GridBagConstraints gbc_sliderStarttime = new GridBagConstraints();
			gbc_sliderStarttime.insets = new Insets(0, 0, 5, 0);
			gbc_sliderStarttime.fill = GridBagConstraints.HORIZONTAL;
			gbc_sliderStarttime.anchor = GridBagConstraints.NORTH;
			gbc_sliderStarttime.gridx = 1;
			gbc_sliderStarttime.gridy = 2;
			panel_3.add(sliderStarttime, gbc_sliderStarttime);

			JLabel lblStarttime = new JLabel("9 Uhr");
			GridBagConstraints gbc_lblStarttime = new GridBagConstraints();
			gbc_lblStarttime.insets = new Insets(0, 0, 5, 0);
			gbc_lblStarttime.gridx = 1;
			gbc_lblStarttime.gridy = 3;
			panel_3.add(lblStarttime, gbc_lblStarttime);

			JLabel lblDauer = new JLabel("Dauer");
			GridBagConstraints gbc_lblDauer = new GridBagConstraints();
			gbc_lblDauer.anchor = GridBagConstraints.WEST;
			gbc_lblDauer.insets = new Insets(0, 0, 5, 5);
			gbc_lblDauer.gridx = 0;
			gbc_lblDauer.gridy = 4;
			panel_3.add(lblDauer, gbc_lblDauer);

			JSlider sliderDuration = new JSlider();
			sliderDuration.setValue(9);
			sliderDuration.setMaximum(96);
			GridBagConstraints gbc_sliderDuration = new GridBagConstraints();
			gbc_sliderDuration.insets = new Insets(0, 0, 5, 0);
			gbc_sliderDuration.fill = GridBagConstraints.HORIZONTAL;
			gbc_sliderDuration.gridx = 1;
			gbc_sliderDuration.gridy = 4;
			panel_3.add(sliderDuration, gbc_sliderDuration);

			JLabel lblDuration = new JLabel("2.25 h");
			GridBagConstraints gbc_lblDuration = new GridBagConstraints();
			gbc_lblDuration.gridx = 1;
			gbc_lblDuration.gridy = 5;
			panel_3.add(lblDuration, gbc_lblDuration);

			// Funktionen
			sliderStarttime.addChangeListener(new ChangeListener() {
				public void stateChanged(ChangeEvent arg0) {
					starttime = sliderStarttime.getValue();
					lblStarttime.setText(String.valueOf(starttime) + " Uhr");
				}
			});

			sliderDuration.addChangeListener(new ChangeListener() {
				public void stateChanged(ChangeEvent e) {
					duration = (double)sliderDuration.getValue() / 4.0;
					lblDuration.setText(String.valueOf(duration) + " h");
				}
			});
		}

		JPanel panel_2 = new JPanel();
		FlowLayout flowLayout = (FlowLayout) panel_2.getLayout();
		flowLayout.setAlignment(FlowLayout.RIGHT);
		GridBagConstraints gbc_panel_2 = new GridBagConstraints();
		gbc_panel_2.fill = GridBagConstraints.BOTH;
		gbc_panel_2.gridx = 0;
		gbc_panel_2.gridy = 3;
		add(panel_2, gbc_panel_2);

		btnRemoveButton = new JButton("-");
		btnRemoveButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				
			}
		});
		panel_2.add(btnRemoveButton);

		if(_resultlist){
			btnNewButton = new JButton("+");
			btnNewButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent arg0) {
				}
			});
			panel_2.add(btnNewButton);
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
	public double getDuration() {
		return duration;
	}

	/**
	 * @param duration the duration to set
	 */
	public void setDuration(double duration) {
		this.duration = duration;
	}

	/**
	 * @return the event
	 */
	public Event getEvent() {
		return event;
	}

	/**
	 * @return the profile
	 */
	public Profile getProfile() {
		return profile;
	}
}
