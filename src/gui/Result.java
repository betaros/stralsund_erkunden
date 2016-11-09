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
import javax.swing.event.ChangeEvent;

public class Result extends JPanel {

	private static final long serialVersionUID = -647953753569806672L;
	private int starttime = 0;
	private double duration = 0.0d;
	
	/**
	 * Create the panel.
	 */
	public Result() {
		GridBagLayout gridBagLayout = new GridBagLayout();
		gridBagLayout.columnWidths = new int[]{0, 0};
		gridBagLayout.rowHeights = new int[]{0, 0, 0, 0, 0};
		gridBagLayout.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gridBagLayout.rowWeights = new double[]{0.0, 0.0, 1.0, 0.0, Double.MIN_VALUE};
		setLayout(gridBagLayout);
		
		JPanel panel = new JPanel();
		GridBagConstraints gbc_panel = new GridBagConstraints();
		gbc_panel.anchor = GridBagConstraints.WEST;
		gbc_panel.insets = new Insets(0, 0, 5, 0);
		gbc_panel.fill = GridBagConstraints.VERTICAL;
		gbc_panel.gridx = 0;
		gbc_panel.gridy = 0;
		add(panel, gbc_panel);
		
		JLabel lblTitle = new JLabel("Title");
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
		gbl_panel_1.columnWeights = new double[]{0.0, 0.0, 0.0, 1.0, 0.0, Double.MIN_VALUE};
		gbl_panel_1.rowWeights = new double[]{0.0, 0.0, Double.MIN_VALUE};
		panel_1.setLayout(gbl_panel_1);
		
		JLabel lblKinder = new JLabel("Kinder");
		GridBagConstraints gbc_lblKinder = new GridBagConstraints();
		gbc_lblKinder.anchor = GridBagConstraints.WEST;
		gbc_lblKinder.insets = new Insets(0, 0, 5, 5);
		gbc_lblKinder.gridx = 0;
		gbc_lblKinder.gridy = 0;
		panel_1.add(lblKinder, gbc_lblKinder);
		
		JLabel label = new JLabel("-");
		GridBagConstraints gbc_label = new GridBagConstraints();
		gbc_label.insets = new Insets(0, 0, 5, 5);
		gbc_label.gridx = 1;
		gbc_label.gridy = 0;
		panel_1.add(label, gbc_label);
		
		JLabel label_1 = new JLabel("3,00 \u20AC");
		GridBagConstraints gbc_label_1 = new GridBagConstraints();
		gbc_label_1.anchor = GridBagConstraints.WEST;
		gbc_label_1.insets = new Insets(0, 0, 5, 5);
		gbc_label_1.gridx = 3;
		gbc_label_1.gridy = 0;
		panel_1.add(label_1, gbc_label_1);
		
		JLabel lblffnungszeiten = new JLabel("\u00D6ffnungszeiten");
		GridBagConstraints gbc_lblffnungszeiten = new GridBagConstraints();
		gbc_lblffnungszeiten.insets = new Insets(0, 0, 5, 0);
		gbc_lblffnungszeiten.gridx = 4;
		gbc_lblffnungszeiten.gridy = 0;
		panel_1.add(lblffnungszeiten, gbc_lblffnungszeiten);
		
		JLabel lblErwachsene = new JLabel("Erwachsene");
		GridBagConstraints gbc_lblErwachsene = new GridBagConstraints();
		gbc_lblErwachsene.insets = new Insets(0, 0, 0, 5);
		gbc_lblErwachsene.anchor = GridBagConstraints.WEST;
		gbc_lblErwachsene.gridx = 0;
		gbc_lblErwachsene.gridy = 1;
		panel_1.add(lblErwachsene, gbc_lblErwachsene);
		
		JLabel lblfdgdf = new JLabel("-");
		GridBagConstraints gbc_lblfdgdf = new GridBagConstraints();
		gbc_lblfdgdf.insets = new Insets(0, 0, 0, 5);
		gbc_lblfdgdf.gridx = 1;
		gbc_lblfdgdf.gridy = 1;
		panel_1.add(lblfdgdf, gbc_lblfdgdf);
		
		JLabel label_2 = new JLabel("2,00 \u20AC");
		GridBagConstraints gbc_label_2 = new GridBagConstraints();
		gbc_label_2.anchor = GridBagConstraints.WEST;
		gbc_label_2.insets = new Insets(0, 0, 0, 5);
		gbc_label_2.gridx = 3;
		gbc_label_2.gridy = 1;
		panel_1.add(label_2, gbc_label_2);
		
		JPanel panel_3 = new JPanel();
		GridBagConstraints gbc_panel_3 = new GridBagConstraints();
		gbc_panel_3.insets = new Insets(0, 0, 5, 0);
		gbc_panel_3.fill = GridBagConstraints.BOTH;
		gbc_panel_3.gridx = 0;
		gbc_panel_3.gridy = 2;
		add(panel_3, gbc_panel_3);
		GridBagLayout gbl_panel_3 = new GridBagLayout();
		gbl_panel_3.columnWidths = new int[]{52, 200, 0};
		gbl_panel_3.rowHeights = new int[]{26, 0, 0, 0, 0, 0};
		gbl_panel_3.columnWeights = new double[]{0.0, 1.0, Double.MIN_VALUE};
		gbl_panel_3.rowWeights = new double[]{0.0, 0.0, 0.0, 0.0, 0.0, Double.MIN_VALUE};
		panel_3.setLayout(gbl_panel_3);
		
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
		
		JLabel lblBeginn = new JLabel("Beginn");
		GridBagConstraints gbc_lblBeginn = new GridBagConstraints();
		gbc_lblBeginn.anchor = GridBagConstraints.WEST;
		gbc_lblBeginn.insets = new Insets(0, 0, 5, 5);
		gbc_lblBeginn.gridx = 0;
		gbc_lblBeginn.gridy = 1;
		panel_3.add(lblBeginn, gbc_lblBeginn);
		
		JSlider sliderStarttime = new JSlider();
		sliderStarttime.setMaximum(24);
		GridBagConstraints gbc_sliderStarttime = new GridBagConstraints();
		gbc_sliderStarttime.insets = new Insets(0, 0, 5, 0);
		gbc_sliderStarttime.fill = GridBagConstraints.HORIZONTAL;
		gbc_sliderStarttime.anchor = GridBagConstraints.NORTH;
		gbc_sliderStarttime.gridx = 1;
		gbc_sliderStarttime.gridy = 1;
		panel_3.add(sliderStarttime, gbc_sliderStarttime);
		
		JLabel lblStarttime = new JLabel("9 Uhr");
		GridBagConstraints gbc_lblStarttime = new GridBagConstraints();
		gbc_lblStarttime.insets = new Insets(0, 0, 5, 0);
		gbc_lblStarttime.gridx = 1;
		gbc_lblStarttime.gridy = 2;
		panel_3.add(lblStarttime, gbc_lblStarttime);
		
		JLabel lblDauer = new JLabel("Dauer");
		GridBagConstraints gbc_lblDauer = new GridBagConstraints();
		gbc_lblDauer.anchor = GridBagConstraints.WEST;
		gbc_lblDauer.insets = new Insets(0, 0, 5, 5);
		gbc_lblDauer.gridx = 0;
		gbc_lblDauer.gridy = 3;
		panel_3.add(lblDauer, gbc_lblDauer);
		
		JSlider sliderDuration = new JSlider();
		sliderDuration.setMaximum(96);
		GridBagConstraints gbc_sliderDuration = new GridBagConstraints();
		gbc_sliderDuration.insets = new Insets(0, 0, 5, 0);
		gbc_sliderDuration.fill = GridBagConstraints.HORIZONTAL;
		gbc_sliderDuration.gridx = 1;
		gbc_sliderDuration.gridy = 3;
		panel_3.add(sliderDuration, gbc_sliderDuration);
		
		JLabel lblDuration = new JLabel("1h 15min");
		GridBagConstraints gbc_lblDuration = new GridBagConstraints();
		gbc_lblDuration.gridx = 1;
		gbc_lblDuration.gridy = 4;
		panel_3.add(lblDuration, gbc_lblDuration);
		
		JPanel panel_2 = new JPanel();
		FlowLayout flowLayout = (FlowLayout) panel_2.getLayout();
		flowLayout.setAlignment(FlowLayout.RIGHT);
		GridBagConstraints gbc_panel_2 = new GridBagConstraints();
		gbc_panel_2.fill = GridBagConstraints.BOTH;
		gbc_panel_2.gridx = 0;
		gbc_panel_2.gridy = 3;
		add(panel_2, gbc_panel_2);
		
		JButton button = new JButton("-");
		panel_2.add(button);
		
		JButton btnNewButton = new JButton("+");
		panel_2.add(btnNewButton);
		
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

}
