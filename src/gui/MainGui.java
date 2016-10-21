package gui;

// https://github.com/msteiger/jxmapviewer2

import javax.swing.JFrame;
import javax.swing.JSplitPane;
import java.awt.BorderLayout;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;

import org.jxmapviewer.JXMapViewer;
import org.jxmapviewer.viewer.GeoPosition;

import map.Map;

import javax.swing.JLabel;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.util.ArrayList;

import javax.swing.JTextField;
import javax.swing.JCheckBox;
import java.awt.FlowLayout;

public class MainGui {

	public JFrame frmStralsundErkunden;
	private JTextField textField;
	private JTextField textField_1;
	private JTextField textField_2;

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
		frmStralsundErkunden.setTitle("Stralsund erkunden");
		frmStralsundErkunden.setBounds(100, 100, 969, 635);
		frmStralsundErkunden.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		JSplitPane splitPane_1 = new JSplitPane();
		frmStralsundErkunden.getContentPane().add(splitPane_1, BorderLayout.CENTER);
		
		JPanel resultpanel = new JPanel();
		splitPane_1.setLeftComponent(resultpanel);
		
		JSplitPane splitPane = new JSplitPane();
		splitPane.setOrientation(JSplitPane.VERTICAL_SPLIT);
		splitPane_1.setRightComponent(splitPane);
		
		JPanel routingpanel = new JPanel();
		splitPane.setLeftComponent(routingpanel);
		
		JPanel mappanel = new JPanel();
		splitPane.setRightComponent(mappanel);
		GridBagLayout gbl_mappanel = new GridBagLayout();
		gbl_mappanel.columnWidths = new int[]{652, 0};
		gbl_mappanel.rowHeights = new int[]{118, 0};
		gbl_mappanel.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gbl_mappanel.rowWeights = new double[]{1.0, Double.MIN_VALUE};
		mappanel.setLayout(gbl_mappanel);
		
		//JXMapViewer mapViewer = new JXMapViewer();
		//JXMapViewer mapViewer = showMap();
		Map map = new Map();
		ArrayList<GeoPosition> waypoints = new ArrayList<GeoPosition>();
		waypoints.add(new GeoPosition(54.3199026, 13.0416835));
		waypoints.add(new GeoPosition(54.3200465, 13.0446653));
		waypoints.add(new GeoPosition(54.315509,13.0949513));
		waypoints.add(new GeoPosition(54.311055, 13.090076));
		JXMapViewer mapViewer = map.getMap(waypoints);
		
		mapViewer.setLayout(new FlowLayout(FlowLayout.CENTER, 5, 5));
		
		GridBagConstraints gbc_mapViewer = new GridBagConstraints();
		gbc_mapViewer.fill = GridBagConstraints.BOTH;
		gbc_mapViewer.gridx = 0;
		gbc_mapViewer.gridy = 0;
		mappanel.add(mapViewer, gbc_mapViewer);
		
		JPanel profilpanel = new JPanel();
		frmStralsundErkunden.getContentPane().add(profilpanel, BorderLayout.WEST);
		profilpanel.setBorder(new TitledBorder(null, "Profil", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		GridBagLayout gbl_profilpanel = new GridBagLayout();
		gbl_profilpanel.columnWidths = new int[]{161, 0};
		gbl_profilpanel.rowHeights = new int[]{68, 0, 0, 0};
		gbl_profilpanel.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gbl_profilpanel.rowWeights = new double[]{0.0, 0.0, 1.0, Double.MIN_VALUE};
		profilpanel.setLayout(gbl_profilpanel);
		
		JPanel peoplepanel = new JPanel();
		peoplepanel.setBorder(new TitledBorder(null, "Personen", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		GridBagConstraints gbc_peoplepanel = new GridBagConstraints();
		gbc_peoplepanel.fill = GridBagConstraints.HORIZONTAL;
		gbc_peoplepanel.anchor = GridBagConstraints.NORTH;
		gbc_peoplepanel.insets = new Insets(0, 0, 5, 0);
		gbc_peoplepanel.gridx = 0;
		gbc_peoplepanel.gridy = 0;
		profilpanel.add(peoplepanel, gbc_peoplepanel);
		GridBagLayout gbl_peoplepanel = new GridBagLayout();
		gbl_peoplepanel.columnWidths = new int[]{0, 0, 0};
		gbl_peoplepanel.rowHeights = new int[]{0, 0, 0};
		gbl_peoplepanel.columnWeights = new double[]{1.0, 0.0, Double.MIN_VALUE};
		gbl_peoplepanel.rowWeights = new double[]{0.0, 0.0, Double.MIN_VALUE};
		peoplepanel.setLayout(gbl_peoplepanel);
		
		textField = new JTextField();
		GridBagConstraints gbc_textField = new GridBagConstraints();
		gbc_textField.insets = new Insets(0, 0, 5, 5);
		gbc_textField.fill = GridBagConstraints.HORIZONTAL;
		gbc_textField.gridx = 0;
		gbc_textField.gridy = 0;
		peoplepanel.add(textField, gbc_textField);
		textField.setColumns(10);
		
		JLabel lblAdult = new JLabel("Erwachsene");
		GridBagConstraints gbc_lblAdult = new GridBagConstraints();
		gbc_lblAdult.insets = new Insets(0, 0, 5, 0);
		gbc_lblAdult.gridx = 1;
		gbc_lblAdult.gridy = 0;
		peoplepanel.add(lblAdult, gbc_lblAdult);
		
		textField_1 = new JTextField();
		GridBagConstraints gbc_textField_1 = new GridBagConstraints();
		gbc_textField_1.insets = new Insets(0, 0, 0, 5);
		gbc_textField_1.fill = GridBagConstraints.HORIZONTAL;
		gbc_textField_1.gridx = 0;
		gbc_textField_1.gridy = 1;
		peoplepanel.add(textField_1, gbc_textField_1);
		textField_1.setColumns(10);
		
		JLabel lblChildren = new JLabel("Kinder");
		GridBagConstraints gbc_lblChildren = new GridBagConstraints();
		gbc_lblChildren.gridx = 1;
		gbc_lblChildren.gridy = 1;
		peoplepanel.add(lblChildren, gbc_lblChildren);
		
		JPanel moneypanel = new JPanel();
		moneypanel.setBorder(new TitledBorder(null, "Budget", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		GridBagConstraints gbc_moneypanel = new GridBagConstraints();
		gbc_moneypanel.insets = new Insets(0, 0, 5, 0);
		gbc_moneypanel.fill = GridBagConstraints.HORIZONTAL;
		gbc_moneypanel.gridx = 0;
		gbc_moneypanel.gridy = 1;
		profilpanel.add(moneypanel, gbc_moneypanel);
		GridBagLayout gbl_moneypanel = new GridBagLayout();
		gbl_moneypanel.columnWidths = new int[]{86, 6, 0};
		gbl_moneypanel.rowHeights = new int[]{20, 0};
		gbl_moneypanel.columnWeights = new double[]{0.0, 0.0, Double.MIN_VALUE};
		gbl_moneypanel.rowWeights = new double[]{0.0, Double.MIN_VALUE};
		moneypanel.setLayout(gbl_moneypanel);
		
		textField_2 = new JTextField();
		GridBagConstraints gbc_textField_2 = new GridBagConstraints();
		gbc_textField_2.anchor = GridBagConstraints.NORTHWEST;
		gbc_textField_2.insets = new Insets(0, 0, 0, 5);
		gbc_textField_2.gridx = 0;
		gbc_textField_2.gridy = 0;
		moneypanel.add(textField_2, gbc_textField_2);
		textField_2.setColumns(10);
		
		JLabel label = new JLabel("\u20AC");
		GridBagConstraints gbc_label = new GridBagConstraints();
		gbc_label.anchor = GridBagConstraints.WEST;
		gbc_label.gridx = 1;
		gbc_label.gridy = 0;
		moneypanel.add(label, gbc_label);
		
		JPanel categoriespanel = new JPanel();
		categoriespanel.setBorder(new TitledBorder(null, "Interessen", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		GridBagConstraints gbc_categoriespanel = new GridBagConstraints();
		gbc_categoriespanel.fill = GridBagConstraints.BOTH;
		gbc_categoriespanel.gridx = 0;
		gbc_categoriespanel.gridy = 2;
		profilpanel.add(categoriespanel, gbc_categoriespanel);
		GridBagLayout gbl_categoriespanel = new GridBagLayout();
		gbl_categoriespanel.columnWidths = new int[]{63, 0};
		gbl_categoriespanel.rowHeights = new int[]{23, 0, 0};
		gbl_categoriespanel.columnWeights = new double[]{0.0, Double.MIN_VALUE};
		gbl_categoriespanel.rowWeights = new double[]{0.0, 0.0, Double.MIN_VALUE};
		categoriespanel.setLayout(gbl_categoriespanel);
		
		JCheckBox chckbxMuseen = new JCheckBox("Museen");
		GridBagConstraints gbc_chckbxMuseen = new GridBagConstraints();
		gbc_chckbxMuseen.fill = GridBagConstraints.HORIZONTAL;
		gbc_chckbxMuseen.anchor = GridBagConstraints.NORTH;
		gbc_chckbxMuseen.insets = new Insets(0, 0, 5, 0);
		gbc_chckbxMuseen.gridx = 0;
		gbc_chckbxMuseen.gridy = 0;
		categoriespanel.add(chckbxMuseen, gbc_chckbxMuseen);
		
		JCheckBox chckbxSchwimmen = new JCheckBox("Schwimmen");
		GridBagConstraints gbc_chckbxSchwimmen = new GridBagConstraints();
		gbc_chckbxSchwimmen.anchor = GridBagConstraints.NORTHWEST;
		gbc_chckbxSchwimmen.gridx = 0;
		gbc_chckbxSchwimmen.gridy = 1;
		categoriespanel.add(chckbxSchwimmen, gbc_chckbxSchwimmen);
	}
	
}
