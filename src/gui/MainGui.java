package gui;

// https://github.com/msteiger/jxmapviewer2

import javax.swing.JFrame;
import javax.swing.JSplitPane;
import java.awt.BorderLayout;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;

import org.jxmapviewer.JXMapViewer;
import org.jxmapviewer.OSMTileFactoryInfo;
import org.jxmapviewer.viewer.DefaultTileFactory;
import org.jxmapviewer.viewer.GeoPosition;
import org.jxmapviewer.viewer.TileFactoryInfo;

import javax.swing.JLabel;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.Insets;
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
		
		JPanel ergebnispanel = new JPanel();
		splitPane_1.setLeftComponent(ergebnispanel);
		
		JSplitPane splitPane = new JSplitPane();
		splitPane.setOrientation(JSplitPane.VERTICAL_SPLIT);
		splitPane_1.setRightComponent(splitPane);
		
		JPanel routenpanel = new JPanel();
		splitPane.setLeftComponent(routenpanel);
		
		JPanel kartenpanel = new JPanel();
		splitPane.setRightComponent(kartenpanel);
		GridBagLayout gbl_kartenpanel = new GridBagLayout();
		gbl_kartenpanel.columnWidths = new int[]{652, 0};
		gbl_kartenpanel.rowHeights = new int[]{118, 0};
		gbl_kartenpanel.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gbl_kartenpanel.rowWeights = new double[]{1.0, Double.MIN_VALUE};
		kartenpanel.setLayout(gbl_kartenpanel);
		JXMapViewer mapViewer = new JXMapViewer();
		
		// Create a TileFactoryInfo for OpenStreetMap
		TileFactoryInfo info = new OSMTileFactoryInfo();
		DefaultTileFactory tileFactory = new DefaultTileFactory(info);
		
		// Use 8 threads in parallel to load the tiles
		tileFactory.setThreadPoolSize(8);

		// Set the focus
		GeoPosition stralsund = new GeoPosition(54.31, 13.08);
		
		mapViewer.setZoom(7);
		mapViewer.setAddressLocation(stralsund);
		mapViewer.setLayout(new FlowLayout(FlowLayout.CENTER, 5, 5));
						
		GridBagConstraints gbc_mapViewer = new GridBagConstraints();
		gbc_mapViewer.fill = GridBagConstraints.BOTH;
		gbc_mapViewer.gridx = 0;
		gbc_mapViewer.gridy = 0;
		kartenpanel.add(mapViewer, gbc_mapViewer);
		
		JPanel profilpanel = new JPanel();
		frmStralsundErkunden.getContentPane().add(profilpanel, BorderLayout.WEST);
		profilpanel.setBorder(new TitledBorder(null, "Profil", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		GridBagLayout gbl_profilpanel = new GridBagLayout();
		gbl_profilpanel.columnWidths = new int[]{161, 0};
		gbl_profilpanel.rowHeights = new int[]{68, 0, 0, 0};
		gbl_profilpanel.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gbl_profilpanel.rowWeights = new double[]{0.0, 0.0, 1.0, Double.MIN_VALUE};
		profilpanel.setLayout(gbl_profilpanel);
		
		JPanel personenpanel = new JPanel();
		personenpanel.setBorder(new TitledBorder(null, "Personen", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		GridBagConstraints gbc_personenpanel = new GridBagConstraints();
		gbc_personenpanel.fill = GridBagConstraints.HORIZONTAL;
		gbc_personenpanel.anchor = GridBagConstraints.NORTH;
		gbc_personenpanel.insets = new Insets(0, 0, 5, 0);
		gbc_personenpanel.gridx = 0;
		gbc_personenpanel.gridy = 0;
		profilpanel.add(personenpanel, gbc_personenpanel);
		GridBagLayout gbl_personenpanel = new GridBagLayout();
		gbl_personenpanel.columnWidths = new int[]{0, 0, 0};
		gbl_personenpanel.rowHeights = new int[]{0, 0, 0};
		gbl_personenpanel.columnWeights = new double[]{1.0, 0.0, Double.MIN_VALUE};
		gbl_personenpanel.rowWeights = new double[]{0.0, 0.0, Double.MIN_VALUE};
		personenpanel.setLayout(gbl_personenpanel);
		
		textField = new JTextField();
		GridBagConstraints gbc_textField = new GridBagConstraints();
		gbc_textField.insets = new Insets(0, 0, 5, 5);
		gbc_textField.fill = GridBagConstraints.HORIZONTAL;
		gbc_textField.gridx = 0;
		gbc_textField.gridy = 0;
		personenpanel.add(textField, gbc_textField);
		textField.setColumns(10);
		
		JLabel lblErwachsene = new JLabel("Erwachsene");
		GridBagConstraints gbc_lblErwachsene = new GridBagConstraints();
		gbc_lblErwachsene.insets = new Insets(0, 0, 5, 0);
		gbc_lblErwachsene.gridx = 1;
		gbc_lblErwachsene.gridy = 0;
		personenpanel.add(lblErwachsene, gbc_lblErwachsene);
		
		textField_1 = new JTextField();
		GridBagConstraints gbc_textField_1 = new GridBagConstraints();
		gbc_textField_1.insets = new Insets(0, 0, 0, 5);
		gbc_textField_1.fill = GridBagConstraints.HORIZONTAL;
		gbc_textField_1.gridx = 0;
		gbc_textField_1.gridy = 1;
		personenpanel.add(textField_1, gbc_textField_1);
		textField_1.setColumns(10);
		
		JLabel lblKinder = new JLabel("Kinder");
		GridBagConstraints gbc_lblKinder = new GridBagConstraints();
		gbc_lblKinder.gridx = 1;
		gbc_lblKinder.gridy = 1;
		personenpanel.add(lblKinder, gbc_lblKinder);
		
		JPanel geldpanel = new JPanel();
		geldpanel.setBorder(new TitledBorder(null, "Budget", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		GridBagConstraints gbc_geldpanel = new GridBagConstraints();
		gbc_geldpanel.insets = new Insets(0, 0, 5, 0);
		gbc_geldpanel.fill = GridBagConstraints.HORIZONTAL;
		gbc_geldpanel.gridx = 0;
		gbc_geldpanel.gridy = 1;
		profilpanel.add(geldpanel, gbc_geldpanel);
		GridBagLayout gbl_geldpanel = new GridBagLayout();
		gbl_geldpanel.columnWidths = new int[]{86, 6, 0};
		gbl_geldpanel.rowHeights = new int[]{20, 0};
		gbl_geldpanel.columnWeights = new double[]{0.0, 0.0, Double.MIN_VALUE};
		gbl_geldpanel.rowWeights = new double[]{0.0, Double.MIN_VALUE};
		geldpanel.setLayout(gbl_geldpanel);
		
		textField_2 = new JTextField();
		GridBagConstraints gbc_textField_2 = new GridBagConstraints();
		gbc_textField_2.anchor = GridBagConstraints.NORTHWEST;
		gbc_textField_2.insets = new Insets(0, 0, 0, 5);
		gbc_textField_2.gridx = 0;
		gbc_textField_2.gridy = 0;
		geldpanel.add(textField_2, gbc_textField_2);
		textField_2.setColumns(10);
		
		JLabel label = new JLabel("\u20AC");
		GridBagConstraints gbc_label = new GridBagConstraints();
		gbc_label.anchor = GridBagConstraints.WEST;
		gbc_label.gridx = 1;
		gbc_label.gridy = 0;
		geldpanel.add(label, gbc_label);
		
		JPanel kategorienpanel = new JPanel();
		kategorienpanel.setBorder(new TitledBorder(null, "Interessen", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		GridBagConstraints gbc_kategorienpanel = new GridBagConstraints();
		gbc_kategorienpanel.fill = GridBagConstraints.BOTH;
		gbc_kategorienpanel.gridx = 0;
		gbc_kategorienpanel.gridy = 2;
		profilpanel.add(kategorienpanel, gbc_kategorienpanel);
		GridBagLayout gbl_kategorienpanel = new GridBagLayout();
		gbl_kategorienpanel.columnWidths = new int[]{63, 0};
		gbl_kategorienpanel.rowHeights = new int[]{23, 0, 0};
		gbl_kategorienpanel.columnWeights = new double[]{0.0, Double.MIN_VALUE};
		gbl_kategorienpanel.rowWeights = new double[]{0.0, 0.0, Double.MIN_VALUE};
		kategorienpanel.setLayout(gbl_kategorienpanel);
		
		JCheckBox chckbxMuseen = new JCheckBox("Museen");
		GridBagConstraints gbc_chckbxMuseen = new GridBagConstraints();
		gbc_chckbxMuseen.fill = GridBagConstraints.HORIZONTAL;
		gbc_chckbxMuseen.anchor = GridBagConstraints.NORTH;
		gbc_chckbxMuseen.insets = new Insets(0, 0, 5, 0);
		gbc_chckbxMuseen.gridx = 0;
		gbc_chckbxMuseen.gridy = 0;
		kategorienpanel.add(chckbxMuseen, gbc_chckbxMuseen);
		
		JCheckBox chckbxSchwimmen = new JCheckBox("Schwimmen");
		GridBagConstraints gbc_chckbxSchwimmen = new GridBagConstraints();
		gbc_chckbxSchwimmen.anchor = GridBagConstraints.NORTHWEST;
		gbc_chckbxSchwimmen.gridx = 0;
		gbc_chckbxSchwimmen.gridy = 1;
		kategorienpanel.add(chckbxSchwimmen, gbc_chckbxSchwimmen);
	}

}
