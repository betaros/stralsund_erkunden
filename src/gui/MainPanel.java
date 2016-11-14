package gui;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.util.ArrayList;

import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.border.TitledBorder;

import org.jxmapviewer.JXMapViewer;
import org.jxmapviewer.viewer.GeoPosition;

import map.Map;
import prolog.PrologConnector;
import src.Event;
import src.Profile;

import javax.swing.UIManager;
import java.awt.Color;
import javax.swing.JScrollPane;

public class MainPanel extends JPanel {

	private static final long serialVersionUID = -8438576029794021570L;
	
	private JPanel mainList;
	private JPanel planList;
	private JXMapViewer mapViewer;
	private ArrayList<GeoPosition> waypoints;
	
	private PrologConnector prologConnector;

	/**
	 * Create the panel.
	 */
	public MainPanel() {
		prologConnector = new PrologConnector();
		
		setBorder(new TitledBorder(UIManager.getBorder("TitledBorder.border"), "Ergebnisse", TitledBorder.LEADING, TitledBorder.TOP, null, new Color(0, 0, 0)));
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
		gbl_panel.columnWidths = new int[]{0, 0};
		gbl_panel.rowHeights = new int[]{0, 0};
		gbl_panel.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gbl_panel.rowWeights = new double[]{1.0, Double.MIN_VALUE};
		panel.setLayout(gbl_panel);
		
		JSplitPane splitPane_1 = new JSplitPane();
		GridBagConstraints gbc_splitPane_1 = new GridBagConstraints();
		gbc_splitPane_1.fill = GridBagConstraints.BOTH;
		gbc_splitPane_1.gridx = 0;
		gbc_splitPane_1.gridy = 0;
		panel.add(splitPane_1, gbc_splitPane_1);
		
		JPanel resultpanel = new JPanel();
		splitPane_1.setLeftComponent(resultpanel);
		GridBagLayout gbl_resultpanel = new GridBagLayout();
		gbl_resultpanel.columnWidths = new int[] {350, 0};
		gbl_resultpanel.rowHeights = new int[]{2, 0};
		gbl_resultpanel.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gbl_resultpanel.rowWeights = new double[]{1.0, Double.MIN_VALUE};
		resultpanel.setLayout(gbl_resultpanel);
		
		GridBagLayout gbl_mainList = new GridBagLayout();
		gbl_mainList.columnWeights = new double[]{1.0};
		gbl_mainList.rowWeights = new double[]{1.0};
		
		mainList = new JPanel(gbl_mainList);
        GridBagConstraints gbc_mainlist = new GridBagConstraints();
        gbc_mainlist.fill = GridBagConstraints.VERTICAL;
        gbc_mainlist.gridwidth = GridBagConstraints.REMAINDER;
        gbc_mainlist.weightx = 1;
        gbc_mainlist.weighty = 1;
        mainList.add(new JPanel(), gbc_mainlist);
		
		JScrollPane resultScrollPane = new JScrollPane(mainList);
		GridBagConstraints gbc_scrollPane = new GridBagConstraints();
		gbc_scrollPane.fill = GridBagConstraints.BOTH;
		gbc_scrollPane.gridx = 0;
		gbc_scrollPane.gridy = 0;
		resultpanel.add(resultScrollPane, gbc_scrollPane);
		
		JSplitPane splitPane = new JSplitPane();
		splitPane.setOrientation(JSplitPane.VERTICAL_SPLIT);
		splitPane_1.setRightComponent(splitPane);
		
		JPanel routingpanel = new JPanel();
		splitPane.setLeftComponent(routingpanel);
		GridBagLayout gbl_routingpanel = new GridBagLayout();
		gbl_routingpanel.columnWidths = new int[]{2, 0};
		gbl_routingpanel.rowHeights = new int[]{2, 0};
		gbl_routingpanel.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gbl_routingpanel.rowWeights = new double[]{1.0, Double.MIN_VALUE};
		routingpanel.setLayout(gbl_routingpanel);
		
		GridBagLayout gbl_planList = new GridBagLayout();
		gbl_planList.rowWeights = new double[]{1.0};
		
		planList = new JPanel();
		GridBagConstraints gbc_planlist = new GridBagConstraints();
        gbc_planlist.fill = GridBagConstraints.VERTICAL;
        gbc_planlist.gridwidth = GridBagConstraints.REMAINDER;
        gbc_planlist.weightx = 1;
        gbc_planlist.weighty = 1;
        planList.add(new JPanel(), gbc_planlist);
		
		JScrollPane planListScrollPane = new JScrollPane(planList);
		GridBagConstraints gbc_planListScrollPane = new GridBagConstraints();
		gbc_planListScrollPane.fill = GridBagConstraints.HORIZONTAL;
		gbc_planListScrollPane.anchor = GridBagConstraints.NORTH;
		gbc_planListScrollPane.gridx = 0;
		gbc_planListScrollPane.gridy = 0;
		routingpanel.add(planListScrollPane, gbc_planListScrollPane);
		
		JPanel mappanel = new JPanel();
		splitPane.setRightComponent(mappanel);
		GridBagLayout gbl_mappanel = new GridBagLayout();
		gbl_mappanel.columnWidths = new int[] {400, 0};
		gbl_mappanel.rowHeights = new int[]{118, 0};
		gbl_mappanel.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gbl_mappanel.rowWeights = new double[]{1.0, Double.MIN_VALUE};
		mappanel.setLayout(gbl_mappanel);
		
		Map map = new Map();
		waypoints = new ArrayList<GeoPosition>();
		waypoints.add(new GeoPosition(54.3199026, 13.0416835));
		waypoints.add(new GeoPosition(54.3200465, 13.0446653));
		waypoints.add(new GeoPosition(54.315509,13.0949513));
		waypoints.add(new GeoPosition(54.311055, 13.090076));
		mapViewer = map.getMap(waypoints);
		
		mapViewer.setLayout(new FlowLayout(FlowLayout.CENTER, 5, 5));
		
		GridBagConstraints gbc_mapViewer = new GridBagConstraints();
		gbc_mapViewer.fill = GridBagConstraints.BOTH;
		gbc_mapViewer.gridx = 0;
		gbc_mapViewer.gridy = 0;
		mappanel.add(mapViewer, gbc_mapViewer);
	}
	
	public void showSearchResults(){
		ArrayList<String> categories = new ArrayList<String>();
		categories.add("Shopping");
		categories.add("Schwimmen");
    	Event event = new Event("Hansedom", 52.1, 19.1, 2.5, 3.5, categories);
		Profile profile = new Profile(2, 20000, 2, 1);
		
		ArrayList<String> eventNames = prologConnector.getEventsByPrologWithCategories(profile.getCategories());
		ArrayList<Result> results = new ArrayList<Result>();
		for(String eventName:eventNames){
			//results.add(new Result(prologConnector.findEvent(eventName),profile));
		}
		
		Result r = new Result(event, profile);
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainList.add(r, gbc, 0);
        
        validate();
        repaint();
	}

	public void showTimePlan(){
		ArrayList<String> categories = new ArrayList<String>();
		categories.add("Shopping");
		categories.add("Schwimmen");
    	Event event = new Event("Hansedom", 52.1, 19.1, 2.5, 3.5, categories);
		Profile profile = new Profile(2, 20000, 2, 1);
		
		Result r = new Result(event, profile);
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        planList.add(r, gbc, 0);

        validate();
        repaint();
	}
	
}
