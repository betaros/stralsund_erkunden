package gui;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collections;

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
	public JPanel planList;
	private JPanel mappanel;
	private JXMapViewer mapViewer;

	private PrologConnector prologConnector;
	public Profile profile;

	private ArrayList<Event> resultArrayList;
	private ArrayList<Event> timelineArrayList;

	/**
	 * Create the panel.
	 */
	public MainPanel() {
		prologConnector = new PrologConnector();
		resultArrayList = new ArrayList<Event>();
		timelineArrayList = new ArrayList<Event>();
		
		setBorder(null);
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
		resultpanel.setBorder(new TitledBorder(UIManager.getBorder("TitledBorder.border"), "M\u00F6gliche Events", TitledBorder.LEADING, TitledBorder.TOP, null, new Color(0, 0, 0)));
		splitPane_1.setLeftComponent(resultpanel);
		GridBagLayout gbl_resultpanel = new GridBagLayout();
		gbl_resultpanel.columnWidths = new int[] {300, 0};
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
		splitPane_1.setRightComponent(splitPane);

		JPanel routingpanel = new JPanel();
		routingpanel.setBorder(new TitledBorder(null, "Zeitablauf", TitledBorder.LEADING, TitledBorder.TOP, null, null));
		splitPane.setLeftComponent(routingpanel);
		GridBagLayout gbl_routingpanel = new GridBagLayout();
		gbl_routingpanel.columnWidths = new int[] {300, 0};
		gbl_routingpanel.rowHeights = new int[]{2, 0};
		gbl_routingpanel.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gbl_routingpanel.rowWeights = new double[]{1.0, Double.MIN_VALUE};
		routingpanel.setLayout(gbl_routingpanel);

		planList = new JPanel();
		GridBagConstraints gbc_planlist = new GridBagConstraints();
		gbc_planlist.fill = GridBagConstraints.VERTICAL;
		gbc_planlist.gridwidth = GridBagConstraints.REMAINDER;
		gbc_planlist.weightx = 1;
		gbc_planlist.weighty = 1;
		planList.add(new JPanel(), gbc_planlist);

		GridBagLayout gbl_planList = new GridBagLayout();
		gbl_planList.columnWidths = new int[]{144, 0};
		gbl_planList.rowHeights = new int[]{10, 0};
		gbl_planList.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gbl_planList.rowWeights = new double[]{1.0, Double.MIN_VALUE};
		planList.setLayout(gbl_planList);

		JScrollPane planListScrollPane = new JScrollPane(planList);
		GridBagConstraints gbc_planListScrollPane = new GridBagConstraints();
		gbc_planListScrollPane.fill = GridBagConstraints.BOTH;
		gbc_planListScrollPane.gridx = 0;
		gbc_planListScrollPane.gridy = 0;
		routingpanel.add(planListScrollPane, gbc_planListScrollPane);

		mappanel = new JPanel();
		splitPane.setRightComponent(mappanel);
		GridBagLayout gbl_mappanel = new GridBagLayout();
		gbl_mappanel.columnWidths = new int[] {400, 0};
		gbl_mappanel.rowHeights = new int[]{118, 0};
		gbl_mappanel.columnWeights = new double[]{1.0, Double.MIN_VALUE};
		gbl_mappanel.rowWeights = new double[]{1.0, Double.MIN_VALUE};
		mappanel.setLayout(gbl_mappanel);
	}

	/**
	 * Zeige Eventliste an
	 */
	public boolean showSearchResults(Profile _p){
		profile = _p;
		ArrayList<String> categoryList = profile.getSelectedCategories();
		ArrayList<String> hotelList = profile.getSelectedHotel();
		ArrayList<String> foodList = profile.getSelectedFood();
		if(categoryList.isEmpty() || hotelList.isEmpty() || foodList.isEmpty()){
			return false;
		}

		ArrayList<String> eventStringList = prologConnector.getEventsByPrologWithCategories(categoryList);
		ArrayList<String> hotelStringList = prologConnector.findHotels(hotelList);

		resultArrayList.removeAll(resultArrayList);
		for(String s:eventStringList){
			resultArrayList.add(prologConnector.findEvent(s, false));
		}
		for(String s:hotelStringList){
			resultArrayList.add(prologConnector.findEvent(s, true));
		}

		resultArrayList.removeAll(Collections.singleton(null));
		
		mainList.removeAll();
		
		Map map = new Map();
		ArrayList<GeoPosition> waypoints = new ArrayList<GeoPosition>();
		int eventCounter = 1;

		for(Event e:resultArrayList){
			waypoints.add(new GeoPosition(e.getLatitude(), e.getLongitude()));

			Result result = new Result(eventCounter, e, profile, true);
			GridBagConstraints gbc = new GridBagConstraints();
			gbc.gridwidth = GridBagConstraints.REMAINDER;
			gbc.weightx = 1;
			gbc.fill = GridBagConstraints.HORIZONTAL;
			mainList.add(result, gbc, 0);
			
			eventCounter++;

			result.btnNewButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent arg0) {
					addToTimeplan(result.getEvent());
					int totalCost = 0;
					for(Event e:timelineArrayList){
						totalCost += e.getPriceInCentAdult() + e.getPriceInCentAdult();
					}
					profile.setTotalCost(totalCost);
					validate();
					repaint();
				}
			});

			result.btnRemoveButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent arg0) {
					mainList.remove(result);
					int totalCost = 0;
					for(Event e:timelineArrayList){
						totalCost += e.getPriceInCentAdult() + e.getPriceInCentAdult();
					}
					profile.setTotalCost(totalCost);
					validate();
					repaint();
				}
			});

			mapViewer = map.getMap(waypoints);
			mapViewer.setLayout(new FlowLayout(FlowLayout.CENTER, 5, 5));

			GridBagConstraints gbc_mapViewer = new GridBagConstraints();
			gbc_mapViewer.fill = GridBagConstraints.BOTH;
			gbc_mapViewer.gridx = 0;
			gbc_mapViewer.gridy = 0;
			mappanel.removeAll();
			mappanel.add(mapViewer, gbc_mapViewer);
		}

		mapViewer = map.getMap(waypoints);
		validate();
		repaint();

		return true;
	}

	/**
	 * Generiere Timeline
	 */
	public void fillTimeline(){
		String hotel = "Pension Am Ozeaneum";
		for(Event e:resultArrayList){
			if(e.getCategories().contains("0") ||
					e.getCategories().contains("1") ||
					e.getCategories().contains("2") ||
					e.getCategories().contains("3") ||
					e.getCategories().contains("4")){
				hotel = e.getName();
				break;
			}
		}
		ArrayList<Event> resultTimeLine = prologConnector.fillTimeLine(hotel, profile);
		for(Event e:resultTimeLine){
			addToTimeplan(e);
		}
	}
	
	/**
	 * Fuege Event zur Timeline hinzu
	 * @param _newEvent
	 */
	public void addToTimeplan(Event _newEvent){
		//String hotel = "";
		//boolean isEventOnTime = prologConnector.checkEventsOnTime(profile.getAdultCounter(), profile.getChildCounter(), timelineArrayList, profile.getDayStart(), profile.getDayEnd(), hotel, hotel, profile.getBudgetInCent(), profile); 
		//System.out.println(isEventOnTime);
		
		//if(isEventOnTime){
			if(!timelineArrayList.contains(_newEvent)){
				if(!timelineArrayList.isEmpty()){
					_newEvent.setTraveltime(
							prologConnector.calcApproachForEvent(
									profile.getAdultCounter(), 
									profile.getChildCounter(), 
									timelineArrayList.get(timelineArrayList.size()-1).getName(), 
									_newEvent.getName(),
									profile.getArrival(), 
									_newEvent.getStartTime()));
				}
				timelineArrayList.add(_newEvent);
			}

			planList.removeAll();

			for(Event e:timelineArrayList){			
				Result result = new Result(0, e, profile, false);
				GridBagConstraints gbc = new GridBagConstraints();
				gbc.gridwidth = GridBagConstraints.REMAINDER;
				gbc.weightx = 1;
				gbc.fill = GridBagConstraints.HORIZONTAL;
				planList.add(result, gbc, 0);

				result.btnRemoveButton.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent arg0) {
						planList.remove(result);
						timelineArrayList.remove(result.getEvent());
						validate();
						repaint();
					}
				});
			}
			validate();
			repaint();
		//}
	}
}
