package map;

import java.awt.Color;
import java.io.File;
import java.util.ArrayList;
//import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import javax.swing.event.MouseInputListener;

import org.jxmapviewer.JXMapViewer;
import org.jxmapviewer.VirtualEarthTileFactoryInfo;
import org.jxmapviewer.input.CenterMapListener;
import org.jxmapviewer.input.PanKeyListener;
import org.jxmapviewer.input.PanMouseInputListener;
import org.jxmapviewer.input.ZoomMouseWheelListenerCenter;
import org.jxmapviewer.viewer.DefaultTileFactory;
import org.jxmapviewer.viewer.GeoPosition;
import org.jxmapviewer.viewer.LocalResponseCache;
import org.jxmapviewer.viewer.TileFactoryInfo;
import org.jxmapviewer.viewer.WaypointPainter;

/**
 * A simple sample application that shows
 * a OSM map of Europe
 * @author Martin Steiger
 */
public class Map
{
	/**
	 * @param args the program args (ignored)
	 */
	//public static void main(String[] args)
	public JXMapViewer getMap(ArrayList<GeoPosition> _waypoints)
	{
		// Create a TileFactoryInfo for Virtual Earth
		TileFactoryInfo info = new VirtualEarthTileFactoryInfo(VirtualEarthTileFactoryInfo.MAP);
		DefaultTileFactory tileFactory = new DefaultTileFactory(info);
		tileFactory.setThreadPoolSize(8);

		// Setup local file cache
		File cacheDir = new File(System.getProperty("user.home") + File.separator + ".jxmapviewer2");
		LocalResponseCache.installResponseCache(info.getBaseURL(), cacheDir, false);

		// Setup JXMapViewer
		JXMapViewer mapViewer = new JXMapViewer();
		mapViewer.setTileFactory(tileFactory);

		GeoPosition stralsund	= new GeoPosition(54.315645, 13.0906511);

		// Set the focus
		mapViewer.setZoom(6);
		mapViewer.setAddressLocation(stralsund);
	
		// Add interactions
		MouseInputListener mia = new PanMouseInputListener(mapViewer);
		mapViewer.addMouseListener(mia);
		mapViewer.addMouseMotionListener(mia);
		mapViewer.addMouseListener(new CenterMapListener(mapViewer));
		mapViewer.addMouseWheelListener(new ZoomMouseWheelListenerCenter(mapViewer));
		mapViewer.addKeyListener(new PanKeyListener(mapViewer));

		// Create waypoints from the geo-positions
		//Set<MyWaypoint> waypoints = new HashSet<MyWaypoint>(Arrays.asList(
		//		new MyWaypoint("1", Color.ORANGE, hansedom),
		//		new MyWaypoint("2", Color.RED, citti),
		//		new MyWaypoint("3", Color.GREEN, ozeaneum)));
		
		ArrayList<MyWaypoint> waypointArray = new ArrayList<MyWaypoint>();
		int counter = 1;
		for(GeoPosition gp:_waypoints) {
			waypointArray.add(new MyWaypoint(String.valueOf(counter), Color.ORANGE, gp));
			counter++;
		}
		Set<MyWaypoint> waypoints = new HashSet<MyWaypoint>(waypointArray);

		// Create a waypoint painter that takes all the waypoints
		WaypointPainter<MyWaypoint> waypointPainter = new WaypointPainter<MyWaypoint>();
		waypointPainter.setWaypoints(waypoints);
		waypointPainter.setRenderer(new FancyWaypointRenderer());
		
		mapViewer.setOverlayPainter(waypointPainter);

		return(mapViewer);
	}
}
