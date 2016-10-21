package src;

import java.util.ArrayList;

import org.jxmapviewer.viewer.GeoPosition;

public class Event {
	private String name;
	private double latitude;
	private double longitude;
	private ArrayList<String> categories;
	private GeoPosition geoPos;
	
	public Event(String _name, double _lat, double _lon, ArrayList<String> _cat){
		this.name = _name;
		this.latitude = _lat;
		this.longitude = _lon;
		this.categories = _cat;
		
		this.geoPos = new GeoPosition(_lat, _lon);
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @return the latitude
	 */
	public double getLatitude() {
		return latitude;
	}

	/**
	 * @return the longitude
	 */
	public double getLongitude() {
		return longitude;
	}

	/**
	 * @return the categories
	 */
	public ArrayList<String> getCategories() {
		return categories;
	}

	/**
	 * @return the geoPos
	 */
	public GeoPosition getGeoPos() {
		return geoPos;
	}
}
