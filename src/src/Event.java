package src;

import java.util.ArrayList;

import org.jxmapviewer.viewer.GeoPosition;

public class Event {
	private String name;
	private double latitude;
	private double longitude;
	private ArrayList<String> categories;
	private GeoPosition geoPos;
	
	// Unsicher
	private int open = 900;
	private int closed = 2200;
	private int priceInCentAdult = 2000;
	private int priceInCentChild = 2000;

	/**
	 * 
	 * @param _name
	 * @param _lat
	 * @param _lon
	 * @param _priceAdult
	 * @param _priceChild
	 * @param _cat
	 */
	public Event(String _name, double _lat, double _lon, double _priceAdult, double _priceChild, ArrayList<String> _cat){
		this.name = _name;
		this.latitude = _lat;
		this.longitude = _lon;
		this.priceInCentChild = (int)_priceChild * 100;
		this.priceInCentAdult = (int)_priceAdult * 100;
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

	// Unsicher
	/**
	 * @return the open
	 */
	public int getOpen() {
		return open;
	}

	/**
	 * @param open the open to set
	 */
	public void setOpen(int open) {
		this.open = open;
	}

	/**
	 * @return the closed
	 */
	public int getClosed() {
		return closed;
	}

	/**
	 * @param closed the closed to set
	 */
	public void setClosed(int closed) {
		this.closed = closed;
	}

	/**
	 * @return the priceInCent
	 */
	public int getPriceInCentAdult() {
		return priceInCentAdult;
	}

	/**
	 * @param priceInCent the priceInCent to set
	 */
	public void setPriceInCentAdult(int priceInCent) {
		this.priceInCentAdult = priceInCent;
	}
	
	/**
	 * @return the priceInCentChild
	 */
	public int getPriceInCentChild() {
		return priceInCentChild;
	}

	/**
	 * @param priceInCentChild the priceInCentChild to set
	 */
	public void setPriceInCentChild(int priceInCent) {
		this.priceInCentChild = priceInCent;
	}
}
