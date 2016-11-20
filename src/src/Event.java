package src;

import java.util.ArrayList;

import org.jxmapviewer.viewer.GeoPosition;

public class Event {
	private String name;
	private double latitude;
	private double longitude;
	private ArrayList<String> categories;
	private GeoPosition geoPos;
	
	private int priceInCentAdult;
	private int priceInCentChild;
	
	private int startTime;
	private int duration;
	private String arrival;

	/**
	 * 
	 * @param _name
	 * @param _lat
	 * @param _lon
	 * @param _priceAdult
	 * @param _priceChild
	 * @param _cat
	 */
	public Event(String _name, double _lat, double _lon, int _priceAdult, int _priceChild, ArrayList<String> _cat, int _startTime, int _duration, String _arrival){
		this.name = _name;
		this.latitude = _lat;
		this.longitude = _lon;
		this.priceInCentChild = _priceChild * 100;
		this.priceInCentAdult = _priceAdult * 100;
		this.categories = _cat;
		this.startTime = _startTime;
		this.duration = _duration;
		this.arrival = _arrival;
		
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
	
	/**
	 * @return the startTime
	 */
	public int getStartTime() {
		return startTime;
	}

	/**
	 * @param startTime the startTime to set
	 */
	public void setStartTime(int startTime) {
		this.startTime = startTime;
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
	 * @return the arrival
	 */
	public String getArrival() {
		return arrival;
	}

	/**
	 * @param arrival the arrival to set
	 */
	public void setArrival(String arrival) {
		this.arrival = arrival;
	}
}
