package src;

import java.util.ArrayList;

import prolog.PrologConnector;

public class Profile {
	
	private int days;
	private int budgetInCent;
	private int adultCounter;
	private int childCounter;
	
	private int dayStart;
	private int dayEnd;
	
	private String arrival;
	
	private ArrayList<String> categories;
	private ArrayList<String> selectedCategories;
	
	private ArrayList<String> food;
	private ArrayList<String> selectedFood;
	
	private ArrayList<String> hotel;
	private ArrayList<String> selectedHotel;
	
	private PrologConnector pc;
	
	public Profile(int days, int budgetInCent, int adultCounter, int childCounter) {
		super();
		
		pc = new PrologConnector();
		
		this.days = days;
		this.budgetInCent = budgetInCent;
		this.adultCounter = adultCounter;
		this.childCounter = childCounter;
		
		this.dayStart = 480;
		this.dayEnd = 1320;
		
		this.categories = pc.getCategoriesByProlog();
		this.selectedCategories = new ArrayList<String>();
		
		this.food = pc.getFoodByProlog();
		this.selectedFood = new ArrayList<String>();
		
		this.hotel = pc.getHotelByProlog();
		this.selectedHotel = new ArrayList<String>();
		
		this.arrival = "Auto";
	}
	
	/**
	 * @return the days
	 */
	public int getDays() {
		return days;
	}
	/**
	 * @param days the days to set
	 */
	public void setDays(int days) {
		this.days = days;
	}
	/**
	 * @return the budgetInCent
	 */
	public int getBudgetInCent() {
		return budgetInCent;
	}
	/**
	 * @param budgetInCent the budgetInCent to set
	 */
	public void setBudgetInCent(int budgetInCent) {
		this.budgetInCent = budgetInCent;
	}
	/**
	 * @return the adultCounter
	 */
	public int getAdultCounter() {
		return adultCounter;
	}
	/**
	 * @param adultCounter the adultCounter to set
	 */
	public void setAdultCounter(int adultCounter) {
		this.adultCounter = adultCounter;
	}
	/**
	 * @return the childCounter
	 */
	public int getChildCounter() {
		return childCounter;
	}
	/**
	 * @param childCounter the childCounter to set
	 */
	public void setChildCounter(int childCounter) {
		this.childCounter = childCounter;
	}
	/**
	 * @return the categories
	 */
	public ArrayList<String> getCategories() {
		return categories;
	}
	
	/**
	 * @return the selectedCategories
	 */
	public ArrayList<String> getSelectedCategories() {
		return selectedCategories;
	}

	/**
	 * @param selectedCategories the selectedCategories to set
	 */
	public void setSelectedCategories(ArrayList<String> selectedCategories) {
		this.selectedCategories = selectedCategories;
	}
	
	/**
	 * @return the selectedFood
	 */
	public ArrayList<String> getSelectedFood() {
		return selectedFood;
	}

	/**
	 * @param selectedFood the selectedFood to set
	 */
	public void setSelectedFood(ArrayList<String> selectedFood) {
		this.selectedFood = selectedFood;
	}

	/**
	 * @return the food
	 */
	public ArrayList<String> getFood() {
		return food;
	}
	
	/**
	 * @return the selectedHotel
	 */
	public ArrayList<String> getSelectedHotel() {
		return selectedHotel;
	}

	/**
	 * @param selectedHotel the selectedHotel to set
	 */
	public void setSelectedHotel(ArrayList<String> selectedHotel) {
		this.selectedHotel = selectedHotel;
	}

	/**
	 * @return the hotel
	 */
	public ArrayList<String> getHotel() {
		return hotel;
	}

	/**
	 * @return the dayStart
	 */
	public int getDayStart() {
		return dayStart;
	}

	/**
	 * @param dayStart the dayStart to set
	 */
	public void setDayStart(int dayStart) {
		this.dayStart = dayStart;
	}

	/**
	 * @return the dayEnd
	 */
	public int getDayEnd() {
		return dayEnd;
	}

	/**
	 * @param dayEnd the dayEnd to set
	 */
	public void setDayEnd(int dayEnd) {
		this.dayEnd = dayEnd;
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
