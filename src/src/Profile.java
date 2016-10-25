package src;

public class Profile {
	
	private int days;
	private int budgetInCent;
	private int adultCounter;
	private int childCounter;
	private boolean categories[][];
	
	public Profile(int days, int budgetInCent, int adultCounter, int childCounter, boolean[][] categories) {
		super();
		this.days = days;
		this.budgetInCent = budgetInCent;
		this.adultCounter = adultCounter;
		this.childCounter = childCounter;
		this.categories = categories;
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
	public boolean[][] getCategories() {
		return categories;
	}
	/**
	 * @param categories the categories to set
	 */
	public void setCategories(boolean[][] categories) {
		this.categories = categories;
	}
	
	
	
}
