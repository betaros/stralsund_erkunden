package src;

public class Businesshours {
	private String weekday;
	private int open;
	private int closed;
	
	public Businesshours(String weekday, int open, int closed) {
		super();
		this.weekday = weekday;
		this.open = open;
		this.closed = closed;
	}
	
	/**
	 * @return the weekday
	 */
	public String getWeekday() {
		return weekday;
	}
	
	/**
	 * @param weekday the weekday to set
	 */
	public void setWeekday(String weekday) {
		this.weekday = weekday;
	}
	
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
	
	
	
}
