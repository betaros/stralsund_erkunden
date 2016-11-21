// https://github.com/SWI-Prolog/packages-jpl/tree/master/examples/java

package prolog;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Map;

import org.jpl7.*;

import src.Event;
import src.Profile;

public class PrologConnector {

	boolean debug = false;
	
	/**
	 * Konstruktor
	 */
	public PrologConnector(){
		Query.hasSolution("consult('src/prolog/functions.pl')");
	}

	/**
	 * Finde Event
	 * @return
	 */
	public Event findEvent(String title){
		Atom Name = new Atom(title);
		Variable Position = new Variable("Position");
		Variable Categories = new Variable("Categories");
		Variable Food = new Variable("Food");
		Variable Price = new Variable("Price");
		Variable Businesshours = new Variable("Businesshours");

		Query query =
				new Query(
						"event",
						new Term[] {Name, Position, Categories, Food, Price, Businesshours}
						);

		Event e = null;
		
		if(query.hasSolution()){
			Map<String, Term> solution = query.oneSolution();
			String arrayPosition[]     = solution.get("Position").toString().split(",");
			String arrayCategories[]   = solution.get("Categories").toString().split(",");
			String arrayFood[]         = solution.get("Food").toString().split(",");
			String arrayPrice[]        = solution.get("Price").toString().split(",");
			String arrayBusinesshours[]        = solution.get("Businesshours").toString().split(",");

			for(int i = 0; i<arrayPosition.length; i++){
				arrayPosition[i] = arrayPosition[i].replaceAll("[^A-Za-z0-9.]", "");
				if(debug){
					System.out.println("findEvent: Position:   " + arrayPosition[i]);
				}
			}
			
			for(int i = 0; i<arrayCategories.length; i++){
				arrayCategories[i] = arrayCategories[i].replaceAll("[^A-Za-z0-9.]", "");
				if(debug){
					System.out.println("findEvent: Categories: " + arrayCategories[i]);
				}
			}
			
			for(int i = 0; i<arrayFood.length; i++){
				arrayFood[i] = arrayFood[i].replaceAll("[^A-Za-z0-9.]", "");
				if(debug){
					System.out.println("findEvent: Food:       " + arrayFood[i]);
				}
			}
			
			for(int i = 0; i<arrayPrice.length; i++){
				arrayPrice[i] = arrayPrice[i].replaceAll("[^A-Za-z0-9.]", "");
				if(debug){
					System.out.println("findEvent: Price:      " + arrayPrice[i]);
				}
			}
			
			for(int i = 0; i<arrayBusinesshours.length; i++){
				arrayBusinesshours[i] = arrayBusinesshours[i].replaceAll("[^A-Za-z0-9.]", "");
				if(debug){
					System.out.println("findEvent: Businesshours: " + arrayBusinesshours[i]);
				}
			}
			
			arrayPosition = Arrays.copyOf(arrayPosition, arrayPosition.length-1);
			arrayCategories = Arrays.copyOf(arrayCategories, arrayCategories.length-1);
			arrayFood = Arrays.copyOf(arrayFood, arrayFood.length-1);
			arrayPrice = Arrays.copyOf(arrayPrice, arrayPrice.length-1);
			arrayBusinesshours = Arrays.copyOf(arrayBusinesshours, arrayBusinesshours.length-1);
			
			double lat = Double.parseDouble(arrayPosition[0]);
			double lon = Double.parseDouble(arrayPosition[1]);
			int priceAdult = java.lang.Integer.parseInt(arrayPrice[0]);
			int priceReduced = java.lang.Integer.parseInt(arrayPrice[1]);
			ArrayList<String> categoryList = new ArrayList<String>();
			ArrayList<String> foodList = new ArrayList<String>();

			for(String category:arrayCategories){
				categoryList.add(category);
			}

			for(String food:arrayFood){
				foodList.add(food);
			}
			
			int businessOpen = java.lang.Integer.parseInt(arrayBusinesshours[0]);
			int businessClosed = java.lang.Integer.parseInt(arrayBusinesshours[1]);
			
			e = new Event(title, lat, lon, priceAdult, priceReduced, categoryList, foodList, 1, 0, 0, businessOpen, businessClosed);
		}

		return e;
	}

	/**
	 * Hole passende Hotels zu Hotelkategorien
	 * 
	 * @param hotelCategories
	 * @return
	 */
	public ArrayList<Event> findHotels(ArrayList<String> hotelCategories){
		Term HotelCategories = Util.textToTerm(prologListGenerator(hotelCategories));
		Variable Hotels = new Variable("Hotels");
		
		Query query =
				new Query(
						"searchHotelsOnCategory",
						new Term[] {HotelCategories, Hotels}
						);
		
		ArrayList<Event> hotelList = new ArrayList<Event>();
		ArrayList<String> categories = new ArrayList<String>();
		categories.add("Hotel");
		
		if(query.hasSolution()){
			Map<String, Term> solution = query.oneSolution();
			String[] array = solution.get("Hotels").toString().split(",");
			for(int i = 0; i<array.length-1; i++){
				array[i] = array[i].replaceAll("[^A-Za-z0-9 ]", "");
				array[i] = array[i].trim();
				if(debug){
					System.out.println("findHotels: " + array[i]);
				}
				
				Event e = new Event(array[i], 0, 0, 0, 0, categories, new ArrayList<String>(), 1, 0, 0, 0, 0);
				if (!hotelList.contains(e)) {
					hotelList.add(e);
				}
			}
		}
		
		return hotelList;
	}
	
	/**
	 * Berechnet die Distanz zwischen zwei Orten
	 * @param placeA
	 * @param placeB
	 */
	public void calcDistance(String placeA, String placeB){
		Query query = 
				new Query(
						new Compound(
								"calcDistance",
								new Term[] {new Atom(placeA), new Atom(placeB), new Variable("X")}
								)
						);

		if(query.hasSolution()){
			Map<String, Term> ergebnis = query.oneSolution();
			System.out.println("Entfernung zwischen " + placeA + " und " + placeB + ": " + ergebnis.get("X"));
		}
	}

	/**
	 * Gibt passende Events zu den Kategorien aus
	 * @param categories
	 * @return
	 */
	public ArrayList<String> getEventsByPrologWithCategories(ArrayList<String> categories){
		ArrayList<String> events = new ArrayList<String>();

		Variable X = new Variable("X");
		Term termCategories = Util.textToTerm(prologListGenerator(categories));

		if(debug){
			System.out.println("Debug: " + termCategories.toString());
		}

		Query query =
				new Query(
						"searchEventsOnCategory",
						new Term[] {termCategories,X}
						);

		if(query.hasSolution()){
			Map<String, Term> solution = query.oneSolution();
			String[] array = solution.get("X").toString().split(",");

			for(int i = 0; i<array.length-1; i++){
				array[i] = array[i].replaceAll("[^A-Za-z0-9 ]", "");
				array[i] = array[i].trim();
				if(debug){
					System.out.println("getEventsByPrologWithCategories: " + array[i]);
				}
				if (!events.contains(array[i])) {
					events.add(array[i]);
				}
			}
		}

		return events;
	}

	/**
	 * Ueberprueft, ob die Evente im Budget liegen
	 * @param adultCount
	 * @param reducedCount
	 * @param budget
	 * @param eventslist
	 * @return
	 */
	public ArrayList<String> checkEventForBudget(int adultCount, int reducedCount, int budget, ArrayList<String> eventslist){
		ArrayList<String> persons = new ArrayList<String>();
		persons.add(String.valueOf(adultCount));
		persons.add(String.valueOf(reducedCount));
		
		Term termPersons = Util.textToTerm(prologListGenerator(persons));
		Atom Budget = new Atom(String.valueOf(budget));
		Term termEvents = Util.textToTerm(prologListGenerator(eventslist));
		Variable X = new Variable("X");

		Query query =
				new Query(
						"checkEventForBudget",
						new Term[] {termPersons,Budget,termEvents,X}
						);

		ArrayList<String> events = new ArrayList<String>();
		if(query.hasSolution()){
			Map<String, Term> solution = query.oneSolution();
			String[] array = solution.get("X").toString().split(",");

			for(int i = 0; i<array.length-1; i++){
				array[i] = array[i].replaceAll("[^A-Za-z0-9 ]", "");
				System.out.println("getEventsByPrologWithCategories: " + array[i]);
				if (!events.contains(array[i])) {
					events.add(array[i]);
				}
			}
		}

		return events;
	}

	/**
	 * http://stackoverflow.com/questions/16046192/prolog-find-minimum-in-list-error
	 * 
	 * @param adultCount
	 * @param reducedCount
	 * @param budget
	 * @param categories
	 * @return
	 */
	public ArrayList<Event> searchUsefulEvents(int adultCount, int reducedCount, int budget, ArrayList<String> categories){
		ArrayList<Event> eventList = new ArrayList<>();

		ArrayList<String> peopleList = new ArrayList<String>();
		peopleList.add(String.valueOf(adultCount));
		peopleList.add(String.valueOf(reducedCount));
		Term People = Util.textToTerm(prologListGenerator(peopleList));
		Atom Budget = new Atom(String.valueOf(budget));
		Term CategoryList = Util.textToTerm(prologListGenerator(categories));
		Variable X = new Variable();

		Term arg[] = {People, Budget, CategoryList, X};
		
		Query query =
				new Query(
						"searchUsefulEvents",
						arg
						);

		if(query.hasSolution()){
			Map<String, Term> solution = query.oneSolution();
			String[] array = solution.get("X").toString().split(" ");

			for(int i = 0; i<array.length-1; i++){
				array[i] = array[i].replaceAll("[^A-Za-z0-9 ]", "");
				System.out.println("getCategoriesByProlog: " + array[i]);
				if (!eventList.contains(array[i])) {
					//eventList.add(array[i]);
				}
			}
		}

		return eventList;
	}
	
	/**
	 * Gibt eine Liste aller Kategorien aus
	 * @return
	 */
	public ArrayList<String> getCategoriesByProlog(){
		ArrayList<String> categories = new ArrayList<String>();
		Variable X = new Variable("X");

		Query query =
				new Query(
						"findAllCategories",
						new Term[] {X}
						);

		if(query.hasSolution()){
			Map<String, Term> solution = query.oneSolution();
			String[] array = solution.get("X").toString().split(" ");

			for(int i = 0; i<array.length-1; i++){
				array[i] = array[i].replaceAll("[^A-Za-z0-9 ]", "");
				if (!categories.contains(array[i])) {
					categories.add(array[i]);
					if(debug){
						System.out.println("getCategoriesByProlog: " + array[i]);
					}
				}
			}
		}

		return categories;
	}

	/**
	 * Gibt eine Liste aller Essenskategorien aus
	 * @return
	 */
	public ArrayList<String> getFoodByProlog(){
		ArrayList<String> foodCategories = new ArrayList<String>();
		Variable X = new Variable("X");

		Query query =
				new Query(
						"findAllFoodCategories",
						new Term[] {X}
						);

		if(query.hasSolution()){
			Map<String, Term> solution = query.oneSolution();
			String[] array = solution.get("X").toString().split(" ");

			for(int i = 0; i<array.length-1; i++){
				array[i] = array[i].replaceAll("[^A-Za-z0-9 ]", "");
				if (!foodCategories.contains(array[i])) {
					foodCategories.add(array[i]);
					if(debug){
						System.out.println("getFoodByProlog: " + array[i]);
					}
				}
			}
		}

		return foodCategories;
	}
	
	/**
	 * Gibt eine Liste aller Hotelkategorien aus
	 * @return
	 */
	public ArrayList<String> getHotelByProlog(){
		ArrayList<String> hotelCategories = new ArrayList<String>();
		Variable X = new Variable("X");

		Query query =
				new Query(
						"findAllHotelCategories",
						new Term[] {X}
						);

		if(query.hasSolution()){
			Map<String, Term> solution = query.oneSolution();
			String[] array = solution.get("X").toString().split(" ");

			for(int i = 0; i<array.length-1; i++){
				array[i] = array[i].replaceAll("[^A-Za-z0-9 ]", "");
				if (!hotelCategories.contains(array[i])) {
					hotelCategories.add(array[i]);
					if(debug){
						System.out.println("getHotelByProlog: " + array[i]);
					}
				}
			}
		}

		return hotelCategories;
	}
	
	/**
	 * Ueberprueft, ob ein Event zeitlich passt (einzelnes Event)
	 * 
	 * @param adultCount
	 * @param reducedCount
	 * @param eventList
	 * @param dayStart
	 * @param hotel
	 * @param budget
	 * @param returnValue
	 * @param price
	 * @return
	 */
	public boolean checkEventsOnTime(int adultCount, int reducedCount, ArrayList<Event> eventList, int dayStart, String hotel, int budget, Profile profile){
		ArrayList<String> peopleList = new ArrayList<String>();
		peopleList.add(String.valueOf(adultCount));
		peopleList.add(String.valueOf(reducedCount));
		Term People = Util.textToTerm(prologListGenerator(peopleList));
		
		ArrayList<Event> sortedEventList = sortEventlist(eventList, profile.getDays());
		
		ArrayList<String> eventStringList = new ArrayList<String>();
		for(Event event:sortedEventList){
			StringBuilder eventString = new StringBuilder();
			eventString.append("[");
			eventString.append(event.getName());
			eventString.append(",");
			eventString.append(String.valueOf(event.getDay()));
			eventString.append(",");
			eventString.append(String.valueOf(event.getStartTime()));
			eventString.append(",");
			eventString.append(String.valueOf(event.getDuration()));
			eventString.append(",");
			
			String arrival = profile.getArrival(); 
			switch(arrival){
			case "Zu fuss":
				eventString.append("Foot");
				break;
			case "Fahrrad":
				eventString.append("Bike");
				break;
			case "Bus":
				eventString.append("Bus");
				break;
			default:
				eventString.append("Car");
			}
			eventString.append("]");
			
			eventStringList.add(eventString.toString());
		}
		Term EventList = Util.textToTerm(prologListGenerator(eventStringList));
		Atom DayStart = new Atom(String.valueOf(dayStart));
		Atom Hotel = new Atom(hotel);
		Atom Budget = new Atom(String.valueOf(budget));
		Variable Price = new Variable("Price");
		Variable ReturnValue = new Variable("ReturnValue");
		
		Query query =
				new Query(
					"checkEventsOnTime",
					new Term[]{People, EventList, DayStart, Hotel, Budget, ReturnValue, Price}
				);
		
		boolean result = false;
		
		if(query.hasSolution()){
			Map<String,Term> solution = query.oneSolution();
			String array = solution.get("ReturnValue").toString();
			String price = solution.get("Price").toString();
			if(array.contains("true")){
				result = true;
				int priceInt = java.lang.Integer.parseInt(price);
				profile.setTotalCost(priceInt);
			}
		}
		
		return result;
	}
	
	/**
	 * Ueberprueft, ob ein Event zeitlich passt (zwei Events)
	 * 
	 * @param adultCount
	 * @param reducedCount
	 * @param eventList
	 * @param dayStart
	 * @param hotel
	 * @param budget
	 * @param returnValue
	 * @param price
	 * @return
	 */
	public boolean checkEventsOnTime(int adultCount, int reducedCount, String prevEvent, ArrayList<Event> eventList, int dayStart, String hotel, int budget, Profile profile){
		ArrayList<String> peopleList = new ArrayList<String>();
		peopleList.add(String.valueOf(adultCount));
		peopleList.add(String.valueOf(reducedCount));
		Term People = Util.textToTerm(prologListGenerator(peopleList));
		Atom PrevEvent = new Atom(prevEvent);
		
		ArrayList<Event> sortedEventList = sortEventlist(eventList, profile.getDays());
		
		ArrayList<String> eventStringList = new ArrayList<String>();
		for(Event event:sortedEventList){
			StringBuilder eventString = new StringBuilder();
			eventString.append("[");
			eventString.append(event.getName());
			eventString.append(",");
			eventString.append(String.valueOf(event.getDay()));
			eventString.append(",");
			eventString.append(String.valueOf(event.getStartTime()));
			eventString.append(",");
			eventString.append(String.valueOf(event.getDuration()));
			eventString.append(",");
			
			String arrival = profile.getArrival(); 
			switch(arrival){
			case "Zu Fuss":
				eventString.append("Foot");
				break;
			case "Fahrrad":
				eventString.append("Bike");
				break;
			case "Bus":
				eventString.append("Bus");
				break;
			default:
				eventString.append("Car");
			}
			eventString.append("]");
			
			eventStringList.add(eventString.toString());
		}
		
		Term EventList = Util.textToTerm(prologListGenerator(eventStringList));
		
		Atom DayStart = new Atom(String.valueOf(dayStart));
		Atom Hotel = new Atom(hotel);
		Atom Budget = new Atom(String.valueOf(budget));
		Variable Price = new Variable("Price");
		Variable ReturnValue = new Variable("ReturnValue");
		
		Query query =
				new Query(
					"checkEventsOnTime",
					new Term[]{People, PrevEvent, EventList, DayStart, Hotel, Budget, ReturnValue, Price}
				);
		
		boolean result = false;
		
		if(query.hasSolution()){
			Map<String,Term> solution = query.oneSolution();
			String array = solution.get("ReturnValue").toString();
			String price = solution.get("Price").toString();
			if(array.contains("true")){
				result = true;
				int priceInt = java.lang.Integer.parseInt(price);
				profile.setTotalCost(priceInt);
			}
		}
		
		return result;
	}
	
	/**
	 * Berechnet Anfahrtsdauer
	 * 
	 * @param adultCount
	 * @param reducedCount
	 * @param prevEvent
	 * @param thisEvent
	 * @param hotel
	 * @param vehicle
	 * @param eventTime
	 * @return
	 */
	public int calcApproachForEvent(int adultCount, int reducedCount, String prevEvent, String thisEvent, String hotel, String vehicle, int eventTime){
		ArrayList<String> peopleList = new ArrayList<String>();
		peopleList.add(String.valueOf(adultCount));
		peopleList.add(String.valueOf(reducedCount));
		Term People = Util.textToTerm(prologListGenerator(peopleList));
		Atom PrevEvent = new Atom(prevEvent);
		Atom ThisEvent = new Atom(thisEvent);
		Atom Hotel = new Atom(hotel);
		Atom Vehicle = new Atom(vehicle);
		Atom EventTime = new Atom(String.valueOf(eventTime));
		
		Variable Approach = new Variable("Approach");
		
		Query query =
				new Query(
						"calcApproachForEvent",
						new Term[]{People, PrevEvent, ThisEvent, Hotel, Vehicle, EventTime, Approach}
						);
		
		int result = 0;
		
		if(query.hasSolution()){
			Map<String,Term> solution = query.oneSolution();
			String array = solution.get("Approach").toString();
			array = array.replaceAll("[^0-9.]", "");
			result = java.lang.Integer.parseInt(array);
		}
		
		return result;
	}
	
	/**
	 * Eventliste sortieren
	 * 
	 * @param eventlist
	 * @param daysCounter
	 * @return
	 */
	private ArrayList<Event> sortEventlist(ArrayList<Event> eventlist, int daysCounter) {
		ArrayList<Event> resultEventlist = new ArrayList<Event>();
		
		Event[][] eventArray = new Event[daysCounter][];
		int counter = 0;
		
		// NullPointerException
		for(Event event:eventlist){
			eventArray[event.getDay()-1][counter] = event;
			counter++;
		}
		
		for(int i = 0; i<daysCounter; i++){
			int e = eventArray[i].length;
			ArrayList<java.lang.Integer> times = new ArrayList<java.lang.Integer>();
			for(int j = 0; j<e; j++){
				times.add(eventArray[i][j].getStartTime());
			}
			Collections.sort(times);
			for(int t:times){
				for(int j = 0; j < eventArray[i].length; j++){
					if(eventArray[i][j].getStartTime() == t){
						resultEventlist.add(eventArray[i][j]);
					}
				}
			}
		}
		
		return resultEventlist;
	}
	
	/**
	 * Erstellt eine Prolog Liste aus einer String Arrayliste
	 * @param list
	 * @return
	 */
	private String prologListGenerator(ArrayList<String> list){
		StringBuilder sb = new StringBuilder();
		sb.append("[");
		for(String listelement:list){
			sb.append("'");
			sb.append(listelement);
			sb.append("'");
			sb.append(",");
		}
		sb.deleteCharAt(sb.length()-1);
		sb.append("]");

		return sb.toString();
	}
}
