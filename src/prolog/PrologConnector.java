// https://github.com/SWI-Prolog/packages-jpl/tree/master/examples/java

package prolog;

import java.util.ArrayList;
import java.util.Arrays;
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
	public Event findEvent(String title, boolean hotel){
		Atom Name = new Atom(title);
		Variable Position = new Variable("Position");
		Variable Categories = new Variable("Categories");
		Variable Food = new Variable("Food");
		Variable Price = new Variable("Price");
		Variable Businesshours = new Variable("Businesshours");
		Variable Duration = new Variable("Duration");

		Query query;
		
		if(hotel){
			query =
					new Query(
							"hotel",
							new Term[] {Name, Position, Price, Categories}
							);
		} else {
			query =
					new Query(
							"event",
							new Term[] {Name, Position, Categories, Food, Price, Businesshours, Duration}
							);
		}

		Event e = null;
		
		if(query.hasSolution()){
			Map<String, Term> solution = query.oneSolution();
			String arrayPosition[]     = solution.get("Position").toString().split(",");
			String arrayCategories[]   = solution.get("Categories").toString().split(",");
			String arrayFood[]         = solution.get("Food").toString().split(",");
			String arrayPrice[]        = solution.get("Price").toString().split(",");
			String arrayBusinesshours[] = solution.get("Businesshours").toString().split(",");
			String arrayDuration[]     = solution.get("Duration").toString().split(",");

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
			
			for(int i = 0; i<arrayDuration.length; i++){
				arrayDuration[i] = arrayDuration[i].replaceAll("[^A-Za-z0-9.]", "");
				if(debug){
					System.out.println("findEvent: Duration: " + arrayDuration[i]);
				}
			}
			
			arrayPosition = Arrays.copyOf(arrayPosition, arrayPosition.length-1);
			arrayCategories = Arrays.copyOf(arrayCategories, arrayCategories.length-1);
			arrayFood = Arrays.copyOf(arrayFood, arrayFood.length-1);
			arrayPrice = Arrays.copyOf(arrayPrice, arrayPrice.length-1);
			arrayBusinesshours = Arrays.copyOf(arrayBusinesshours, arrayBusinesshours.length-1);
			arrayDuration = Arrays.copyOf(arrayDuration, arrayDuration.length-1);
			
			double lat = Double.parseDouble(arrayPosition[0]);
			double lon = Double.parseDouble(arrayPosition[1]);
			int priceAdult = java.lang.Integer.parseInt(arrayPrice[0]);
			int priceReduced = java.lang.Integer.parseInt(arrayPrice[1]);
			int duration = java.lang.Integer.parseInt(arrayDuration[0]);
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
			
			e = new Event(title, lat, lon, priceAdult, priceReduced, categoryList, foodList, 1, 480, duration, businessOpen, businessClosed);
		}

		return e;
	}
	
	/**
	 * Hole passende Hotels zu Hotelkategorien
	 * 
	 * @param hotelCategories
	 * @return
	 */
	public ArrayList<String> findHotels(ArrayList<String> hotelCategories){
		Term HotelCategories = Util.textToTerm(prologListGenerator(hotelCategories));
		Variable Hotels = new Variable("Hotels");
		
		Query query =
				new Query(
						"searchHotelsOnCategory",
						new Term[] {HotelCategories, Hotels}
						);
		
		ArrayList<String> hotelList = new ArrayList<String>();
		
		if(query.hasSolution()){
			Map<String, Term> solution = query.oneSolution();
			String[] array = solution.get("Hotels").toString().split(",");
			for(int i = 0; i<array.length; i++){
				array[i] = array[i].replaceAll("[^A-Za-z0-9 ]", "");
				array[i] = array[i].trim();
				if(debug){
					System.out.println("findHotels: " + array[i]);
				}
				
				if (!hotelList.contains(array[i])) {
					hotelList.add(array[i]);
				}
			}
		}
		
		return hotelList;
	}
	
	public ArrayList<String> findFood(ArrayList<String> foodCategories){
		ArrayList<String> resultfood = new ArrayList<String>();
		
		
		return resultfood;
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
	public boolean checkEventsOnTime(int adultCount, int reducedCount, ArrayList<Event> eventList, int dayStart, int dayEnd, String hotel, String hotelCategory, int budget, Profile profile){
		ArrayList<String> peopleList = new ArrayList<String>();
		peopleList.add(String.valueOf(adultCount));
		peopleList.add(String.valueOf(reducedCount));
		Term People = Util.textToTerm(prologListGenerator(peopleList));
		
		ArrayList<Term> eventTermList = new ArrayList<Term>();
		if(eventList.isEmpty()){
			return true;
		}
		for(Event event:eventList){
			ArrayList<String> eventStringList = new ArrayList<String>();
			eventStringList.add(event.getName());
			eventStringList.add(String.valueOf(event.getDay()));
			eventStringList.add(String.valueOf(event.getStartTime()));
			eventStringList.add(String.valueOf(event.getDuration()));
			String arrival = profile.getArrival(); 
			switch(arrival){
			case "Zu fuss":
				eventStringList.add("Foot");
				break;
			case "Fahrrad":
				eventStringList.add("Bike");
				break;
			case "Bus":
				eventStringList.add("Bus");
				break;
			default:
				eventStringList.add("Car");
			}
			
			Term EventTerm = Util.textToTerm(prologListGenerator(eventStringList));
			
			eventTermList.add(EventTerm);
		}
		
		// Term mit weiteren Termen fuellen
		Term[] EventList = new Term[eventTermList.size()];
		for(int i = 0; i < eventTermList.size(); i++){
			EventList[i] = eventTermList.get(i);
		}
		Compound EventListCompound = new Compound("EventList", EventList);
		Atom DayStart = new Atom(String.valueOf(dayStart));
		Atom DayEnd = new Atom(String.valueOf(dayEnd));
		Atom Hotel = new Atom(hotel);
		Atom HotelCategory = new Atom(hotelCategory);
		Atom Budget = new Atom(String.valueOf(budget));
		Variable Price = new Variable("Price");
		Variable ReturnValue = new Variable("ReturnValue");
		
		Query query =
				new Query(
					"checkEventsOnTime",
					new Term[]{People, EventListCompound, DayStart, DayEnd, Hotel, HotelCategory, Budget, ReturnValue, Price}
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
	public int calcApproachForEvent(int adultCount, int reducedCount, String prevEvent, String thisEvent, String vehicle, int eventTime){
		ArrayList<String> peopleList = new ArrayList<String>();
		peopleList.add(String.valueOf(adultCount));
		peopleList.add(String.valueOf(reducedCount));
		Term People = Util.textToTerm(prologListGenerator(peopleList));
		Atom PrevEvent = new Atom(prevEvent);
		Atom ThisEvent = new Atom(thisEvent);
		Variable Hotel = new Variable("Hotel");
		Atom Vehicle;
		switch(vehicle){
		case "Zu fuss":
			Vehicle = new Atom("Foot");
			break;
		case "Fahrrad":
			Vehicle = new Atom("Bike");
			break;
		case "Bus":
			Vehicle = new Atom("Bus");
			break;
		default:
			Vehicle = new Atom("Car");
		}
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
			String[] array = solution.get("Approach").toString().split(",");
			result = java.lang.Integer.parseInt(array[2].replaceAll("[^0-9.]", ""));
		}
		
		return result;
	}
	
	/**
	 * Fuellt Timeline aus
	 * 
	 * @param hotel
	 * @param profile
	 * @return
	 */
	public ArrayList<Event> fillTimeLine(String hotel, Profile profile){
		ArrayList<String> peopleList = new ArrayList<String>();
		peopleList.add(String.valueOf(profile.getAdultCounter()));
		peopleList.add(String.valueOf(profile.getChildCounter()));
		Term Persons = Util.textToTerm(prologListGenerator(peopleList));
		Term EventCategories = Util.textToTerm(prologListGenerator(profile.getSelectedCategories()));
		Atom Hotel = new Atom(hotel);
		Atom Vehicle = new Atom(profile.getArrival());
		
		Atom DayStart = new Atom(String.valueOf(profile.getDayStart()));
		Atom DayEnd = new Atom(String.valueOf(profile.getDayEnd()));
		
		Term HotelCategories = Util.textToTerm(prologListGenerator(profile.getSelectedHotel()));
		Atom FullBudget = new Atom(String.valueOf(profile.getBudgetInCent()));
		
		Variable ResultTimeLine = new Variable("ResultTimeLine");
		Variable TimeLine = new Variable("TimeLine");
		
		Query query =
				new Query(
						"fillTimeLineAllDays",
						new Term[]{Persons, EventCategories, TimeLine, DayStart, DayEnd, Hotel, HotelCategories, Vehicle, FullBudget, ResultTimeLine}
						);
		
		ArrayList<Event> resultArrayList = new ArrayList<Event>();
		
		if(query.hasSolution()){
			Map<String,Term> solution = query.oneSolution();
			String[] arrayResult = solution.get("ResultTimeLine").toString().split(",");
			for(String s:arrayResult){
				System.out.println(s);
			}
			
		}
		
		return resultArrayList;
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
