// https://github.com/SWI-Prolog/packages-jpl/tree/master/examples/java

package prolog;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Map;

import org.jpl7.*;
import org.jpl7.Integer;

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
			String arrayPrice[]        = solution.get("Price").toString().split(",");
			String arrayBusinesshours[];
			String arrayDuration[];
			String arrayFood[];
			int duration = 0;
			ArrayList<String> foodList = new ArrayList<String>();
			int businessOpen = 0;
			int businessClosed = 0;
			
			if(!hotel){
				arrayBusinesshours = solution.get("Businesshours").toString().split(",");
				arrayDuration      = solution.get("Duration").toString().split(",");
				arrayFood          = solution.get("Food").toString().split(",");
				
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
				
				for(int i = 0; i<arrayFood.length; i++){
					arrayFood[i] = arrayFood[i].replaceAll("[^A-Za-z0-9.]", "");
					if(debug){
						System.out.println("findEvent: Food:       " + arrayFood[i]);
					}
				}
				
				arrayBusinesshours = Arrays.copyOf(arrayBusinesshours, arrayBusinesshours.length-1);
				arrayDuration = Arrays.copyOf(arrayDuration, arrayDuration.length-1);
				arrayFood = Arrays.copyOf(arrayFood, arrayFood.length-1);
				
				duration = java.lang.Integer.parseInt(arrayDuration[0]);
				
				for(String food:arrayFood){
					foodList.add(food);
				}
				
				businessOpen = java.lang.Integer.parseInt(arrayBusinesshours[0]);
				businessClosed = java.lang.Integer.parseInt(arrayBusinesshours[1]);
			}
				
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
			
			for(int i = 0; i<arrayPrice.length; i++){
				arrayPrice[i] = arrayPrice[i].replaceAll("[^0-9]", "");
				if(debug){
					System.out.println("findEvent: Price:      " + arrayPrice[i]);
				}
			}
			
			arrayPosition = Arrays.copyOf(arrayPosition, arrayPosition.length-1);
			arrayCategories = Arrays.copyOf(arrayCategories, arrayCategories.length-1);
			if(!hotel){
				arrayPrice = Arrays.copyOf(arrayPrice, arrayPrice.length-1);
			}
			
			double lat = Double.parseDouble(arrayPosition[0]);
			double lon = Double.parseDouble(arrayPosition[1]);
			int priceAdult = 0;
			int priceReduced = 0;
			if(hotel){
				System.out.println(title);
				System.out.println(title + " " + arrayPrice[0]);
				priceAdult = java.lang.Integer.parseInt(arrayPrice[0]);
				priceReduced = java.lang.Integer.parseInt(arrayPrice[0]);
			} else {
				System.out.println(title);
				System.out.println(title + " " + arrayPrice[0]);
				System.out.println(title + " " + arrayPrice[1]);
				priceAdult = java.lang.Integer.parseInt(arrayPrice[0]);
				priceReduced = java.lang.Integer.parseInt(arrayPrice[1]);
			}
			
			ArrayList<String> categoryList = new ArrayList<String>();
			
			for(String category:arrayCategories){
				categoryList.add(category);
			}
			
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
		StringBuilder sb = new StringBuilder();
		sb.append("[");
		for(String s:hotelCategories){
			sb.append(s);
			sb.append(",");
		}
		sb.deleteCharAt(sb.length()-1);
		sb.append("]");
		
		Term HotelCategories = Util.textToTerm(sb.toString());
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
	 * @param eventslist
	 * @param profile
	 * @return
	 */
	public ArrayList<String> checkEventForBudget(ArrayList<Event> eventlist, Profile profile){
		Term People = Util.textToTerm("[" + profile.getAdultCounter() + "," + profile.getChildCounter() + "]");
		
		Integer Budget = new Integer(profile.getBudgetInCent());
		String eventTermList = prologTermFromEvent(eventlist);
		System.out.println(eventTermList);
		
		Term termEvents = Util.textToTerm(eventTermList);
		Variable X = new Variable("X");

		Query query =
				new Query(
						"checkEventForBudget",
						new Term[] {People,Budget,termEvents,X}
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

		Term People = Util.textToTerm("[" + adultCount + "," + reducedCount + "]");
		Integer Budget = new Integer(budget);
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
		Term People = Util.textToTerm("[" + adultCount + "," + reducedCount + "]");
		
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
		Integer DayStart = new Integer(dayStart);
		Integer DayEnd = new Integer(dayEnd);
		Atom Hotel = new Atom(hotel);
		Atom HotelCategory = new Atom(hotelCategory);
		Integer Budget = new Integer(budget);
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
		Term People = Util.textToTerm("[" + adultCount + "," + reducedCount + "]");
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
		Integer EventTime = new Integer(eventTime);
		
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
		Term Persons = Util.textToTerm("[" + profile.getAdultCounter() + "," + profile.getChildCounter() + "]");
		Term EventCategories = Util.textToTerm(prologListGenerator(profile.getSelectedCategories()));
		Atom Hotel = new Atom(hotel);
		Atom Vehicle = new Atom(profile.getArrival());
		switch(profile.getArrival()){
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
		
		Integer DayStart = new Integer(profile.getDayStart());
		Integer DayEnd = new Integer(profile.getDayEnd());
		
		StringBuilder hc = new StringBuilder();
		hc.append("[");
		for(String s:profile.getSelectedHotel()){
			hc.append(s);
			hc.append(",");
		}
		if(!profile.getSelectedHotel().isEmpty()){
			hc.deleteCharAt(hc.length()-1);
		}
		hc.append("]");
		Term HotelCategories = Util.textToTerm(hc.toString());
		Integer FullBudget = new Integer(profile.getBudgetInCent());
		
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
			String[] arrayResult = solution.get("ResultTimeLine").toString().split(", ");
			ArrayList<String> queryResultArrayList = new ArrayList<String>();
			for(int i = 0; i<arrayResult.length; i++){
				arrayResult[i] = arrayResult[i].replaceAll("[^A-Za-z0-9. ]", "");
				if(!arrayResult[i].equals("")){
					queryResultArrayList.add(arrayResult[i]);
				}
			}
			int counter = 0;
			String title = "";
			String day = "";
			String eventStart = "";
			String duration = "";
			for(String s:queryResultArrayList){
				switch(counter){
				case 0:
					title = s;
					counter++;
					break;
				case 1:
					day = s;
					counter++;
					break;
				case 2:
					eventStart = s;
					counter++;
					break;
				case 3:
					duration = s;
					counter++;
					break;
				default:
					Event tempEvent = findEvent(title, false);
					if(tempEvent != null){
						resultArrayList.add(new Event(title, tempEvent.getLatitude(), tempEvent.getLongitude(), tempEvent.getPriceInCentAdult(), tempEvent.getPriceInCentChild(), tempEvent.getCategories(), tempEvent.getFood(), java.lang.Integer.parseInt(day), java.lang.Integer.parseInt(eventStart), java.lang.Integer.parseInt(duration), tempEvent.getBusinesshoursBegin(), tempEvent.getBusinesshoursEnd()));
					}
					counter = 0;
				}
			}
			
			Event hotelEvent = findEvent(hotel, true);
			resultArrayList.add(hotelEvent);
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
	
	/**
	 * Generiert aus EventListe einen Term
	 * @param eventlist
	 * @return
	 */
	private String prologTermFromEvent(ArrayList<Event> eventlist){
		StringBuilder sb = new StringBuilder();
		sb.append("[");
		for(Event e:eventlist){
			sb.append("[");
			sb.append(e.getName());
			sb.append(",[");
			sb.append(e.getLongitude());
			sb.append(",");
			sb.append(e.getLatitude());
			sb.append("],");
			sb.append("[");
			for(String s:e.getCategories()){
				sb.append(s);
				sb.append(",");
			}
			if(!e.getCategories().isEmpty()){
				sb.deleteCharAt(sb.length()-1);
			}
			sb.append("],");
			sb.append("[");
			for(String s:e.getFood()){
				sb.append(s);
				sb.append(",");
			}
			if(!e.getFood().isEmpty()){
				sb.deleteCharAt(sb.length()-1);
			}
			sb.append("],");
			sb.append("[");
			sb.append(e.getPriceInCentAdult());
			sb.append(",");
			sb.append(e.getPriceInCentChild());
			sb.append("],");
			sb.append("[");
			sb.append(e.getBusinesshoursBegin());
			sb.append(",");
			sb.append(e.getBusinesshoursEnd());
			sb.append("],");
			sb.append("[");
			sb.append(e.getDuration());
			sb.append("]");
			sb.append("]");
		}
		sb.append("]");
		
		return sb.toString();
	}
}
