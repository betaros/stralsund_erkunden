// https://github.com/SWI-Prolog/packages-jpl/tree/master/examples/java

package prolog;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Map;

import org.jpl7.*;

import src.Event;

public class PrologConnector {

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
		Variable Price = new Variable("Price");

		Query query =
				new Query(
						"event",
						new Term[] {Name, Position, Categories, Price}
						);

		Event e = null;
		
		if(query.hasSolution()){
			Map<String, Term> solution = query.oneSolution();
			String arrayPosition[]   = solution.get("Position").toString().split(",");
			String arrayCategories[] = solution.get("Categories").toString().split(",");
			String arrayPrice[]      = solution.get("Price").toString().split(",");

			for(int i = 0; i<arrayPosition.length; i++){
				arrayPosition[i] = arrayPosition[i].replaceAll("[^A-Za-z0-9.]", "");
				System.out.println("findEvent: Position:   " + arrayPosition[i]);
			}
			
			for(int i = 0; i<arrayCategories.length; i++){
				arrayCategories[i] = arrayCategories[i].replaceAll("[^A-Za-z0-9.]", "");
				System.out.println("findEvent: Categories: " + arrayCategories[i]);
			}
			
			for(int i = 0; i<arrayPrice.length; i++){
				arrayPrice[i] = arrayPrice[i].replaceAll("[^A-Za-z0-9.]", "");
				System.out.println("findEvent: Price:      " + arrayPrice[i]);
			}
			
			arrayPosition = Arrays.copyOf(arrayPosition, arrayPosition.length-1);
			arrayCategories = Arrays.copyOf(arrayCategories, arrayCategories.length-1);
			arrayPrice = Arrays.copyOf(arrayPrice, arrayPrice.length-1);
			
			double lat = Double.parseDouble(arrayPosition[0]);
			double lon = Double.parseDouble(arrayPosition[1]);
			int priceAdult = java.lang.Integer.parseInt(arrayPrice[0]);
			int priceReduced = java.lang.Integer.parseInt(arrayPrice[1]);
			ArrayList<String> categoryList = new ArrayList<String>();

			for(String category:arrayCategories){
				categoryList.add(category);
			}

			e = new Event(title, lat, lon, priceAdult, priceReduced, categoryList);
		}

		return e;
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

		System.out.println("Debug: " + termCategories.toString());

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
				System.out.println("getEventsByPrologWithCategories: " + array[i]);
				if (!events.contains(array[i])) {
					events.add(array[i]);
				}
			}
		}

		return events;
	}

	/**
	 * Ueberprueft, ob die Evente im Budget liegen
	 * @param reduced
	 * @param adult
	 * @param budget
	 * @param eventslist
	 * @return
	 */
	public ArrayList<String> checkEventForBudget(int reduced, int adult, int budget, ArrayList<String> eventslist){
		ArrayList<String> persons = new ArrayList<String>();
		persons.add(String.valueOf(adult));
		persons.add(String.valueOf(reduced));
		
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
	 * Liste sortieren
	 * Events senden
	 * Event als Liste mit Namen, Beginn, usw schicken
	 */
	public void calcApproachForEvent(){
		
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
				System.out.println("getCategoriesByProlog: " + array[i]);
				if (!categories.contains(array[i])) {
					categories.add(array[i]);
				}
			}
		}

		return categories;
	}

	/**
	 * Ueberprueft, ob ein Event zeitlich passt (einzelnes Event)
	 * 
	 * @param reducedCount
	 * @param adultCount
	 * @param eventList
	 * @param dayStart
	 * @param hotel
	 * @param budget
	 * @param returnValue
	 * @param price
	 * @return
	 */
	public boolean checkEventsOnTime(int reducedCount, int adultCount, ArrayList<String> eventList, int dayStart, String hotel, int budget, int returnValue, int price){
		ArrayList<String> peopleList = new ArrayList<String>();
		peopleList.add(String.valueOf(adultCount));
		peopleList.add(String.valueOf(reducedCount));
		Term People = Util.textToTerm(prologListGenerator(peopleList));
		Term EventList = Util.textToTerm(prologListGenerator(eventList));
		Atom DayStart = new Atom(String.valueOf(dayStart));
		Atom Hotel = new Atom(hotel);
		Atom Budget = new Atom(String.valueOf(budget));
		Atom Price = new Atom(String.valueOf(price));
		Atom ReturnValue = new Atom(String.valueOf(returnValue));
		
		Variable X = new Variable();
		
		Query query =
				new Query(
					"checkEventsOnTime",
					new Term[]{People, X, EventList, DayStart, Hotel, Budget, ReturnValue, Price}
				);
		
		boolean result = false;
		
		if(query.hasSolution()){
			Map<String,Term> solution = query.oneSolution();
			String array = solution.get("X").toString();
			if(array.contains("true")){
				result = true;
			}
		}
		
		return result;
	}
	
	/**
	 * Ueberprueft, ob ein Event zeitlich passt (zwei Events)
	 * 
	 * @param reducedCount
	 * @param adultCount
	 * @param eventList
	 * @param dayStart
	 * @param hotel
	 * @param budget
	 * @param returnValue
	 * @param price
	 * @return
	 */
	public boolean checkEventsOnTime(int reducedCount, int adultCount, String prevEvent, ArrayList<String> eventList, int dayStart, String hotel, int budget, int returnValue, int price){
		ArrayList<String> peopleList = new ArrayList<String>();
		peopleList.add(String.valueOf(adultCount));
		peopleList.add(String.valueOf(reducedCount));
		Term People = Util.textToTerm(prologListGenerator(peopleList));
		Atom PrevEvent = new Atom(prevEvent);
		Term EventList = Util.textToTerm(prologListGenerator(eventList));
		Atom DayStart = new Atom(String.valueOf(dayStart));
		Atom Hotel = new Atom(hotel);
		Atom Budget = new Atom(String.valueOf(budget));
		Atom Price = new Atom(String.valueOf(price));
		Atom ReturnValue = new Atom(String.valueOf(returnValue));
		
		Variable X = new Variable();
		
		Query query =
				new Query(
					"checkEventsOnTime",
					new Term[]{People, PrevEvent, X, EventList, DayStart, Hotel, Budget, ReturnValue, Price}
				);
		
		boolean result = false;
		
		if(query.hasSolution()){
			Map<String,Term> solution = query.oneSolution();
			String array = solution.get("X").toString();
			if(array.contains("true")){
				result = true;
			}
		}
		
		return result;
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
