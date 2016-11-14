// https://github.com/SWI-Prolog/packages-jpl/tree/master/examples/java

package prolog;

import java.util.ArrayList;
import java.util.Map;
import org.jpl7.*;

public class PrologConnector {

	public PrologConnector(){
		Query.hasSolution("consult('src/prolog/functions.pl')");
	}

	public ArrayList<String> findEvents(){
		ArrayList<String> events = new ArrayList<String>();
		Variable X = new Variable("X");
		Variable Y = new Variable("Y");
		Variable Z = new Variable("Z");
		Variable V = new Variable("V");

		Query query =
			new Query(
				"event",
				new Term[] {X,Y,Z,V}
			);

		Map<String, Term>[] solutions = query.allSolutions();

		for ( int i=0 ; i<solutions.length ; i++ ) {
			System.out.println( "X = " + solutions[i].get("X"));
			events.add(solutions[i].get("X").toString().replace("'", ""));
		}

		return events;
	}

	public void calcDistance(String placeA, String placeB){
		Query distanceQuery = 
			new Query(
				new Compound(
					"calcDistance",
					new Term[] {new Atom(placeA), new Atom(placeB), new Variable("X")}
				)
			);

		Map<String, Term> ergebnis = distanceQuery.oneSolution();
		System.out.println("Entfernung zwischen " + placeA + " und " + placeB + ": " + ergebnis.get("X"));
	}
	
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

		Map<String, Term> solution = query.oneSolution();
		String[] array = solution.get("X").toString().split(",");

		for(int i = 0; i<array.length-1; i++){
			array[i] = array[i].replaceAll("[^A-Za-z0-9 ]", "");
			System.out.println("getEventsByPrologWithCategories: " + array[i]);
			if (!events.contains(array[i])) {
				events.add(array[i]);
			}
		}

		return events;

	}

	public ArrayList<String> getCategoriesByProlog(){
		ArrayList<String> categories = new ArrayList<String>();
		Variable X = new Variable("X");

		Query query =
			new Query(
				"findAllCategories",
				new Term[] {X}
			);

		Map<String, Term> solution = query.oneSolution();
		String[] array = solution.get("X").toString().split(" ");
		
		for(int i = 0; i<array.length-1; i++){
			array[i] = array[i].replaceAll("[^A-Za-z0-9 ]", "");
			System.out.println("getCategoriesByProlog: " + array[i]);
			if (!categories.contains(array[i])) {
				categories.add(array[i]);
			}
		}

		return categories;
	}
	
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
