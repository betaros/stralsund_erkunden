// https://github.com/SWI-Prolog/packages-jpl/tree/master/examples/java

package prolog;

import java.util.ArrayList;
import java.util.Map;

import org.jpl7.*;

public class PrologConnector {
	
	public PrologConnector(){

		Query.hasSolution("consult('src/prolog/functions.pl')");
	
	}
	
	public void findEvent(){
		//Query suchQuery = new Query(text, arg);
	}
	
	public void calcDistance(String placeA, String placeB){
		Variable eventA = new Variable(placeA);
		Variable eventB = new Variable(placeB);
		Variable distance = new Variable();
		Query distanceQuery = 
				new Query(
						"calcDistance",
						new Term[] {eventA, eventB, distance}
					);
		
		@SuppressWarnings("rawtypes")
		Map ergebnis = distanceQuery.oneSolution();
		
		System.out.println("Entfernung zwischen " + placeA + " und " + placeB + ": " + ergebnis.get(distance));
	}
	
	public ArrayList<String> getCategories(){
		ArrayList<String> categories = new ArrayList<String>();
		
		categories.add("Schwimmen");
		categories.add("Einkaufen");
		categories.add("Freizeit");
		categories.add("Fitness");
		categories.add("Bildung");
		
		return categories;
	}
}
