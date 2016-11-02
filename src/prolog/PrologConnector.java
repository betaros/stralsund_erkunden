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
	
		Query distanceQuery = 
				new Query(
						new Compound(
								"calcDistance",
								new Term[] {new Atom(placeA), new Atom(placeB), new Variable("X")}
						)
					);
		
		@SuppressWarnings("rawtypes")
		java.util.Hashtable solution;
		Map ergebnis = distanceQuery.oneSolution();
		
		System.out.println("Entfernung zwischen " + placeA + " und " + placeB + ": " + ergebnis.get("X"));
	

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
