// https://github.com/SWI-Prolog/packages-jpl/tree/master/examples/java

package prolog;

import java.util.Hashtable;
import java.util.Map;

import org.jpl7.*;

public class PrologConnector {
	
	public PrologConnector(){
		Query.hasSolution("consult('src/prolog/functions.pl')");
		//new Query("consult",
		//        new Term[] {new Atom("test.pl")}
		//    );
		
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
}
