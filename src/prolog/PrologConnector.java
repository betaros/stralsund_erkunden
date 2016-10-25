// https://github.com/SWI-Prolog/packages-jpl/tree/master/examples/java

package prolog;

import java.util.Hashtable;

import org.jpl7.*;

public class PrologConnector {
	
	public PrologConnector(){
		Query.hasSolution("consult('functions.pl')");
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
						"berechneEntfernung",
						new Term[] {eventA, eventB, distance}
					);
		
		@SuppressWarnings("rawtypes")
		Hashtable ergebnis = (Hashtable) distanceQuery.oneSolution();
		
		System.out.println("Entfernung zwischen " + placeA + " und " + placeB + ": " + ergebnis.get(distance));
	}
}
