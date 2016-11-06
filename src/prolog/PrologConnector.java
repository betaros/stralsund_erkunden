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
		
		Map ergebnis = distanceQuery.oneSolution();
		
		System.out.println("Entfernung zwischen " + placeA + " und " + placeB + ": " + ergebnis.get("X"));
	

	}

	
	public ArrayList<String> getCategoriesByProlog(){
		ArrayList<String> categories = new ArrayList<String>();
		
		Variable X = new Variable("X");
		/*
		
		Query q4 =
		    new Query(
		        "category",
		        new Term[] {X}
		    );

		Map<String, Term>[] solutions = q4.allSolutions();

	    for ( int i=0 ; i<solutions.length ; i++ ) {
	        System.out.println( "X = " + solutions[i].get("X"));
	        categories.add(solutions[i].get("X").toString());
	    }
	    
	    */
		Query q4 =
			    new Query(
			        "findAllCategories",
			        new Term[] {X}
			    );

		Map<String, Term>[] solutions = q4.allSolutions();

	    for ( int i=0 ; i<solutions.length ; i++ ) {
	        System.out.println( "X = " + solutions[i].get("X"));
	        categories.add(solutions[i].get("X").toString());
	    }
		        
	    return categories;
	}
}
