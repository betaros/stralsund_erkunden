package src;

import java.util.Hashtable;

import jpl.*;

public class Prolog {
	
	public Prolog(){
		new Query("consult",
		        new Term[] {new Atom("test.pl")}
		    );
	}
	
	public void sucheVeranstaltung(){
		//Query suchQuery = new Query(text, arg);
	}
	
	public void berechneEntfernung(String ortA, String ortB){
		Variable veranstaltungA = new Variable(ortA);
		Variable veranstaltungB = new Variable(ortB);
		Variable entfernung = new Variable();
		Query entfernungsQuery = 
				new Query(
						"berechneEntfernung",
						new Term[] {veranstaltungA, veranstaltungB, entfernung}
					);
		
		@SuppressWarnings("rawtypes")
		Hashtable ergebnis = entfernungsQuery.oneSolution();
		
		System.out.println("Entfernung zwischen " + ortA + " und " + ortB + ": " + ergebnis.get(entfernung));
	}
}
