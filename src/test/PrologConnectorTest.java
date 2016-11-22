package test;

import static org.junit.Assert.*;

import java.util.ArrayList;

import org.junit.Test;

import prolog.PrologConnector;
import src.Event;
import src.Profile;

public class PrologConnectorTest {

	PrologConnector pc = new PrologConnector();

	@Test
	public void testFindEvent() {
		Event result = pc.findEvent("Hansedom", false);
		ArrayList<String> food = new ArrayList<String>();
		food.add("Fast-Food");
		
		ArrayList<String> categories = new ArrayList<String>();
		categories.add("Sport");
		categories.add("Schwimmen");
		categories.add("Sauna");
		
		Event toTestEvent = new Event("Hansedom", 54.320021, 13.04384, 2000, 1000, categories, food, 800, 1000, 1, 0, 0);
		
		StringBuilder expected = new StringBuilder();
		expected.append(toTestEvent.getName());
		expected.append(toTestEvent.getLatitude());
		expected.append(toTestEvent.getLongitude());
		expected.append(toTestEvent.getPriceInCentAdult());
		expected.append(toTestEvent.getPriceInCentChild());
		for(String s:toTestEvent.getCategories()){
			expected.append(s);
		}
		
		StringBuilder toTest = new StringBuilder();
		toTest.append(result.getName());
		toTest.append(result.getLatitude());
		toTest.append(result.getLongitude());
		toTest.append(result.getPriceInCentAdult());
		toTest.append(result.getPriceInCentChild());
		for(String s:result.getCategories()){
			toTest.append(s);
		}
		
		assertEquals(expected.toString(), toTest.toString());
	}

	@Test
	public void testGetEventsByPrologWithCategories() {
		ArrayList<String> resultCategories = new ArrayList<String>();
		resultCategories.add("Freizeit");
		
		ArrayList<String> resultEvents = pc.getEventsByPrologWithCategories(resultCategories);
		ArrayList<String> expectedEvents = new ArrayList<String>();
		expectedEvents.add("Ozeaneum");
		expectedEvents.add("Meeresmuseum");
		expectedEvents.add("Nautineum");
		expectedEvents.add("Marinemuseum");
		expectedEvents.add("Cinestar");

		StringBuilder resultString = new StringBuilder();
		for(String s:resultEvents){
			resultString.append(s);
		}
		
		StringBuilder expectedString = new StringBuilder();
		for(String s:expectedEvents){
			expectedString.append(s);
		}
		
		assertEquals(expectedString.toString(), resultString.toString());
	}

	@Test
	public void testCheckEventForBudget() {
		ArrayList<String> eventslist = new ArrayList<String>();
		eventslist.add("Hansedom");
		eventslist.add("Ozeaneum");
		eventslist.add("Nautineum");
		eventslist.add("Meeresmuseum");
		
		ArrayList<String> resultEventList = pc.checkEventForBudget(2, 1, 20000, eventslist);
		
		ArrayList<String> expectedEventList = new ArrayList<String>();
		// Wackelig, da zzT alle Events gleich viel kosten
		expectedEventList = eventslist;
		
		StringBuilder resultString = new StringBuilder();
		for(String s:resultEventList){
			resultString.append(s);
		}
		
		StringBuilder expectedString = new StringBuilder();
		for(String s:expectedEventList){
			expectedString.append(s);
		}
		
		assertEquals(expectedString.toString(), resultString.toString());
	}

	@Test
	public void testSearchUsefulEvents() {
		ArrayList<String> resultCategories = new ArrayList<String>();
		resultCategories.add("Freizeit");
		
		ArrayList<Event> resultEventList = pc.searchUsefulEvents(2, 1, 20000, resultCategories);
		
		ArrayList<Event> expectedEventList = new ArrayList<Event>();
		expectedEventList.add(pc.findEvent("Ozeaneum", false));
		expectedEventList.add(pc.findEvent("Meeresmuseum", false));
		expectedEventList.add(pc.findEvent("Nautineum", false));
		expectedEventList.add(pc.findEvent("Marinemuseum", false));
		expectedEventList.add(pc.findEvent("Cinestar", false));
		
		StringBuilder resultString = new StringBuilder();
		for(Event e:resultEventList){
			StringBuilder resultEvent = new StringBuilder();
			resultEvent.append(e.getName());
			resultEvent.append(e.getLatitude());
			resultEvent.append(e.getLongitude());
			resultEvent.append(e.getPriceInCentAdult());
			resultEvent.append(e.getPriceInCentChild());
			for(String s:e.getCategories()){
				resultEvent.append(s);
			}
			resultString.append(resultEvent);
		}
		
		StringBuilder expectedString = new StringBuilder();
		for(Event e:expectedEventList){
			StringBuilder expectedEvent = new StringBuilder();
			expectedEvent.append(e.getName());
			expectedEvent.append(e.getLatitude());
			expectedEvent.append(e.getLongitude());
			expectedEvent.append(e.getPriceInCentAdult());
			expectedEvent.append(e.getPriceInCentChild());
			for(String s:e.getCategories()){
				expectedEvent.append(s);
			}
			expectedString.append(expectedEvent.toString());
		}
		
		assertEquals(expectedString.toString(), resultString.toString());
	}

	@Test
	public void testGetCategoriesByProlog() {
		ArrayList<String> resultCategories = pc.getCategoriesByProlog();
		ArrayList<String> expectedCategories = new ArrayList<String>();
		expectedCategories.add("Bar");
		expectedCategories.add("Kneipe");
		expectedCategories.add("Freizeit");
		expectedCategories.add("Unterhaltung");
		expectedCategories.add("Tiere");
		expectedCategories.add("Bildung");
		expectedCategories.add("Studium");
		expectedCategories.add("Museum");
		expectedCategories.add("Einkaufen");
		expectedCategories.add("Grosshandel");
		expectedCategories.add("Sport");
		expectedCategories.add("Schwimmen");
		expectedCategories.add("Sauna");
		
		StringBuilder resultString = new StringBuilder();
		for(String s:resultCategories){
			resultString.append(s);
		}
		
		StringBuilder expectedString = new StringBuilder();
		for(String s:expectedCategories){
			expectedString.append(s);
		}
		
		assertEquals(expectedString.toString(), resultString.toString());
	}

	@Test
	public void testCheckEventsOnTimeOneEvent() {
		ArrayList<Event> eventlist = new ArrayList<Event>();
		eventlist.add(pc.findEvent("Ozeaneum", false));
		Profile profile = new Profile(2, 20000, 2, 1);
		boolean result = pc.checkEventsOnTime(2, 1, eventlist, 1, "Hansedom", 20000, profile);
		
		assertTrue(result);
	}

	@Test
	public void testCheckEventsOnTimeTwoEvents() {
		fail("Not yet implemented");
	}

	@Test
	public void testCalcApproachForEvent() {
		fail("Not yet implemented");
	}

}
