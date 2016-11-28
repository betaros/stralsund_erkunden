:- use_module(events).
:- use_module(java_connection_functions).

:- use_module(library(lists)).
:- use_module(library(random)).
 
/*
* Konstanten fuer die Berechnungen
*/

latInKm(X, Res) :-
	Res is X * 111.66.

lonInKm(X, Res) :-
	Res is X * 19.39.

/*----------------------------------------------------------------------------------------------*/

/*
* Sucht alle Kategorien für Events
*/
findAllCategories(Categories):-
	findall(X, event(_,_,X,_,_,_,_), L),
	mergeListOfListsToList(C1,L),
	Categories = C1.

/*
* Sucht alle Kategorien für Restaurants, Imbisse ...
*/
findAllFoodCategories(Categories):-
	findall(X, event(_,_,_,X,_,_,_), L),
	mergeListOfListsToList(C1,L),
	Categories = C1.
	
/*
* Sucht alle Kategorien der Hotels
*/
findAllHotelCategories(Categories):-
	findall(X, hotel(_,_,_,X), L),
	mergeListOfListsToList(C1,L),
	Categories = C1.

/*----------------------------------------------------------------------------------------------*/

/*
* Berechnet die Entfernung zwischen zwei Veranstaltungen
* Entfernung = sqrt((XA-XB)^2 + (YA-YB)^2)
* calcDistance(Name Veranstaltung A, Name Veranstaltung B, Entfernung
*/	
calcDistance(EventA, EventB, Distance) :-
	(
		event(EventA, [XA, YA], _, _, _, _, _)
		;
		hotel(EventA, [XA, YA], _, _)	
	),
		(
		event(EventB, [XB, YB],  _, _, _, _, _)
		;
		hotel(EventB, [XB, YB],  _, _)	
	),
	latInKm(XA, XAinKm),
	latInKm(XB, XBinKm),
	lonInKm(YA, YAinKm),
	lonInKm(YB, YBinKm), 
	TempX is XAinKm - XBinKm,
	TempY is YAinKm - YBinKm,
	PotX is TempX * TempX,
	PotY is TempY * TempY,
	AddBoth is PotX + PotY,
	Distance is sqrt(AddBoth).

/*----------------------------------------------------------------------------------------------*/

/*
* Gibt die möglichen Events zu den Kategorien zurück, wenn Events leer
*/
searchEventsOnCategory(Categories,Events):-
	findall([X,V], event(X,_,V, _, _,_,_), List),
	compareCategories(List,Categories,Events1),
	Events = Events1.
	
/*
* Gibt mögliche Hotels zu den Kategorien zurück
*/
searchHotelsOnCategory(Categories,Hotels):-
	findall([X,V], hotel(X,_,_,V), List),
	compareCategories(List,Categories,Hotels1),
	Hotels = Hotels1.

/*
* Vergleicht die Liste der Kategorien mit der übergebenen Liste
*/	
compareCategories([E|L],Categories,List1):-
	compareCategories(L,Categories,List2),
	E = [X,Y],
	(  compare_list(Y,Categories)
	-> (
		append([X],List2,List3),
	   	List1 = List3
	   )
	   ;
	   (
	   	List1 = List2
	   )	
	).
	
compareCategories([],_,List1):-
	List1 = [].


/*----------------------------------------------------------------------------------------------*/


/*
*Prüft für alle Events der Liste ob sie einzeln nicht zu teuer sind und gibt die zurück die 
*Preislich in das Budget nicht übersteigen
*Persons = [Count of Adult, Count of Reduced]
*Budget = Price in cent
*MyEvents = ['EventA', 'EventB' ..]
*ValidEvents = empty Atom -> becomes List of valid events
*/
checkEventsForBudget(Persons,Budget,MyEvents,ValidEvents):-
	checkEventForBudget(Persons,Budget,MyEvents,ValidEvents1),
	ValidEvents = ValidEvents1.

checkEventForBudget(_,_,[],ValidEvents):-
	ValidEvents = [].
	
checkEventForBudget(Persons,Budget,[Event|MyEvents],ValidEvents):-
	checkEventForBudget(Persons,Budget,MyEvents,ValidEvents1),
	((
		event(Event,_,_,_,[AdultPrice,ReducedPrice],_,_),
		[AdultCount|ReducedCount] = Persons,
		Price is (AdultCount*AdultPrice)+(ReducedCount*ReducedPrice),
		write(Price), nl,
		Budget >= Price,		
		append([Event],ValidEvents1,ValidEvents2),
		ValidEvents = ValidEvents2
	)
	;
	(
		ValidEvents = ValidEvents1
	)).

/*
*searchUsefulEvents
*/
searchUsefulEvents(Persons, Budget, Categories, UsefulEvents):-
	searchEventsOnCategory(Categories, Events1),
	checkEventsForBudget(Persons,Budget,Events1,ValidEvents),
	UsefulEvents = ValidEvents.




/*----------------------------------------------------------------------------------------------*/


/*
Überprüft die gesamte Timeline
checkEventsOnTime(Persons,[Eventlist] ,DayStart, DayEnd, Hotel, HotelCategorie, Budget, Return, Price):-
Persons = Personen = [X,Y] = [Anzahl Erwachsene, Anzahl Ermäßigte]
Eventlist = Eventliste = [Event1, Event2, .., EventX] 
	EventX = [Name des Events, Tag, Startzeit, Dauer, Anfahrt]
Daystart = Startuhrzeit des Tages
Hotel = Name des Hotels
HotelCategorie = Kategorie/nwunsch des Nutzers (wird nur beachtet, wenn kein Hotel angegeben)
Budget = Maximales Budget
Return = Rückgabewert wird true oder false
Price = Gesamtpreis der Tour

Beispiel:
trace, 
checkEventsOnTime([1,2], 
[
['Cinestar', 1, 1100, 100, 'Car'],
['Fachhochschule Stralsund', 1, 1700, 100, 'Car'],
['Marinemuseum', 2, 700, 100, 'Car'],
['Nautineum', 1, 900, 100, 'Car'], 
['Meeresmuseum', 2, 1100, 100, 'Car'],
['Ozeaneum', 2, 1700, 100, 'Car'],
['Citti', 1, 700, 100, 'Car'],
['Strelapark', 2, 900, 100, 'Car']
], 
500, 2200, 'X Sterne Hotel', _, 
10000000, Return, Price).

*/

checkEventsOnTime(Persons, EventList, DayStart, DayEnd, Hotel, HotelCategorie, Budget, Return, Price):-
	sortEventList(EventList,SortedEventList),
	((
		nonvar(Hotel)
	)
	;
	(
		var(Hotel),
		findHotelsForTrip(HotelCategorie, Hotel1), 
		Hotel = Hotel1
	)),
	checkTimeLine(Persons, SortedEventList, DayStart, DayEnd, Hotel, Budget, Return1, Price1),
	Return = Return1,
	Price = Price1.	


/*
checkTimeLine(Persons,[Eventlist] ,DayStart, Hotel, Budget, Return, Price):-
Persons = Personen = [X,Y] = [Anzahl Erwachsene, Anzahl Ermäßigte]
Eventlist = Eventliste = [Event1, Event2, .., EventX] 
	EventX = [Name des Events, Tag, Startzeit, Dauer, Anfahrt]
Daystart = Startuhrzeit des Tages
DayEnd = Uhrzeit des Tagesendes
Hotel = Name des Hotels
Budget = Maximales Budget
Return = Rückgabewert wird true oder false
Price = Gesamtpreis der Tour

Beispiel positiv an einem Tag:
checkTimeLine([1,2], [['Haus 8',1,1030,100,'Car'],['Zoo',1,1230,100,'Car']],800, 2200, 'X Sterne Hotel', 1000000, Return, Price).
Beispiel positiv an 2 Tagen:
checkTimeLine([1,2],[['Haus 8',1,1030,100,'Car'],['Zoo',2,1030,100,'Car']],800, 2200, '1 Sterne Hotel', 100000, Return, Price).
Beispiel positiv an 2 Tagen:
checkTimeLine([1,2],[['Haus 8',1,930,100,'Car'],['Meeresmuseum',2,930,100,'Car'],['Zoo',2,1100,100,'Car']], 800, 2200, '2 Sterne Hotel', 100000, Return, Price).

Beispiel negativ an einem Tag:
checkTimeLine([1,2],[['Haus 8',1,1030,100,'Car'],['Zoo',1,1130,100,'Car']],800, 2200, '1 Sterne Hotel', 100000, Return, Price).
Beispiel negativ an einem Tag:
checkTimeLine([1,2], [['Haus 8',1,830,100,'Car'],['Zoo',1,1230,100,'Car']],800, 2200, 'X Sterne Hotel', 1000000, Return, Price).
Beispiel negativ an 2 Tagen weil letztes Event zu lange:
checkTimeLine([1,2],[['Haus 8',1,830,100,'Car'],['Zoo',2,2130,100,'Car']],800, 2200, '1 Sterne Hotel', 100000, Return, Price).
Beispiel negativ an 2 Tagen:
checkTimeLine([1,2],[['Haus 8',1,830,100,'Car'],['Haus 8',2,830,100,'Car'],['Zoo',2,930,100,'Car']],800, 2200, '1 Sterne Hotel', 100000, Return, Price).
Beispiel negativ an 2 Tagen weil zu früh begonnen:
checkTimeLine([1,2],[['Haus 8',1,830,100,'Car'],['Haus 8',2,830,100,'Car'],['Zoo',2,930,100,'Car']],830, 2200, '1 Sterne Hotelm', 100000, Return, Price).
Beispiel negativ an 2 Tagen weil Budget zu gering:
checkTimeLine([1,2],[['Haus 8',1,830,100,'Car'],['Haus 8',2,830,100,'Car'],['Zoo',2,1030,100,'Car']],800, 2200, '1 Sterne Hotel', 450000, Return, Price).
*/ 


/*
Kalkuliert: 
- Hotel vor dem ersten Event
- Anfahrt zum ersten Event
- Das erste Event
*/
checkTimeLine(Persons,[EventHead|EventsTail],DayStart, DayEnd, Hotel, Budget, Return, Price):-
	(
			write('Prüfe Event ohne Vorgänger'), nl,
		calcHotelPrice(Persons, Hotel, HotelPrice),
		[ThisEvent,_,EventStartTime,EventTime,Vehicle] = EventHead,
			write(Hotel + " zu " + ThisEvent), nl,
		calcApproachForEvent(Persons, _, ThisEvent, Hotel, Vehicle, EventStartTime, [_,_,_,RealStartTime,Price1]),
		calcEventPrice(Persons, ThisEvent, Price2),
			write("Startzeit des Tages: "+DayStart), nl,
			write("Startzeit: "+RealStartTime), nl,
		RealStartTime >= DayStart,
			write("Zeiten Gültig"), nl,
		Price3 is Price1 + Price2 + HotelPrice,
			write("Prüfe auf Preis"), nl,
			write(Budget + Price3), nl,
		Budget >= Price3,
			write("Preis gültig"), nl,
		checkBussinesHours(ThisEvent, EventStartTime, EventTime),
			write("Event gültig"), nl,
		checkTimeLine(Persons, EventHead, EventsTail, DayStart, DayEnd, Hotel, Budget, Return1, Price4),
			write("Price3 " + Price3 + " Price4 " + Price4), nl,
		Price is Price3 + Price4,
			write("Entgültiger Gesamtpreis: "+ Price), nl,
		Return = Return1,
		Budget >= Price
	)
	;
	(
			write("Event ungültig"), nl,
		Price = 0,
		Return = false,!
	).
/*
Kalkuliert: 
- Anfahrt zum ersten Event
- das nächste Event
*/	
checkTimeLine(Persons, PrevEventInput,[EventHead|EventsTail],DayStart, DayEnd, Hotel,Budget, Return, Price):-
	(
			write('Prüfe Event mit Vorgänger'), nl,
		[ThisEvent,Day,EventStartTime,EventTime,Vehicle] = EventHead,
		[PrevEvent,PrevDay,PrevEventStartTime,PrevEventTime,_] = PrevEventInput,
			write("Prüfe " + PrevEvent + " und " + ThisEvent), nl,
		((
			PrevDay \= Day,
				write("Events an unterschiedlichen Tagen"), nl,
				write("checkTimeLine für Vorgängertag start"), nl,
			checkTimeLine(Persons, PrevEventInput, [], _, DayEnd, Hotel, Budget, Return1, Price1a),
				write("checkTimeLine für Vorgängertag beendet"), nl,
			calcApproachForEvent(Persons, _, ThisEvent, Hotel, Vehicle, EventStartTime, [_,_,_,RealStartTime,Price1b]),
				write("Price1a " + Price1a + " Price1b " + Price1b), nl,
			Price1 is Price1a + Price1b,
				write("Startzeit des Tages: "+DayStart), nl,
				write("Startzeit: "+RealStartTime), nl,
			RealStartTime >= DayStart			
		)
		;
		(
			PrevDay = Day,
				write("Events an selben Tag"), nl,
				write(PrevEvent + "zu" + ThisEvent), nl,
			calcApproachForEvent(Persons, PrevEvent, ThisEvent, Hotel, Vehicle, EventStartTime, [_,_,_,RealStartTime,Price1]),
			PrevEventEndTime is PrevEventStartTime+PrevEventTime,
				write("Ende des letzten Events: "+PrevEventEndTime), nl,
				write("Startzeit: "+RealStartTime), nl,
			RealStartTime >= PrevEventEndTime		
		)),
			write("checkTimeLine 3"), nl,
		checkTimeLine(Persons, EventHead, EventsTail, DayStart, DayEnd, Hotel,Budget, Return1, Price2),
		Return = Return1,
		calcEventPrice(Persons, ThisEvent, Price3),
			write("Price1 " + Price1 + " Price2 " + Price2 +" Price3 " + Price3), nl,
		Price is Price3 + Price2 + Price1,
			write("Gesamtpreis bis hier: "+Price), nl,
		Budget >= Price,
		checkBussinesHours(ThisEvent, EventStartTime, EventTime),
			write("Event gültig"), nl
	)
	;
	(
			write("Event ungültig"), nl,
		Price = 0,
		Return = false,!
	).

/*
Kalkuliert: 
- Hotel nach dem letzten Event des Tages
- Anfahrt zum letzten Event des Tages
- Das letzten Event des Tages
*/
checkTimeLine(Persons, PrevEventInput, [], _, DayEnd, Hotel, Budget, Return, Price):-
		nl, write('Letztes Event des Tages wird geprüft'), nl,
	((
		[PrevEvent, _, PrevEventStartTime, PrevEventTime, Vehicle] = PrevEventInput,
			write("Kalkuliere letztes Event und Hotel: "+ PrevEvent), nl, 
		calcApproachForEvent(Persons, PrevEvent, _, Hotel, Vehicle, PrevEventStartTime, [_,_,DriveTime,_,Price1]),
		RealEndTime is PrevEventStartTime + PrevEventTime + DriveTime,
			write("Tagesende nach Events um: "+ RealEndTime), nl,
		RealEndTime =< DayEnd,
		calcHotelPrice(Persons, Hotel, HotelPrice),
		Price is Price1 + HotelPrice,
		Price < Budget,
			write('Letztes Event des Tages gültig'), nl,
		checkBussinesHours(PrevEvent, PrevEventStartTime, PrevEventTime),
		Return = true
	)
	;
	(
		Return = false,
		Price is 0,
			write('Letztes Event des Tages ungültig'), nl
	)).


/*----------------------------------------------------------------------------------------------*/




/*----------------------------------------------------------------------------------------------*/
	
/*
*calcApproachlForEvent
*Berechnet die Anfahrt zum Event
Previousvent = vorheriges Event
ThisEvent = Event zu dem die Anfahrt berechnet wird
Hotel = Das Hotel des Nutzers
Vehicle = Fahrzeug 
EventTime = Startzeit des ThisEvent
Arrivel wird zurückgegeben (Arrival = ('Anfahrt', Vehicle, Zeit in Minuten, Startzeit)
*Wenn PreviousEvent (vorheriges Event) leer, dann wird Hotel genommen.
*Beispiel: calcArrivalForEvent('Cinestar', 'Haus 8', 'Hansedom', 'Car', 800, Arrival).
*/
calcApproachForEvent([AdultCount,ReducedCount], PreviousEvent, ThisEvent, Hotel, Vehicle, EventTime, Approach):-
	((
		nonvar(PreviousEvent),
		Point1 = PreviousEvent
			% ,
			% write('Kalkuliere von PreviousEvent ')
	)
	;
	(
		var(PreviousEvent),
		Point1 = Hotel
			% ,
			% write('Kalkuliere von Hotel ')
	)),
	((
		nonvar(ThisEvent),
		Point2 = ThisEvent
			% ,
			% write('zu ThisEvent'), nl
	)
	;
	(
		var(ThisEvent),
		Point2 = Hotel
			% ,
			% write('zu Hotel'), nl
	)),
	calcDistance(Point1, Point2, Distance),
	vehicle(Vehicle, [AdultPrice,ReducedPrice], Speed),
	ArrivalTime is ceiling(Distance/Speed*60),
		% write("Zeit für Anfahrt: "+ ArrivalTime), nl,
	StartTime is EventTime - ArrivalTime,
	Price is (AdultCount*AdultPrice)+(ReducedCount*ReducedPrice),
		% write("-> Preis für Fahrt: "+Price), nl,
	Approach = ['Anfahrt', Vehicle, ArrivalTime, StartTime, Price].


/*----------------------------------------------------------------------------------------------*/


/*calcEventPrice
calcEventPrice(Persons, Price1, Price2, Event, Price),
Berechnet Preis für Event mit Anfahrt incl. 2 weiterer Preise
Genutzt werden Price1 und Price2 für die Berechnung in der Überprüfung der Timeline
*/
calcEventPrice([AdultCount,ReducedCount], Event, Price):-
		write("Berechne Preis für "+Event), nl,
	event(Event,_,_,_,[AdultPrice,ReducedPrice],_,_),
	Price is (AdultCount*AdultPrice) + (ReducedCount*ReducedPrice),
		write("-> Preis für " + Event + " ist: " + Price), nl. 
	

/*
Berechnet den Preis für das Hotel
In Abhängigkeit der Personen und der benötigten Doppelzimmer
*/
calcHotelPrice(Persons, Hotel, Price):-
	hotel(Hotel,_,PricePerRoom,_),
	[Adult,Reduced] = Persons,
	PersonsCount = Adult + Reduced,
	Rooms is ceiling(PersonsCount/2),
	HotelPrice is Rooms * PricePerRoom,
		write("-> Preis für Hotel für diese Nacht: " + HotelPrice), nl,
	Price is HotelPrice.

/*----------------------------------------------------------------------------------------------*/

	
/*
Findet Hotels zur angegebenen Categorie
*/
findHotelsForTrip(HotelCategorie, Hotel1, Budget, Persons):-
	findall([X,V], hotel(X,_,_,V), List),
	compareCategoriesAndBudget(List,HotelCategorie,Budget,Persons,Hotels),
	Hotels = [Hotel1|_].

/*
* findHotelsForTrip([3], Hotel1, 10, [2,2]).
* findHotelsForTrip([5], Hotel1, 10000000, [2,0]).
* Vergleicht die Liste der Kategorien & Budget mit der übergebenen Liste
*/	
compareCategoriesAndBudget([E|L],Categories,Budget,Persons,List1):-
	compareCategoriesAndBudget(L,Categories,Budget,Persons,List2),
	E = [X,Y],
	((
	
		calcHotelPrice(Persons, X, Price),
		compare_list(Y,Categories), 
		Price  =< Budget)
	-> (
		append([X],List2,List3),
	   	List1 = List3
	   )
	   ;
	   (
	   	List1 = List2
	   )	
	).
	
compareCategoriesAndBudget([],_,_,_,List1):-
	List1 = [].
	
/*
Findet Restaurant passend zur Gruppe
findRestaurant(['Fast-Food'], Restaurant, [1200,1500]).
*/
findRestaurant(FoodCategories, Restaurant,[Starting,Ending]):-
 
	findall([Name,Cat,[Start,End]], event(Name,_,_,Cat,_,[Start,End],_), List),
	compareRestaurants(List,FoodCategories,[Starting,Ending],Restaurants),
	Restaurants = [Restaurant|_].
	
compareRestaurants([E|L],FoodCategories,[Start,End],List1):-
	compareRestaurants(L,FoodCategories,[Start,End],List2),
	E = [Name,Cat,[Opening,Closing]],
	((
	
		compare_list(Cat,FoodCategories), 
		Opening =< Start,
		Duration is Start + 60,
		Duration =< Closing 
		)
	-> (
		append([Name, [Start,Duration]],List2,List3),
	   	List1 = List3
	   )
	   ;
	   (
	   	List1 = List2
	   )	
	).
	
compareRestaurants([],_,_,List1):-
	List1 = [].
/*
Prüft die Öffnungszeiten
*/
checkBussinesHours(ThisEvent, EventStartTime, EventTime):-
	event(ThisEvent,_,_,_,_,[Opening, Closing],_),
	EventStartTime >= Opening,
	EventEndTime is EventStartTime + EventTime,
	EventEndTime =< Closing.

	
	

/*----------------------------------------------------------------------------------------------*/	
/* Mehrfachverwendbare Hilfsfunktionen*/

mergeListOfListsToList(C1,[R|[]]):-
		C1 = R.
	
mergeListOfListsToList(C,[R|L]):-
	mergeListOfListsToList(C1,L),
	append(C1,R,X),
	C = X.
	
/*
* compare_list vergleicht ob mindestens ein Member einer Liste in der anderen Liste ist
*/
compare_list([],[]):-false.
compare_list([],_):-false.
compare_list([L1Head|L1Tail], List2):-
    (member(L1Head,List2)
    )
    ;
    (compare_list(L1Tail,List2)
    ).




/*
Abfolge für das Trennen und Sortieren der Eventliste
sortEventList([
['E1_1', 1, 1000, 100, 'Car'],
['E1_2', 1, 1700, 100, 'Car'],
['E2_1', 2, 700, 100, 'Car'],
['E1_3', 1, 900, 100, 'Car'], 
['E2_2', 2, 1000, 100, 'Car'],
['E2_3', 2, 1700, 100, 'Car'],
['E1_4', 1, 700, 100, 'Car'],
['E2_4', 2, 900, 100, 'Car']], 
SortedEventList).

*/
sortEventList(EventList,SortedEventList):-
	splitList(EventList,1,UnsortedDay1,UnsortedDay2),
	quickSort(UnsortedDay1, SortedDay1),
	quickSort(UnsortedDay2, SortedDay2),
	append(SortedDay1, SortedDay2, SortedEventList1),
	SortedEventList = SortedEventList1.


/*
Teilt die Liste in jeweils eine Liste pro Tag auf
Funktioniert nur bei 2 Tagen
splitList([['E1_1', 1, 1000, 100, 'Car'],['E2_2', 2, 900, 100, 'Car'],['E1_2', 2, 1200, 200, 'Car'],['E2_1', 2, 1300, 100, 'Car'],['E1_2', 1, 1200, 100, 'Car'],['E2_1', 2, 2100, 100, 'Car']], 1, List1, List2).
*/

splitList([], _, [], []).
	
splitList([Head|Rest], A, [Head|Rest1], Rest2) :-
	[_, A, _, _, _] = Head,
    	splitList(Rest, A, Rest1, Rest2).
    	
splitList([Head|Rest], B, Rest1, [Head|Rest2]) :-
	[_, A, _, _, _] = Head,
        A =\= B,
        splitList(Rest, B, Rest1, Rest2).


/*
Sortiert die Liste der Events
quickSort([['E1_1', 1, 1000, 100, 'Car'],['E1_2', 1, 1700, 100, 'Car'],['E1_2', 1, 700, 100, 'Car'],['E1_2', 1, 900, 100, 'Car']], SortedList).
*/
quickSort(List,Sorted):-
	qSort(List,[],Sorted).
	
qSort([],Acc,Acc).

qSort([H|T],Acc,Sorted):-
	pivoting(H,T,L1,L2),
	qSort(L1,Acc,Sorted1),
	qSort(L2,[H|Sorted1],Sorted).
	
pivoting(_,[],[],[]).

pivoting(H,[X|T],[X|L],G):-
	[_, _, XStartTime, _, _] = X,
	[_, _, HStartTime, _, _] = H,
	XStartTime>HStartTime,
	pivoting(H,T,L,G).
	
pivoting(H,[X|T],L,[X|G]):-
	[_, _, XStartTime, _, _] = X,
	[_, _, HStartTime, _, _] = H,
	XStartTime=<HStartTime,
	pivoting(H,T,L,G).
	
list_zerolength(List, Empty) :-
    length(List, Len),
    (   Len == 0
    ->  Empty = true
    ;   Empty = false
    ).	
    
noLunchEvent(TimeLine,Return):-
	Return = false,
	forall(member(Event, TimeLine), 
		Event = [Name,_,_,_,_],
		event(Name,_,_,FoodCats,_,_,_),
		list_zerolength(FoodCats,Empty),
		Empty = false,
		Return = true)
	.
	
	

%calculateFullTimeLine([2,2],1000000,1,600,2100,['Schwimmen','Einkaufen'],['Fast-Food'],[3],TimeLine,Hotel,'Car').
calculateFullTimeLine(Persons,Budget,Day,DayStart,DayEnd,Categories,FoodCategories,Hotelcategories,TimeLine,Hotel,Vehicle):-
	
	%Fall: komplett leer
	(var(Hotel),
	findHotelsForTrip(Hotelcategories, Hotel1, Budget, Persons),
	calcHotelPrice(Persons, Hotel1, Price),
	Budget1 is Budget - Price,
	Hotel is Hotel1,
	findEventForFreeTime(TimeLine, Categories, Persons, Budget1, Hotel, Vehicle, DayStart, DayEnd, DayStart, Result),
	calcEventPrice(Persons, Name, EventPrice),
	Budget1 is Budget1 - EventPrice,
	calcApproachForEvent(Persons, Hotel, Name, Hotel, Vehicle, DayStart, Approach),
	Approach = [_, _, ArrivalTime, StartTime, ApproachPrice],
	Budget1 is Budget1 - ApproachPrice,
	event(Result,_,_,_,_,_,EventTime),
	EventForTimeLine = [Result,Day,ArrivalTime,EventTime,Vehicle],
	append(EventForTimeLine,TimeLine,TimeLine),
	NewTime is ArrivalTime+EventTime,
	calculateFullTimeLine(Persons,Budget1,Day,NewTime,DayEnd,Categories,FoodCategories,Hotelcategories,TimeLine,Hotel,Vehicle)
	)
	;
	%Fall: 1. Event gefüllt, Mittagszeit, noch kein Restuarant ausgewählt
	(
	nonvar(Hotel),
	DayStart > 660,
	DayStart < 840,
	noLunchEvent(TimeLine,Return),
	Return,
	findRestaurant(FoodCategories, Restaurant,[DayStart,DayEnd]),
	Restaurant = [RestaurantName, [Start,Duration]],
	TimeLine = [Head|_],
	calcApproachForEvent(Persons, Head, RestaurantName, Hotel, Vehicle, Start, Approach),
	Approach = [_, _, ArrivalTime, StartTime, ApproachPrice],
	Budget1 is Budget - ApproachPrice,
	EventForTimeLine = [RestaurantName,Day,ArrivalTime,Duration,Vehicle],
	append(EventForTimeLine,TimeLine,TimeLine),
	NewTime is ArrivalTime+Duration,
	calculateFullTimeLine(Persons,Budget1,Day,NewTime,DayEnd,Categories,FoodCategories,Hotelcategories,TimeLine,Hotel,Vehicle)
	)
	;
	(
	write(TimeLine),
	write(Hotel),
	!
	).
	
	
%	findEvent(Persons,Budget,DayStart,DayEnd,Categories,Event),
%	Event = [Name,[Start,Duration],EventPrice],
%	Budget2 is Budget1 - Price,
%	calcApproachForEvent(Persons, Hotel, Name, Hotel, Vehicle, Start, Approach),
%	EventX = [Name, 1, Start, Duration, Approach],
%	append(EventX,TimeLine,TimeLine),
	/*Persons = [A,C],
	Kids = true,
	(C=0,
	Kids = false
	)
	*/
%	write(TimeLine).
	
% findEvent([2,2],100000, 600,2100,['Einkaufen'],Event).
findEvent(Persons,Budget,Start,End,Categories,Event):-
	findall([Name,Cat,[Adultprice,Childprice],[XStart,XEnd],Duration], event(Name,_,Cat,_,[Adultprice,Childprice],[XStart,XEnd],Duration), List),
	compareEvents(List,Persons,Budget,Start,End,Categories,Events),
	checkEventsForBudget(Persons,Budget,Events,ValidEvents),
	ValidEvents = [Event|_].
	
% compareEvents([
compareEvents([E|L],Persons,Budget,Start,End,Categories,List1):-
	compareEvents(L,Persons,Budget,Start,End,Categories,List2),
	E = [Name,Cat,[AdultPrice,ReducedPrice],[Opening,Closing],Duration],
	Persons = [AdultCount,ReducedCount],
	((
		compare_list(Cat,Categories), 
		Opening =< Start,
		Duration+Start =< Closing,
		End - Start >= Duration
		)
	-> (
	
		Price is (AdultCount*AdultPrice)+(ReducedCount*ReducedPrice),
		append([Name, [Start,Duration],Price],List2,List3),
	   	List1 = List3
	   )
	   ;
	   (
	   	List1 = List2
	   )	
	).
	
compareEvents([],_,_,_,_,_,List1):-
	List1 = [].

/*
fillTimeLineAllDays([1,0] ,['Bar', 'Freizeit','Bildung'], [['Marinemuseum',1,1035,45,'Car'],['Cinestar',2,1131,20,'Car']], 800, 2200, 'X Sterne Hotel', _, 'Car', 500000, FTL) 
trace, fillTimeLineAllDays([1,0] ,['Bar', 'Freizeit','Bildung'], [], 800, 2200, 'X Sterne Hotel', _, 'Car', 500000, FTL) 
trace, fillTimeLineAllDays([1,0] ,['Bar', 'Freizeit','Bildung'], [], 800, 2200, _, [4,5], 'Car', 500000, FTL) 
fillTimeLineAllDays([1,0] ,['Bar', 'Freizeit','Bildung'], [], 800, 2200, _, [2], 'Car', 500000, FTL) 

*/

fillTimeLineAllDays(Persons, EventCategories, TimeLine, DayStart, DayEnd, Hotel1, HotelCategories, Vehicle, FullBudget, ResultTimeLine):-
	((
		var(Hotel1),
		write("Kein Hotel angegeben"), nl,
		findHotelsForTrip(HotelCategories, Hotel2, FullBudget, Persons),
		write("Hotel: " + Hotel2), nl
	)
	;
	(
		Hotel2 = Hotel1,
		write("Hotel: " + Hotel2), nl
	)),
	Hotel = Hotel2,
	
	splitList(TimeLine, 1, DayTimeLineBack1, DayTimeLineBack2),
	
	write("DTLB1" + DayTimeLineBack1), nl,
	write("DTLB1" + DayTimeLineBack2), nl,
	
	((
		DayTimeLineBack1 = [],
		OldPrice1 = 0,
		write("DLTB1 leer"), nl
		)
		;
		(
		checkEventsOnTime(Persons, TimeLine, DayStart, DayEnd, Hotel, HotelCategories, FullBudget, Return, OldPrice1),
		write("DLTB1 nicht leer"), nl
	)),
	Budget1 is FullBudget - OldPrice1,
	write(Persons + EventCategories + "PrevEvent" + "Front" + DayTimeLineBack1 + TimeLine + "1" + DayStart + DayEnd + Hotel + HotelCategories + FullBudget + Budget1),
	fillTimeLine(Persons, EventCategories, _, _, DayTimeLineBack1, TimeLine, 1, DayStart, DayEnd, Hotel, HotelCategories, Vehicle, FullBudget, Budget1, ResultTimeLine1),
		write(ResultTimeLine1), nl,
		write("Ende Tag 1"), nl,
	
	((
		DayTimeLineBack2 = [],
		OldPrice2 = 0,
		write("DLTB2 leer"), nl
		)
		;
		(
		checkEventsOnTime(Persons, TimeLine, DayStart, DayEnd, Hotel, HotelCategories, FullBudget, Return, OldPrice2),
		write("DLTB2 nicht leer"), nl
	)),
	Budget2 is FullBudget - OldPrice2,
	write(Persons + EventCategories + "PrevEvent" + "Front" + DayTimeLineBack2 + ResultTimeLine1 + "2" + DayStart + DayEnd + Hotel + HotelCategories + FullBudget + Budget2),
	fillTimeLine(Persons, EventCategories, _, _, DayTimeLineBack2, ResultTimeLine1, 2, DayStart, DayEnd, Hotel, HotelCategories, Vehicle, FullBudget, Budget2, ResultTimeLine2),
		write(ResultTimeLine2), nl,
		write("Ende Tag 2"), nl,
	
	ResultTimeLine = ResultTimeLine2.




 /*
Füllt die bestehende Timeline mit Events
fillTimeLine(Persons, PrevEvent, EventList, DayStart, DayEnd, Hotel, HotelCategorie, Budget, ResultTimeLine, Return, Price)
fillTimeLine(A, B, C, D, E, F , G, H, I, J, K)
A = Persons = [Erwachsene, Kinder]
B = EventCategories
C = PrevEvent = Vorhergehendes Event
D = TimeLine = Restliche Eventliste
E = DayStart = Startzeit des Tages
F = DayEnd = Ende des Tages
G = Hotel = Name des Hotels
H = HotelCategorie = Kategorien des Hotels = ['Kat.Name', ...]
I = Budget = Budget
J = ResultTimeLine = Timeline nach Füllung
K = Return = True oder False
L = Price = Preis der gesamten Tour

trace, fillTimeLine([1,1] ,['Bar', 'Freizeit'], ['Cinestar',1,1131,20,'Car'], [['Marinemuseum',1,804,45,'Car'],['Meeresmuseum',1,1030,100,'Car'],['Cinestar',1,1131,20,'Car']], [['Zoo',1,1230,100,'Car']], [['Marinemuseum',1,804,45,'Car'],['Meeresmuseum',1,1030,100,'Car'],['Cinestar',1,1131,20,'Car'],['Zoo',1,1230,100,'Car']], 1, 800, 2200, 'X Sterne Hotel', _, 'Car', 25000, 1294, FTL) 

trace, fillTimeLine([1,1], ['Bar', 'Freizeit'], _, _, [['Haus 8',1,1030,100,'Car'],['Zoo',1,1230,100,'Car']], [['Haus 8',1,1030,100,'Car'],['Zoo',1,1230,100,'Car']], 1, 800, 2200, 'X Sterne Hotel', _, 'Car', 200000, 200000, X).
trace, fillTimeLine([1,0], ['Bar', 'Freizeit','Bildung'], _, _, [['Meeresmuseum',1,1030,100,'Car'],['Zoo',1,1230,100,'Car']], [['Meeresmuseum',1,1030,100,'Car'],['Zoo',1,1230,100,'Car']], 1, 800, 2200, 'X Sterne Hotel', _, 'Car', 100000, 100000, X).

fillTimeLine([1,0], ['Bar', 'Freizeit','Bildung'], _, _, [['Meeresmuseum',1,900,100,'Car']], [['Meeresmuseum',1,900,100,'Car']], 1, 800, 2200, 'X Sterne Hotel', _, 'Car',100000, 100000, X).
fillTimeLine([1,0], ['Bar', 'Freizeit','Bildung'], _, _, [['Meeresmuseum',1,1500,100,'Car']], [['Meeresmuseum',1,1500,100,'Car']], 1, 800, 2200, 'X Sterne Hotel', _,'Car', 100000, 100000, X).

fillTimeLine([1,0], ['Bar', 'Freizeit','Bildung'], _, _, [], [], 1, 800, 2200, 'X Sterne Hotel', _, 'Car', 100000, 100000, X).

*/
fillTimeLine(Persons, EventCategories, PrevEvent, DayTimeLineFront, DayTimeLineBack, TimeLine, Day, DayStart, DayEnd, Hotel, HotelCategories, Vehicle, FullBudget, Budget, ResultTimeLine):-
	checkRTL(ResultTimeLine, ResultTimeLine1),
	nl, nl, write("Muss leer sein: " + ResultTimeLine), nl, nl,
	write("TimeLine: " +TimeLine), nl,
	write("PE: "+PrevEvent), nl,
	write("DTLF:"+DayTimeLineFront), nl,
	write("DTLB:"+DayTimeLineBack), nl,
	nl, nl, nl, write("Beginne mt Arbeit"), nl, nl, nl,
	((
		% PrevEvent ist nicht angegeben, damit befindet sich die Schleife am Anfang des Tages
		% DayTimeLineFront ist leer, damit befindet sich die Schleife am Anfang des Tages
		% DayTimeLineBack ist leer, damit befindet sich die Schleife am Ende des Tages
		var(PrevEvent),
		var(DayTimeLineFront),
		nonvar(DayTimeLineBack),
		DayTimeLineBack = [],
		nonvar(TimeLine),
			nl, nl, nl, write("Suche FullDay startet"), nl, nl, nl,
		calcHotelPrice(Persons, Hotel, HotelPrice),
		FullHotelPrice is HotelPrice + HotelPrice + HotelPrice,
		RestBudget = Budget - FullHotelPrice,
		((
			RestBudget > 0,
			write("Suche Event für leere Timeline"), nl,
			write(EventCategories + Persons + Budget + Hotel + DayStart + DayEnd + Vehicle), nl,
			findEventForEmptyTimeLine(EventCategories, Persons, Budget, Hotel, TimeLine, Day, DayStart, DayEnd, Vehicle, ResultDayTimeLine1),
			nl, write("RTL: " + ResultDayTimeLine1), nl,
			((
				ResultDayTimeLine1 = [],
				write("ResultTimeLine1 leer"),
				ResultTimeLine = TimeLine
				)
				;
				(
				[PrevEventNew] = ResultDayTimeLine1,
				append(TimeLine, ResultDayTimeLine1, ResultFullTimeLine1),
				write("RDTL1: " +  ResultDayTimeLine1), nl,
				write("RFTL: " +  ResultFullTimeLine1), nl,
				[EventName, _, _, _, _] = PrevEventNew,
				calcApproachForEvent(Persons, EventsName, _, Hotel, Vehicle, 0,  [_,_,_,_,ApproachPrice]),
				ReturnBudget is FullBudget - FullHotelPrice - ApproachPrice - ApproachPrice,
				write(Persons + EventCategories + PrevEventNew + ResultDayTimeLine1 + "[]" + ResultFullTimeLine1 + Day + DayStart + DayEnd + Hotel + HotelCategories + Vehicle + FullBudget + ReturnBudget + ResultFullTimeLine2),
				fillTimeLine(Persons, EventCategories, PrevEventNew, ResultDayTimeLine1, [], ResultFullTimeLine1, Day, DayStart, DayEnd, Hotel, HotelCategories, Vehicle, FullBudget, ReturnBudget, ResultFullTimeLine2),
				write("RFTL2: " +  ResultFullTimeLine2), nl,
				ResultTimeLine = ResultFullTimeLine2
			))
		
		)
		;
		(
			write("Suche Event für leete Timeline unnötig das Geld nicht reicht."), nl,
			ResultTimeLine = []
		))
	)
	;
	(
		% PrevEvent ist angegeben, damit befindet sich die Schleife mitten im Tag
		% DayTimeLineFront ist gefüllt, damit befindet sich die Schleife mitten im Tag
		% DayTimeLineBack ist leer, damit befindet sich die Schleife am Ende des Tages
		% nonvar(PrevEvent),
		[_, _, _, _, _] = PrevEvent,
		nonvar(DayTimeLineFront),
		nonvar(DayTimeLineBack),
		DayTimeLineBack = [],
		checkEventsOnTime(Persons, TimeLine, DayStart, DayEnd, Hotel, HotelCategories, FullBudget, Return, OldPrice),
			nl, nl, nl, write("Suche LastEventOfDay startet"), nl, nl, nl,
		findEventEndOfDay(Persons, EventCategories, PrevEvent, DayTimeLineFront, TimeLine, Budget, DayEnd, Hotel, PrevEventNew, FrontNew, ResultFullTimeLine, ReturnBudget),
		nl, nl, write("RFTL: " +ResultFullTimeLine), nl,
		write("bis hier"), nl,
		write(ResultTimeLine + ResultFullTimeLine), nl,
		ResultTimeLine = ResultFullTimeLine
	)
	;
	(
		
		% PrevEvent ist angegeben, damit befindet sich die Schleife mitten im Tag
		nonvar(PrevEvent),
		[_, _, _, _, _] = PrevEvent,
		nonvar(DayTimeLineFront),
		nonvar(DayTimeLineBack),
		checkEventsOnTime(Persons, TimeLine, DayStart, DayEnd, Hotel, HotelCategories, FullBudget, Return, OldPrice),
			nl, nl, nl, write("Suche zwischen 2 Events startet"), nl, nl, nl,
		% findeventbetween aufrufen
		findEventBetweenEvents(Persons, EventCategories, PrevEvent, DayTimeLineFront, DayTimeLineBack, TimeLine, Budget, PrevEventNew, FrontNew, TailNew, ResultFullTimeLine, ReturnBudget),
		% wenn daytimeline anders als resultdaytimeline 	berechne zwischem neuem event und dem darauffolgenden
		% wenn daytimeline gleich resultdaytimeline 		berechne zwischen nächstem und üernächstem
		% wenn EventTail [] leer				berechne bis zum Ende des Tages
			write("InputBudget: " + Budget), nl,
			write("InputPrice: " + OldPrice), nl,
			write("OriginalBudget: " + FullBudget), nl,
		NewPrice is FullBudget - ReturnBudget,
			write("NewPrice: " + NewPrice), nl,
		NewBudget is FullBudget - NewPrice,
			write("NewBudget: " + NewBudget), nl,
			nl, nl, nl, write("Suche zwischen 2 Events beendet"), nl, nl, nl,
		write(Persons + EventCategories + PrevEventNew + FrontNew + TailNew + ResultFullTimeLine + Day + DayStart + DayEnd + Hotel + HotelCategories + Vehicle + FullBudget + ReturnBudget + ResultFullTimeLine1),
		fillTimeLine(Persons, EventCategories, PrevEventNew, FrontNew, TailNew, ResultFullTimeLine, Day, DayStart, DayEnd, Hotel, HotelCategories, Vehicle, FullBudget, ReturnBudget, ResultFullTimeLine1),
		nl, nl, write("RFTL: " +ResultFullTimeLine1), nl,
		ResultTimeLine = ResultFullTimeLine1
	)
	;
	(
		% kein PrevEvent angegeben, damit befindet sich die Schleife am Anfang des Tages
		var(PrevEvent),
		var(DayTimeLineFront),
		nonvar(DayTimeLineBack),
		checkEventsOnTime(Persons, TimeLine, DayStart, DayEnd, Hotel, HotelCategories, FullBudget, Return, OldPrice),
			nl, nl, nl, write("Suche FirstEventOfDay startet"), nl, nl, nl,
		[EventHead|_] = DayTimeLineBack,
		RestBudget is FullBudget-OldPrice,
		write("Bis hier"), nl,
		write(Persons + EventCategories + DayTimeLineBack + TimeLine + DayStart + DayEnd + Hotel + RestBudget + ResultDayTimeLine1 + ReturnBudget), nl,
		/*
		trace, findFirstEventOfDay([1,1], ['Bar','Freizeit'] ,[['Marinemuseum',1,810,45,'Car'],['Cinestar',1,1131,20,'Car']], [['Marinemuseum',1,810,45,'Car'],['Cinestar',1,1131,20,'Car']], 800, 2200, 'X Sterne Hotel', 37702, X, Y)
		*/
		findFirstEventOfDay(Persons, EventCategories, DayTimeLineBack, TimeLine, DayStart, DayEnd, Hotel, RestBudget, ResultDayTimeLine1, ReturnBudget),
			write("Finden des Events beendet"), nl,
			write(Budget + "€"), nl,
			write(DayTimeLineBack), nl,
			write(OldPrice + "€"), nl,
			write(RestBudget + "€"), nl,
			write(ResultDayTimeLine1), nl,
			write(ReturnBudget + "€"), nl,
		NewPrice is Budget-ReturnBudget,
			write(NewPrice + "€"), nl,
			
		[Front|Tail1] = ResultDayTimeLine1,
			write(Front + Tail1), nl,
		refreshTimeLine(ResultDayTimeLine1, Day, TimeLine, ResultFullTimeLine2),
			nl, nl, nl, write("Suche FirstEventOfDay beendet"), nl, nl, nl,
		fillTimeLine(Persons, EventCategories, Front, Front, Tail1, ResultFullTimeLine2, Day, DayStart, DayEnd, Hotel, HotelCategories, Vehicle, FullBudget, ReturnBudget, ResultFullTimeLine),
		nl, nl, write("RFTL: " +ResultFullTimeLine), nl,
		ResultTimeLine = ResultFullTimeLine
	)). 


findEventForEmptyTimeLine(EventCategories, Persons, Budget, Hotel, TimeLine, Day, DayStart, DayEnd, Vehicle, ResultTimeLine):-
	findEventForFreeTime4(EventCategories, Persons, Budget, Hotel, TimeLine, DayStart, DayEnd, Vehicle, Result),
	((
		Result = [],
		ResultTimeLine = [],
		write("Kein Event gefunden")
	)
	;
	(
		event(Result, _, _, _, _, [Opening, _], [Duration]),
		findEarliest(Opening, DayStart, Earliest),
		calcApproachForEvent(Persons, _, Result, Hotel,  Vehicle, 0,  [_,_,_,ApproachTime,_]),
		((
			Earliest = DayStart,
			RealEarliest is Earliest + ApproachTime
			)
			;
			(
			RealEarliest = Earliest
		)),
		ResultEvent = [Result, Day, RealEarliest, Duration, Vehicle],
		write("RE first" + ResultEvent),
		ResultTimeLine = [ResultEvent]
	)).
	
findEventForFreeTime4(EventCategories, Persons, Budget, Hotel, TimeLine, DayStart, DayEnd, Vehicle, Result):-
	searchEventsOnCategory(EventCategories, Events),
		write(Events), nl,
	searchPossibleEventsOnDuration4(DayStart, DayEnd, Hotel, Vehicle, Events, PossibleEventsOnDuration),
		write(PossibleEventsOnDuration), nl, 
	searchPossibleEventsOnBudget4(Budget, Persons, Hotel, Vehicle, PossibleEventsOnDuration, PossibleEventsOnBudget),
		write(PossibleEventsOnBudget), nl, 
	searchPossibleEventsOnAdultChildRatio(Persons, PossibleEventsOnBudget, PossibleEventOnAdultChildRatio),
		write(PossibleEventOnAdultChildRatio), nl,
	searchPossibleEventsOnTimeline(PossibleEventOnAdultChildRatio, TimeLine, PossibleEventsOnTimeline),
		write(PossibleEventsOnTimeline), nl,
	shuffleOneResult(PossibleEventsOnTimeline, Result), nl,
		write(Result), nl
	.


/*
Prüft ob die Events in der Zeit passen
*/
searchPossibleEventsOnBudget4(_, _, _, _, [], PossibleEventsOnBudget):-
	 	write("Ende der Suche"), nl,
	PossibleEventsOnBudget = [].
			
searchPossibleEventsOnBudget4(Budget, Persons, Hotel, Vehicle, [EventsHead | EventsTail], PossibleEventsOnBudget):-
	searchPossibleEventsOnBudget4(Budget, Persons, Hotel, Vehicle, EventsTail, PossibleEventsOnBudget1),
		write("SearchOnBudget4"), nl,
	calcApproachForEvent(Persons, EventsHead, _, Hotel, Vehicle, 0,  [_,_,_,_,ApproachPrice]),
	calcEventPrice(Persons, EventsHead, EventPrice),
	FullEventPrice is EventPrice + ApproachPrice + ApproachPrice,
	(
		(
			Budget >= FullEventPrice,
			append(PossibleEventsOnBudget1, [EventsHead], PossibleEventsOnBudget2)
		)
		;
		(	
			PossibleEventsOnBudget2 = PossibleEventsOnBudget1
		)
	),
	PossibleEventsOnBudget = PossibleEventsOnBudget2.

	
/*
Prüft ob die Events in der Zeit passen
trace, fillTimeLineAllDays([1,0] ,['Bar', 'Freizeit','Bildung'], [], 800, 2200, 'X Sterne Hotel', _, 'Car', 500000, FTL) 

*/	
searchPossibleEventsOnDuration4(_, _, _, _, [], PossibleEventsOnDuration):-
		write("Suche nach Duration 4 am Ende der Liste"), nl,
	PossibleEventsOnDuration = [].
			
searchPossibleEventsOnDuration4(DayStart, DayEnd, Hotel, Vehicle, [EventsHead | EventsTail], PossibleEventsOnDuration):-
		write("Suche nach Duration 4"), nl,
	searchPossibleEventsOnDuration4(DayStart, DayEnd, Hotel, Vehicle, EventsTail, PossibleEventsOnDuration1),
	event(EventsHead, _, _, _, _, [Opening, Closing], [EventDuration]),
	findEarliestLatest(Opening, DayStart, Closing, DayEnd, Earliest, Latest),
	MaxDuration is Latest - Earliest,
	Persons = [0,0],	
		write("MaxDuration: " + MaxDuration + " EventDuration " + EventDuration), nl,
		write(Persons + EventsHead + Hotel + Vehicle), nl,
	calcApproachForEvent(Persons, _, EventsHead, Hotel,  Vehicle, 0,  [_,_,_,ApproachTime,_]),
	FullEventDuration is ApproachTime + ApproachTime + EventDuration,
	((
			MaxDuration >= FullEventDuration,
			append(PossibleEventsOnDuration1, [EventsHead], PossibleEventsOnDuration2)
		)
		;
		(
			PossibleEventsOnDuration2 = PossibleEventsOnDuration1
	)),
	PossibleEventsOnDuration = PossibleEventsOnDuration2.



/*
fillTimeLine([1,0],['Bar','Freizeit','Bildung'],['Ozeaneum',1,1332,100,'Car'],[['Ozeaneum',1,1332,100,'Car']],[['Zoo',1,1730,100,'Car']],[['Ozeaneum',1,1332,100,'Car'],['Zoo',1,1230,100,'Car']],1,800,2200,'X Sterne Hotel',_,200000,166983,X)
fillTimeLine([1,0],['Bar','Freizeit','Bildung','Einkaufen','Sport'],_,_,[['Ozeaneum',1,1332,100,'Car'],['Zoo',1,1730,100,'Car']],[['Ozeaneum',1,1332,100,'Car'],['Zoo',1,1230,100,'Car']],1,800,2200,'X Sterne Hotel',_,200000,166983,X)


fillTimeLine([1,0],['Bar','Freizeit','Bildung'],['Ozeaneum',1,1332,100,'Car'],[['Marinemuseum',1,804,45,'Car'],['Meeresmuseum',1,1030,100,'Car'],['Cinestar',1,1131,20,'Car'],['Nautineum',1,1153,60,'Car'],['Zoo',1,1230,100,'Car'],['Ozeaneum',1,1332,100,'Car']],[],[['Marinemuseum',1,804,45,'Car'],['Meeresmuseum',1,1030,100,'Car'],['Cinestar',1,1131,20,'Car'],['Nautineum',1,1153,60,'Car'],['Zoo',1,1230,100,'Car'],['Ozeaneum',1,1332,100,'Car']],1,800,2200,'X Sterne Hotel',_,200000,166983,X)
fillTimeLine([1,0],['Bar','Freizeit','Bildung'],['Fachhochschule Stralsund',1,1500,120,'Car'],[['Marinemuseum',1,804,45,'Car'],['Meeresmuseum',1,1030,100,'Car'],['Cinestar',1,1131,20,'Car'],['Nautineum',1,1153,60,'Car'],['Zoo',1,1230,100,'Car'],['Ozeaneum',1,1332,120,'Car'],['Fachhochschule Stralsund',1,1500,120,'Car']],[],[['Marinemuseum',1,804,45,'Car'],['Meeresmuseum',1,1030,100,'Car'],['Cinestar',1,1131,20,'Car'],['Nautineum',1,1153,60,'Car'],['Zoo',1,1230,100,'Car'],['Ozeaneum',1,1332,120,'Car'],['Fachhochschule Stralsund',1,1500,120,'Car']],1,800,2200,'X Sterne Hotel',_,200000,161339,X)
*/

findEventEndOfDay(Persons, EventCategories, PrevEvent, DayTimeLineFrontIn, TimeLine, Budget, DayEnd, Hotel, PrevEventNew1, FrontNew1, ResultFullTimeLine1, ReturnBudget1):-
		write("find Event End Of Day"), nl,
		write("DTLF IN" + DayTimeLineFrontIn), nl,
		((
			DayTimeLineFrontIn = [_,_,_,_,X],
			vehicle(X,_,_),
			DayTimeLineFront = [DayTimeLineFrontIn], 
			write("DayTimeLineFront wurde repariert!"), nl
			)
			;
			(
			DayTimeLineFront = DayTimeLineFrontIn, 
			write("DayTimeLineFront ist OK"), nl	
		)),
		write("DTLF" + DayTimeLineFront), nl,
	findEventForFreeTime3(TimeLine, PrevEvent, EventCategories, Persons, Budget, Hotel, DayEnd, Result),
	[PrevEventName, Day, _, _, Vehicle] = PrevEvent,
	((
		Result = [],
		FrontNew1 = DayTimeLineFront,
		ResultFullTimeLine1 = TimeLine,
		PrevEventNew1 = PrevEvent,
		ReturnBudget1 = Budget,
			write("Kein Event gefunden"), nl
		)
		;
		(
		addResultToTimeLineBetween(PrevEvent, Result, DayTimeLineFront, [], Day, Vehicle, ResultEvent, ResultDayTimeLine),
		append(DayTimeLineFront, [ResultEvent], FrontNew),
			write("Front new" + FrontNew), nl,
		write(FrontNew + Day + TimeLine + ResultFullTimeLine),	
		refreshTimeLine(FrontNew, Day, TimeLine, ResultFullTimeLine),
		calcApproachForEvent(Persons, PrevEventName, _, Hotel,  Vehicle, 0,  [_,_,_,_,OldApproachPrice]),
		calcApproachForEvent(Persons, PrevEventName, Result, _,  Vehicle, 0,  [_,_,_,_,NewApproachPrice1]),
		calcApproachForEvent(Persons, Result, _, Hotel,  Vehicle, 0,  [_,_,_,_,NewApproachPrice2]),
		calcEventPrice(Persons, Result, EventPrice),
		ReturnBudget is Budget + OldApproachPrice - NewApproachPrice1 - NewApproachPrice2 - EventPrice,
		PrevEventNew = ResultEvent,
			write("PrevEventNew: " + PrevEventNew), nl,
			write("Neues Budget: " + ReturnBudget), nl,
		write(Persons + EventCategories + PrevEventNew + FrontNew + ResultFullTimeLine + ReturnBudget + DayEnd + Hotel + PrevEventNew1 + FrontNew1 + ResultFullTimeLine1 + ReturnBudget1),
		findEventEndOfDay(Persons, EventCategories, PrevEventNew, FrontNew, ResultFullTimeLine, ReturnBudget, DayEnd, Hotel, PrevEventNew1, FrontNew1, ResultFullTimeLine1, ReturnBudget1),
		nl,nl,write("NEUE SUCHE BEENDET"), nl,nl
	)),
	write(FrontNew1), nl,
	write(ResultFullTimeLine1), nl,
	write("Kehre zurück").


findEventBetweenEvents(Persons, EventCategories, PrevEvent, DayTimeLineFrontIn, DayTimeLineBack, TimeLine, Budget, PrevEventNew, DayTimeLineFrontNew, DayTimeLineBackNew, ResultFullTimeLine, ReturnBudget):-
		nl,nl, write("find Event Between Events startet"), nl,
		write(PrevEvent), nl,
		write("DTLF IN" + DayTimeLineFrontIn), nl,
		write("DTLB" + DayTimeLineBack), nl,
		((
			DayTimeLineFrontIn = [_,_,_,_,X],
			vehicle(X,_,_),
			DayTimeLineFront = [DayTimeLineFrontIn], 
			write("DayTimeLineFront wurde repariert!"), nl
			)
			;
			(
			DayTimeLineFront = DayTimeLineFrontIn, 
			write("DayTimeLineFront ist OK"), nl	
		)),
		write("DTLF" + DayTimeLineFront), nl,
	[NextEvent|TailBack] = DayTimeLineBack,
	[PrevEventName, Day, PrevStartTime, PrevEventTime, Vehicle] = PrevEvent,
	[NextEventName, _, NextStartTime, _, Vehicle] = NextEvent,
	findEventForFreeTime2(TimeLine, EventCategories, Persons, Budget, PrevEvent, NextEvent, Result),
		write("Result: " + Result), nl,
	((
			% Wenn Result leer fahre fort mit nextevent zu übernextevent
			Result = [],
			DayTimeLineBackNew = TailBack,
			append(DayTimeLineFront, [NextEvent], DayTimeLineFrontNew),
			ResultFullTimeLine = TimeLine,
			ReturnBudget = Budget,
			PrevEventNew = NextEvent,
				write("Kein neues Event gefunden"), nl
		)
		;
		(
			% Wenn Result nicht leer fahre fort mit result zu nextevent
			DayTimeLineBackNew = DayTimeLineBack,
			nl, nl, write("DTLB:" + DayTimeLineBack), nl,
			write("PE:" + PrevEvent), nl,
			addResultToTimeLineBetween(PrevEvent, Result, DayTimeLineFront, DayTimeLineBack, Day, Vehicle, ResultEvent, ResultDayTimeLine),
				write("ResultDayTimeLine: " + ResultDayTimeLine), nl,
			append(DayTimeLineFront, [ResultEvent], DayTimeLineFrontNew),
			refreshTimeLine(ResultDayTimeLine, Day, TimeLine, ResultFullTimeLine),
				write("Neues Event gefunden"), nl,
				write("find Event Between Events beendet, berechne jetzt das Budget neu"), nl,
				write("Altes Budget: " + Budget), nl,
			calcApproachForEvent(Persons, PrevEventName, NextEventName, _, Vehicle, 0, [_,_,_,_,PriceOld1]),
			calcApproachForEvent(Persons, PrevEventName, Result, _, Vehicle, 0, [_,_,_,_,PriceNew1]),
			calcApproachForEvent(Persons, Result, NextEventName, _, Vehicle, 0, [_,_,_,_,PriceNew2]),
			calcEventPrice(Persons, Result, PriceNew3),
			BudgetNew is Budget + PriceOld1 - PriceNew1 - PriceNew2 - PriceNew3,
			ReturnBudget = BudgetNew,
			PrevEventNew = ResultEvent,
				write("Neues Budget: " + BudgetNew), nl
			
	)),
		write("Neue Eventliste: " + ResultFullTimeLine), nl,
		write("Neue Tagesliste Front: " +  DayTimeLineFrontNew), nl,
		write("Neue Tagesliste Back: " +  DayTimeLineBackNew), nl,
		write("PrevEventNew" + PrevEventNew), nl.

refreshTimeLine(ResultDayTimeLine, Day, TimeLine, ResultFullTimeLine):-
	splitList(TimeLine, 1, Day1, Day2),
	((
		Day = 1,
		append(ResultDayTimeLine, Day2, ResultFullTimeLine)
	)
	;
	(
		Day = 2,
		append(Day1, ResultDayTimeLine, ResultFullTimeLine)
	)).

	
/*
Erstellt ein Event für den Zeitraum zwischen "Anfang des Tages" bis zum ersten Event
Beispiel: findFirstEventOfDay(Persons, TimeLine, DayStart, DayEnd, Hotel, Budget, ResultTimeLine, Return, Price)
*/
findFirstEventOfDay(Persons, EventCategories, DayTimeLine, TimeLine, DayStart, DayEnd, Hotel, RestBudget, ResultDayTimeLine, ReturnBudget):-
	[EventHead|_] = TimeLine,
	[FirstEvent, Day, FirstStartTime, _, Vehicle] = EventHead,
	calcApproachForEvent(Persons, _, FirstEvent, Hotel, Vehicle, FirstStartTime, [_,_,_,NextRealStartTime,_]),
	(
		NextRealStartTime > DayStart,
		findEventForFreeTime(TimeLine, EventHead, EventCategories, Persons, RestBudget, Hotel, Vehicle, DayStart, DayEnd, NextRealStartTime, Result),
		((
			Result = [],
			ReturnBudget = RestBudget,
			ResultDayTimeLine = DayTimeLine,
				write("Kein passendes Event gefunden"), nl
		)
		;
		(
				write("Füge Result zu Timeline"), nl,
				write(Result), nl,
				write(DayTimeLine), nl,
			addResultToTimeLine(Result, DayTimeLine, DayStart, Day, Vehicle, Hotel, ResultDayTimeLine1),
				write(ResultDayTimeLine1), nl,
			ResultDayTimeLine = ResultDayTimeLine1,
			% Preis für das neues Event und Anfahrt berechnen und von Budget abziehen
			calcApproachForEvent(Persons, _, Result, Hotel, Vehicle, FirstStartTime, [_,_,_,_,PriceNew1]),
			calcEventPrice(Persons, Result, PriceNew2),
			% Preis für alte Anfahrt zum nächsten Event Berechnen und zu Budget hinfügen
			calcApproachForEvent(Persons, _, FirstEvent, Hotel, Vehicle, FirstStartTime, [_,_,_,_,PriceOld1]),
			% Preis für neue Anfahrt zum nächsten Event Berechnen und von Budget abziehen
			calcApproachForEvent(Persons, Result, FirstEvent, Hotel, Vehicle, FirstStartTime, [_,_,_,_,PriceNew3]),	
				write(PriceNew1 + PriceNew2 + PriceNew3 + PriceOld1), nl,
				write(RestBudget), nl,
			NewEventPrice is PriceNew1 + PriceNew2 + PriceNew3 - PriceOld1,
				write(NewEventPrice), nl,
			NewBudget is (RestBudget - NewEventPrice),
				write(NewBudget), nl,
			ReturnBudget = NewBudget 
		))
	).

addResultToTimeLineBetween(PrevEvent, Result, DayTimeLineFrontIn, DayTimeLineBack, Day, Vehicle, ResultEvent, ResultDayTimeLine):-
	nl,nl, write("Starte mit AddResult"), nl,
		write("PE: "+ PrevEvent), nl,
		write("R: " + Result), nl,
		write("DTLF IN" + DayTimeLineFrontIn), nl,
		write("DTLB" + DayTimeLineBack), nl,
		((
			DayTimeLineFrontIn = [_,_,_,_,X],
			vehicle(X,_,_),
			DayTimeLineFront = [DayTimeLineFrontIn], 
			write("DayTimeLineFront wurde repariert!"), nl
			)
			;
			(
			DayTimeLineFront = DayTimeLineFrontIn, 
			write("DayTimeLineFront ist OK"), nl	
		)),
		write("DTLF" + DayTimeLineFront), nl,
	[PrevEventName, Day, PrevStartTime, PrevEventTime, Vehicle] = PrevEvent,
	calcApproachForEvent([0,0], PrevEventName, Result, _,  Vehicle, 0,  [_,_,ArrivalTime1,_,_]),
	event(Result, _, _, _, _, [Opening, _], [Duration]),
	From = PrevStartTime + PrevEventTime,
	findEarliest(Opening, From, Earliest),
	((
		Earliest = From,
		RealEarliest is Earliest + ArrivalTime1
		)
		;
		(
		Earliest = Opening,
		RealEarliest is Earliest
	)),
	[Result, Day, RealEarliest, Duration, Vehicle] = ResultEvent,
		write("RE: " + ResultEvent), nl,
		append(DayTimeLineFront, [ResultEvent], ResultDayTimeLineFront),
	append(ResultDayTimeLineFront, DayTimeLineBack, ResultDayTimeLine1),
	ResultDayTimeLine = ResultDayTimeLine1,
		write("RDTLF:" + ResultDayTimeLineFront), nl,
		write("RDTL:" + ResultDayTimeLine), nl.


addResultToTimeLine([], TimeLine, _, _, _, _, ResultTimeLine):-
	ResultTimeLine = TimeLine.
	
addResultToTimeLine(Event, TimeLine, FromTime, Day, Vehicle, Hotel, ResultTimeLine):-
	calcApproachForEvent([0,0], _, Event, Hotel,  Vehicle, 0,  [_,_,ArrivalTime,_,_]),
	event(Event, _, _, _, _, [Opening, _], [Duration]),
	findEarliest(Opening, FromTime, Earliest),
	((
		Earliest = FromTime,
		RealEarliest is Earliest + ArrivalTime
		)
		;
		(
		Earliest = Opening,
		RealEarliest is Earliest
	)),
	[[Event, Day, RealEarliest, Duration, Vehicle]] = ResultEvent,
	append(ResultEvent, TimeLine, ResultTimeLine1),
	ResultTimeLine = ResultTimeLine1.
	

findEventForFreeTime3(TimeLine, PrevEvent, EventCategories, Persons, Budget, Hotel, DayEnd, Result):-
	searchEventsOnCategory(EventCategories, Events),
		write(Events), nl,
	searchPossibleEventsOnDuration3(Persons, PrevEvent, Hotel, DayEnd, Events, PossibleEventsOnDuration),
		write(PossibleEventsOnDuration), nl, 
	searchPossibleEventsOnBudget3(Budget, Persons, PrevEent, Hotel, PossibleEventsOnDuration, PossibleEventsOnBudget),
		write(PossibleEventsOnBudget), nl, 
	searchPossibleEventsOnAdultChildRatio(Persons, PossibleEventsOnBudget, PossibleEventOnAdultChildRatio),
		write(PossibleEventOnAdultChildRatio), nl,
	searchPossibleEventsOnTimeline(PossibleEventOnAdultChildRatio, TimeLine, PossibleEventsOnTimeline),
		write(PossibleEventsOnTimeline), nl,
	shuffleOneResult(PossibleEventsOnTimeline, Result), nl,
		write(Result), nl
	.

/*
Prüft ob die Events in der Zeit passen
*/
searchPossibleEventsOnBudget3(_, _, _, _, [], PossibleEventsOnBudget):-
	% write("Ende der Suche"), nl,
	PossibleEventsOnBudget = [].
			
searchPossibleEventsOnBudget3(Budget, Persons, PrevEent, Hotel, [EventsHead | EventsTail], PossibleEventsOnBudget):-
	searchPossibleEventsOnBudget3(Budget, Persons,  PrevEvent, Hotel, EventsTail, PossibleEventsOnBudget1),
			write("SearchOnBudget3"), nl,
	% write(EventsHead), nl,
	[PrevEventName, Day, PrevEventStartTime, PrevEventTime, Vehicle] = PrevEvent,
	
	% write("Kalkuliere weiter"), nl,
	calcApproachForEvent(Persons, PrevEventName, _, Hotel,  Vehicle, 0,  [_,_,_,_,OldApproachPrice]),
	calcApproachForEvent(Persons, PrevEventName, EventsHead, _,  Vehicle, 0,  [_,_,_,_,NewApproachPrice1]),
	calcApproachForEvent(Persons, EventsHead, _, Hotel,  Vehicle, 0,  [_,_,_,_,NewApproachPrice2]),
	calcEventPrice(Persons, EventsHead, EventPrice),
	(
		(
			% write("Prüfe auf Budget dass korrekt"), nl,
			FullEventPrice is EventPrice + NewApproachPrice1 + NewApproachPrice2 - OldApproachPrice,
			Budget >= FullEventPrice,
			append(PossibleEventsOnBudget1, [EventsHead], PossibleEventsOnBudget2)
		)
		;
		(	
			% write("Budget reicht nicht"), nl,
			PossibleEventsOnBudget2 = PossibleEventsOnBudget1
		)
	),
	PossibleEventsOnBudget = PossibleEventsOnBudget2.

	
/*
Prüft ob die Events in der Zeit passen
*/	
searchPossibleEventsOnDuration3(_, _, _, _, [], PossibleEventsOnDuration):-
		write("Suche nach Duration 3 am Ende der Liste"), nl,
	PossibleEventsOnDuration = [].
			
searchPossibleEventsOnDuration3(Persons, PrevEvent, Hotel, DayEnd, [EventsHead | EventsTail], PossibleEventsOnDuration):-
	write("Suche nach Duration 3"), nl,
	searchPossibleEventsOnDuration3(Persons, PrevEvent,  Hotel, DayEnd, EventsTail, PossibleEventsOnDuration1),
	[PrevEventName, _, PrevEventStartTime, PrevEventTime, Vehicle] = PrevEvent,
	Earliest is PrevEventStartTime + PrevEventTime,
	event(EventsHead, _, _, _, _, [Opening, Closing], [EventDuration]),
	findLatest(Closing, DayEnd, Latest),
	MaxDuration is Latest - Earliest,
	write("MaxDuration: " + MaxDuration + " EventDuration " + EventDuration), nl,
	write(Persons + PrevEventName + EventsHead +   Vehicle ), nl,
	calcApproachForEvent(Persons, PrevEventName, EventsHead, _,  Vehicle, 0,  [_,_,_,NewApproachTime1,_]),
	calcApproachForEvent(Persons, EventsHead, _, Hotel,  Vehicle, 0,  [_,_,_,NewApproachTime2,_]),
	((
			FullEventDuration is EventDuration + NewApproachTime1 + NewApproachTime2,
			MaxDuration >= FullEventDuration,
			append(PossibleEventsOnDuration1, [EventsHead], PossibleEventsOnDuration2)
		)
		;
		(
			PossibleEventsOnDuration2 = PossibleEventsOnDuration1
	)),
	PossibleEventsOnDuration = PossibleEventsOnDuration2.

findEventForFreeTime2(TimeLine, EventCategories, Persons, Budget, PrevEvent, NextEvent, Result):-
	searchEventsOnCategory(EventCategories, Events),
		write("Event: " +Events), nl,
	searchPossibleEventsOnDuration2(PrevEvent, NextEvent, Events, PossibleEventsOnDuration),
		write(PossibleEventsOnDuration), nl, 
		write(Budget + Persons + PossibleEventsOnDuration), nl,
	searchPossibleEventsOnBudget2(Budget, Persons, PrevEvent, NextEvent, PossibleEventsOnDuration, PossibleEventsOnBudget),
		write(PossibleEventsOnBudget), nl, 
	searchPossibleEventsOnAdultChildRatio(Persons, PossibleEventsOnBudget, PossibleEventOnAdultChildRatio),
		write(PossibleEventOnAdultChildRatio), nl,
	searchPossibleEventsOnTimeline(PossibleEventOnAdultChildRatio, TimeLine, PossibleEventsOnTimeline),
		write(PossibleEventsOnTimeline), nl,
	shuffleOneResult(PossibleEventsOnTimeline, Result), nl,
		write(Result), nl
	.
	
findEventForFreeTime(TimeLine, NextEvent, EventCategories, Persons, Budget, Hotel, Vehicle, DayStart, DayEnd, NextRealStartTime, Result):-
	searchEventsOnCategory(EventCategories, Events),
		write(Events), nl,
	searchPossibleEventsOnDuration(DayStart, NextRealStartTime, Hotel, Vehicle, Events, PossibleEventsOnDuration),
		write(PossibleEventsOnDuration), nl, 
		write(Budget + Persons + Vehicle + Hotel + PossibleEventsOnDuration), nl,
	searchPossibleEventsOnBudget(Budget, Persons, Vehicle, NextEvent, Hotel, PossibleEventsOnDuration, PossibleEventsOnBudget),
		write(PossibleEventsOnBudget), nl, 
	searchPossibleEventsOnAdultChildRatio(Persons, PossibleEventsOnBudget, PossibleEventOnAdultChildRatio),
		write(PossibleEventOnAdultChildRatio), nl,
	searchPossibleEventsOnTimeline(PossibleEventOnAdultChildRatio, TimeLine, PossibleEventsOnTimeline),
		write(PossibleEventsOnTimeline), nl,
	shuffleOneResult(PossibleEventsOnTimeline, Result), nl,
		write(Result), nl
	.


/*
Prüft ob die Events in der Zeit passen
*/
searchPossibleEventsOnTimeline([], _, PossibleEventsOnTimeline):-
	% write("Ende der Suche"), nl,
	PossibleEventsOnTimeline = [].
			
searchPossibleEventsOnTimeline([EventsHead|EventsTail], Timeline, PossibleEventsOnTimeline):-
	searchPossibleEventsOnTimeline(EventsTail, Timeline, PossibleEventsOnTimeline1),
	write("Suche zu " + EventsHead), nl,	
	searchEventInTimeLine(EventsHead, Timeline, Result),
	(
		(
			%write("Event noch nicht vorhanden"), nl,
			Result = 'false',
			append(PossibleEventsOnTimeline1, [EventsHead], PossibleEventsOnTimeline2)
		)
		;
		(	
			write("Event bereits vorhanden"), nl,
			Result = 'true',
			PossibleEventsOnTimeline2 = PossibleEventsOnTimeline1
		)
	),
	PossibleEventsOnTimeline = PossibleEventsOnTimeline2.

searchEventInTimeLine(_, [], Result):-
	Result = 'false'.
searchEventInTimeLine(Event, [Head|Tail], Result):-
	[EventHead, _, _, _, _] = Head,
	write("Suche für " + Event + EventHead), nl,
	((
		write("Vergleiche " + Event + EventHead), nl,
		Event = EventHead,
		Result = 'true',
		write("Ist gleich"), nl
	)
	;
	(
		searchEventInTimeLine(Event, Tail, Result)
	)).

shuffleOneResult([], X):-
	X = [],
		write("ShuffleOneResult leer"), nl.

shuffleOneResult(Events, Result) :-
		write("ShullfeOneResult Liste"), nl,
        length(Events, Length),
        random(0, Length, Index),
       	nth0(Index, Events, Result).
	
/*
Prüft ob die Events in der Zeit passen
searchPossibleEventOnAdultChildRatio([2,3], ['Hansedom','Meeresmuseum','Zoo','Haus 8'], PossibleEventOnAdultChildRatio)
searchPossibleEventOnAdultChildRatio([2,0], ['Hansedom','Meeresmuseum','Zoo','Haus 8'], PossibleEventOnAdultChildRatio)
compareCategories(['Zoo','Kneipe'], ['Tiere','Museum'],  Result)
compareCategories([['Hansedom',['Tiere','Museum']]], ['Tiere','Museum'],  Result)
compareCategories([['Haus 8',['Bar','Kneipe']]], ['Tiere','Museum'],  Result)
*/
searchPossibleEventsOnAdultChildRatio(_, [], PossibleEventOnAdultChildRatio):-
		% write("Ende der Suche"), nl,
	PossibleEventOnAdultChildRatio = [].
			
searchPossibleEventsOnAdultChildRatio(Persons, [EventsHead|EventsTail], PossibleEventOnAdultChildRatio):-
	searchPossibleEventsOnAdultChildRatio(Persons, EventsTail, PossibleEventOnAdultChildRatio1),
		% write("Prüfe"), nl,
	[_,Children] = Persons,
	event(EventsHead, _, EventCategories, _, _, _, _),
	(
		(
			Children =\= 0,
			childCategories(ChildList),
			compareCategories([[EventsHead, EventCategories]], ChildList,  Result)
		)
		;
		(
			Children = 0,
			adultCategories(AdultList),
			compareCategories([[EventsHead, EventCategories]], AdultList,  Result)
		)
		
	),
	(
		(
			Result = [],
			PossibleEventOnAdultChildRatio2 = PossibleEventOnAdultChildRatio1
		)
		;
		(
			append(PossibleEventOnAdultChildRatio1, Result, PossibleEventOnAdultChildRatio2)
		)
	),
	PossibleEventOnAdultChildRatio = PossibleEventOnAdultChildRatio2.



/*
Prüft ob die Events im Budget passen
*/
searchPossibleEventsOnBudget(_, _, _, _, _, [], PossibleEventsOnBudget):-
	% write("Ende der Suche"), nl,
	PossibleEventsOnBudget = [].
			
searchPossibleEventsOnBudget(Budget, Persons, Vehicle, NextEvent, Hotel, [EventsHead|EventsTail], PossibleEventsOnBudget):-
	% write("SearchOnBudget"), nl,
	% write(EventsHead), nl,
	searchPossibleEventsOnBudget(Budget, Persons, Vehicle, NextEvent, Hotel, EventsTail, PossibleEventsOnBudget1),
	% write("Kalkuliere weiter"), nl,
	[NextEventName, _, _, _, _] = NextEvent,
	calcApproachForEvent(Persons, _, EventsHead, Hotel,  Vehicle, 0,  [_,_,_,_,PriceNew1]),
	calcApproachForEvent(Persons, EventsHead, NextEventName, _,  Vehicle, 0,  [_,_,_,_,PriceNew2]),
	calcApproachForEvent(Persons, _, NextEventName, Hotel,  Vehicle, 0,  [_,_,_,_,PriceOld1]),
	calcEventPrice(Persons, EventsHead, EventPrice),
		nl, write("Preise für Berechnung: " + EventPrice + PriceNew1 + PriceNew2 + PriceOld1), nl,
	(
		(
				write("Prüfe auf Budget dass korrekt"), nl,
			FullEventPrice is EventPrice + PriceNew1 + PriceNew2 - PriceOld1,
				nl, write("Budget für neues Event: " + Budget), nl,
				nl, write("FullEventPrice: " + FullEventPrice), nl,
			Budget >= FullEventPrice,
			append(PossibleEventsOnBudget1, [EventsHead], PossibleEventsOnBudget2),
				write("Budget ist ausreichend"), nl
		)
		;
		(	
			write("Budget reicht nicht"), nl,
			PossibleEventsOnBudget2 = PossibleEventsOnBudget1
		)
	),
	PossibleEventsOnBudget = PossibleEventsOnBudget2.

/*
Prüft ob die Events in der Zeit passen
*/
searchPossibleEventsOnBudget2(_, _, _, _, [], PossibleEventsOnBudget):-
	% write("Ende der Suche"), nl,
	PossibleEventsOnBudget = [].
			
searchPossibleEventsOnBudget2(Budget, Persons, PrevEvent, NextEvent, [EventsHead | EventsTail], PossibleEventsOnBudget):-
	searchPossibleEventsOnBudget2(Budget, Persons,  PrevEvent, NextEvent, EventsTail, PossibleEventsOnBudget1),
			write("SearchOnBudget2"), nl,
	% write(EventsHead), nl,
	[PrevEventName, Day, PrevEventStartTime, PrevEventTime, Vehicle] = PrevEvent,
	[NextEventName, _, NextEventStartTime, _, _] = NextEvent,
	
	% write("Kalkuliere weiter"), nl,
	calcApproachForEvent(Persons, PrevEventName, NextEventName, _,  Vehicle, 0,  [_,_,_,_,OldApproachPrice]),
	calcApproachForEvent(Persons, PrevEventName, EventsHead, _,  Vehicle, 0,  [_,_,_,_,NewApproachPrice1]),
	calcApproachForEvent(Persons, EventsHead, NextEventName, _,  Vehicle, 0,  [_,_,_,_,NewApproachPrice2]),
	calcEventPrice(Persons, EventsHead, EventPrice),
	(
		(
			% write("Prüfe auf Budget dass korrekt"), nl,
			FullEventPrice is EventPrice + NewApproachPrice1 + NewApproachPrice2 - OldApproachPrice,
			Budget >= FullEventPrice,
			append(PossibleEventsOnBudget1, [EventsHead], PossibleEventsOnBudget2)
		)
		;
		(	
			% write("Budget reicht nicht"), nl,
			PossibleEventsOnBudget2 = PossibleEventsOnBudget1
		)
	),
	PossibleEventsOnBudget = PossibleEventsOnBudget2.

/*
Prüft ob die Events in der Zeit passen
*/
searchPossibleEventsOnDuration(_, _, _, _, [], PossibleEventsOnDuration):-
	PossibleEventsOnDuration = [].
			
searchPossibleEventsOnDuration(From, To, Hotel, FirstVehicle, [EventsHead|EventsTail], PossibleEventsOnDuration):-
	searchPossibleEventsOnDuration(From, To, Hotel, FirstVehicle, EventsTail, PossibleEventsOnDuration1),
	calcApproachForEvent([0,0], _, EventsHead, Hotel,  FirstVehicle, 0,  [_,_,ApproachTime,_,_]),
	event(EventsHead, _, _, _, _, [Opening, Closing], [EventDuration]),
	findEarliestLatest(Opening, From, Closing, To, Earliest, Latest),
	MaxDuration is Latest - Earliest,
	write("MaxDuration: " + MaxDuration + " EventDuration " + EventDuration), nl,
	((
			FullEventDuration is EventDuration + ApproachTime,
			MaxDuration >= FullEventDuration,
			append(PossibleEventsOnDuration1, [EventsHead], PossibleEventsOnDuration2)
		)
		;
		(
			PossibleEventsOnDuration2 = PossibleEventsOnDuration1
	)),
	PossibleEventsOnDuration = PossibleEventsOnDuration2.


/*
Prüft ob die Events in der Zeit passen
*/	
searchPossibleEventsOnDuration2(_, _, [], PossibleEventsOnDuration):-
		write("Suche nach Duration 2 am Ende der Liste"), nl,
	PossibleEventsOnDuration = [].
			
searchPossibleEventsOnDuration2(PrevEvent, NextEvent, [EventsHead | EventsTail], PossibleEventsOnDuration):-
	write("Suche nach Duration 2"), nl,
	searchPossibleEventsOnDuration2(PrevEvent, NextEvent, EventsTail, PossibleEventsOnDuration1),
	[PrevEventName, _, PrevEventStartTime, PrevEventTime, Vehicle] = PrevEvent,
	PrevEventEndTime is PrevEventStartTime + PrevEventTime,
	[NextEventName, _, NextEventStartTime, _, _] = NextEvent,
	calcApproachForEvent([0,0], PrevEventName, NextEventName, _,  Vehicle, 0,  [_,_,OldApproachTime,_,_]),
	calcApproachForEvent([0,0], PrevEventName, EventsHead, _,  Vehicle, 0,  [_,_,NewApproachTime1,_,_]),
	calcApproachForEvent([0,0], EventsHead, NextEventName, _,  Vehicle, 0,  [_,_,NewApproachTime2,_,_]),
	RealNextEventStartTime is NewApproachTime2 + NextEventStartTime,
	event(EventsHead, _, _, _, _, [Opening, Closing], [EventDuration]),
	findEarliestLatest(Opening, PrevEventEndTime, Closing, RealNextEventStartTime, Earliest, Latest),
	MaxDuration is Latest - Earliest,
	write("MaxDuration: " + MaxDuration + " EventDuration " + EventDuration), nl,
	((
			FullEventDuration is EventDuration - OldApproachTime + NewApproachTime1 + NewApproachTime2,
			MaxDuration >= FullEventDuration,
			append(PossibleEventsOnDuration1, [EventsHead], PossibleEventsOnDuration2)
		)
		;
		(
			PossibleEventsOnDuration2 = PossibleEventsOnDuration1
	)),
	PossibleEventsOnDuration = PossibleEventsOnDuration2.
	
	

findEarliestLatest(Opening, From, Closing, To, Earliest, Latest):-
	write("From: " + From + " To: " + To + " Opening: " + Opening + " Closing: " + Closing), nl,
	findEarliest(Opening, From, Earliest1),
	Earliest = Earliest1,
	findLatest(Closing, To, Latest1),
	Latest = Latest1.

findLatest(Closing, To, Latest):-
	(( 	
		Closing >= To,
		Latest = To
		)
		;
		(
		Closing < To,
		Latest = Closing
	)).
	
findEarliest(Opening, From, Earliest):-
	(( 	
		Opening >= From,
		Earliest = Opening
		)
		;
		(
		Opening < From,
		Earliest = From
	)).	
/*
Prüft ob RTL leer oder nicht und gibt leere Liste zurück
Beispiel1: checkRTL(ResultTimeLine, ResultTimeLine1)
Beispiel2: checkRTL([], ResultTimeLine1)
*/
checkRTL(ResultTimeLine, ResultTimeLine1):-
	((
		% RTL ist angegeben
		nonvar(ResultTimeLine)
	)
	;
	(
		% RTL ist angegeben
		var(ResultTimeLine),
		ResultTimeLine1 = []
	)).


	