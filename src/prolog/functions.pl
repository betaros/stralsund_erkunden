:- use_module(events).
:- use_module(java_connection_functions).

:- use_module(library(lists)).
:- use_module(library(random)).


/*----------------------------------------------------------------------------------------------*/

/*
* Konstanten fuer die Berechnungen der Entfernung
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

Beispiel:
compareCategories([['Hansedom',['Tiere','Museum']],['Haus 8',['Bar','Kneipe']]], ['Tiere','Museum', 'Bar'],  Result)
compareCategories([['Haus 8',['Bar','Kneipe']]], ['Tiere','Museum'],  Result)
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
* Prüft für alle Events der Liste ob sie einzeln nicht zu teuer sind und gibt die zurück die
* preislich in das Budget nicht übersteigen
* Persons = [Erwachsene, Kinder]
* Budget = Preis in Cent
* MyEvents = [Event_01, Event_02 .., Event_n]
* Event_n = [Eventname, [Long,Lat], [Event-Kategorien],[Food-Kategorien], [Preis-Erwachsen, Preis-Ermäßigt], [Öffnung, Schließung], [Dauer]
* ValidEvents = bleibt leer -> wird zur Liste der Validen Events
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
* searchUsefulEvents
* sucht alle Events welche zu den angegebenen Kategorien passen und das Budget nicht übersteigen
* Persons = [Erwachsene, Kinder]
* Budget = Preis in Cent
* Categories = Kategorien der Events
*/
searchUsefulEvents(Persons, Budget, Categories, UsefulEvents):-
	searchEventsOnCategory(Categories, Events1),
	checkEventsForBudget(Persons,Budget,Events1,ValidEvents),
	UsefulEvents = ValidEvents.


/*----------------------------------------------------------------------------------------------*/


/*
checkEventsOnTime Überprüft die gesamte Timeline
checkEventsOnTime(Persons,[Eventlist] ,DayStart, DayEnd, Hotel, HotelCategorie, Budget, Return, Price):-
Persons = [Erwachsene, Kinder]
Budget = Preis in Cent
EventList = [Event_01, Event_02 .., Event_n]
Event_n = [Eventname, [Long,Lat], [Event-Kategorien],[Food-Kategorien], [Preis-Erwachsen, Preis-Ermäßigt], [Öffnung, Schließung], [Dauer]
Daystart = Startuhrzeit des Tages
DayEnd = Enduhrzeit des Tages
Hotel = Name des Hotels
HotelCategorie = Kategorie/nwunsch des Nutzers (wird nur beachtet, wenn kein Hotel angegeben)
Budget = Maximales Budget
Return = Rückgabewert wird true oder false
Price = Gesamtpreis der Tour

Beispiel:
trace,
checkEventsOnTime([1,2],
[
['Cinestar Kino', 1, 660, 40, 'Car'],
['Strandbad', 1, 1020, 60, 'Car'],
['Torschliesserhaus', 2, 720, 50, 'Car'],
['Museumsspeicher', 1, 920, 50, 'Car'],
['Störtebeker Braumanufaktur', 2, 950, 50, 'Car'],
['Hansedom', 2, 1200, 60, 'Car'],
['Strelapark', 1, 720, 100, 'Car'],
['Katharinenkloster', 2, 800, 100, 'Car']
],
500, 1380, 'Hiddenseer Hotel', _,
100000, Return, Price).

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
checkTimeLine Überprüft die gesamte Timeline
Persons = [Erwachsene, Kinder]
Budget = Preis in Cent
EventList = [Event_01, Event_02 .., Event_n]
Event_n = [Eventname, [Long,Lat], [Event-Kategorien],[Food-Kategorien], [Preis-Erwachsen, Preis-Ermäßigt], [Öffnung, Schließung], [Dauer]
Daystart = Startuhrzeit des Tages
DayEnd = Enduhrzeit des Tages
Hotel = Name des Hotels
Return = Rückgabewert wird true oder false
Price = Gesamtpreis der Tour

Beispiel positiv an einem Tag:
checkTimeLine([1,2], [['Ozeaneum',1,1030,40,'Car'],['Rathaus',1,1100,30,'Car']],800, 1320, 'Hiddenseer Hotel', 1000000, Return, Price).
Beispiel positiv an 2 Tagen:
checkTimeLine([1,2],[['Ozeaneum',1,1030,40,'Car'],['Rathaus',2,1100,30,'Car']],800, 1320, 'Hiddenseer Hotel', 100000, Return, Price).
Beispiel positiv an 2 Tagen:
checkTimeLine([1,2],[['Ozeaneum',1,1030,40,'Car'],['Ozeaneum',2,1030,40,'Car'],['Rathaus',2,1100,30,'Car']], 800, 2200, 'Hiddenseer Hotel', 100000, Return, Price).

Diese Beispiele funktionieren nur mit den TestObjekten, diese müssen zuvor einkommentiert und die originaldaten auskommentiert werden
Beispiel negativ an einem Tag:
checkTimeLine([1,2],[['Haus 8',1,1030,100,'Car'],['Zoo',1,1130,100,'Car']],800, 2200, '1 Sterne Hotel', 100000, Return, Price).
Beispiel negativ an einem Tag:
checkTimeLine([1,2], [['Haus 8',1,830,100,'Car'],['Zoo',1,1230,100,'Car']],800, 2200, 'Hiddenseer Hotel', 1000000, Return, Price).
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
checkTimeLine Variante 1
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
checkTimeLine Variante 2
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
checkTimeLine Variante 3
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
Berechnet die Anfahrt zum Event
calcApproachForEvent(Persons, PreviousEvent, ThisEvent, Hotel, Vehicle, EventTime, Approach):-
PreviousEvent = vorheriges Event
Persons = [Erwachsene, Kinder]
ThisEvent = Event zu dem die Anfahrt berechnet wird
Hotel = Das Hotel des Nutzers
Vehicle = Fahrzeug
EventTime = Startzeit des ThisEvent
Arrivel wird zurückgegeben (Arrival = ('Anfahrt', Vehicle, Zeit in Minuten, Startzeit))
Wenn PreviousEvent leer, dann wird vom Hotel berechnet
Wenn ThisEvent leer, dann wird zum Hotel berechnet

Beispiel:
calcApproachForEvent([1,1], 'Rathaus', 'Ozeaneum', 'Wyndham Stralsund', 'Car', 800, Approach).
calcApproachForEvent([1,1], _, 'Ozeaneum', 'Wyndham Stralsund', 'Car', 800, Approach).
calcApproachForEvent([1,1], 'Rathaus', _, 'Wyndham Stralsund', 'Car', 800, Approach).

*/
calcApproachForEvent([AdultCount,ReducedCount], PreviousEvent, ThisEvent, Hotel, Vehicle, EventTime, Approach):-
	((
		nonvar(PreviousEvent),
		Point1 = PreviousEvent
	)
	;
	(
		var(PreviousEvent),
		Point1 = Hotel
	)),
	((
		nonvar(ThisEvent),
		Point2 = ThisEvent
	)
	;
	(
		var(ThisEvent),
		Point2 = Hotel
	)),
	calcDistance(Point1, Point2, Distance),
	vehicle(Vehicle, [AdultPrice,ReducedPrice], Speed),
	ArrivalTime is ceiling(Distance/Speed*60),
	StartTime is EventTime - ArrivalTime,
	Price is (AdultCount*AdultPrice)+(ReducedCount*ReducedPrice),
	Approach = ['Anfahrt', Vehicle, ArrivalTime, StartTime, Price].


/*----------------------------------------------------------------------------------------------*/


/*calcEventPrice
Berechnet Preis für Event
calcEventPrice(Persons, Event, Price),
Persons = [Erwachsene, Kinder]
EventName = Name des Events
Price = Rückgabewert des Preises

Beispiel:
calcEventPrice([1,1], 'Hansedom', Price)
*/
calcEventPrice([AdultCount,ReducedCount], EventName, Price):-
	event(EventName,_,_,_,[AdultPrice,ReducedPrice],_,_),
	Price is (AdultCount*AdultPrice) + (ReducedCount*ReducedPrice),
		write("Preis für " + EventName + " ist: " + Price), nl.


/*
Berechnet den Preis für das Hotel
In Abhängigkeit der Personen und der benötigten Doppelzimmer
Persons = [Erwachsene, Kinder]
HotelName = Name des Hotels
Price = Rückgabewert des Preises

Beispiel:
calcHotelPrice([1,1], 'Hiddenseer Hotel', Price)
*/
calcHotelPrice(Persons, Hotel, Price):-
	hotel(Hotel,_,PricePerRoom,_),
	[Adult,Reduced] = Persons,
	PersonsCount = Adult + Reduced,
	Rooms is ceiling(PersonsCount/2),
	HotelPrice is Rooms * PricePerRoom,
		write("Preis für Hotel für eine Nacht: " + HotelPrice), nl,
	Price is HotelPrice.


/*----------------------------------------------------------------------------------------------*/


/*
Findet Hotels zur angegebenen Categorie
findHotelsForTrip(HotelCategorie, Hotel, Budget, Persons)
HotelCategorie = Liste der Kategorien
Hotel = Rückgabe des Hotels
Budget = Das maximale Bugdet für das Hotel (in cent)
Persons = [Erwachsene, Kinder]

Beispiel:
findHotelsForTrip([1,4], Hotel, 20000, [1,2])
findHotelsForTrip([3], Hotel, 10, [2,2])
trace, findHotelsForTrip([4], Hotel, 10000000, [2,0])

*/
findHotelsForTrip(HotelCategorie, Hotel, Budget, Persons):-
	findall([X,V], hotel(X,_,_,V), List),
	compareCategoriesAndBudget(List,HotelCategorie,Budget,Persons,Hotels),
	Hotels = [Hotel|_].

/*

Vergleicht die Liste der Kategorien & Budget mit der übergebenen Liste
compareCategoriesAndBudget(HotelList,Categories,Budget,Persons,List1)
HotelList = Liste der Hotels
Categories = Liste der Categorien der Hotels
Budget = maximales Budget für das Hotel
Persins = [Erw., Kinder]
List1 = Rückgabe der Hotels / des Hotels, da das erste aus der Liste genommen wird
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

/*----------------------------------------------------------------------------------------------*/

/*
Findet Restaurant passend zur Gruppe und den angegebenen Zeiten
findRestaurant(FoodCategories, Restaurant,[Starting,Ending])
FoodCategories = Kategorien der Restaurants
Restaurant = Rückgabewert
[Starting, Ending] = Zeitrahmen

findRestaurant(['Fast-Food'], Restaurant, [1200,1500]).
*/
findRestaurant(FoodCategories, Restaurant,[Starting,Ending]):-

	findall([Name,Cat,[Start,End]], event(Name,_,_,Cat,_,[Start,End],_), List),
	compareRestaurants(List,FoodCategories,[Starting,Ending],Restaurants),
	Restaurants = [Restaurant|_].

/*
Prüft die Liste der Restaurants auf die Stimmigkeit mit den angegebenen Essenskategorien und den Uhrzeiten
*/
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


/*----------------------------------------------------------------------------------------------*/

/*
Prüft die Öffnungszeiten
Gibt true oder false zurück
ThisEventName = Name des Events
EventStartTime = Uhrzeit wann das Event starten soll
EventTime = Dauer des Events

Beispiel:
checkBussinesHours('Hansedom', 1100, 100)

*/
checkBussinesHours(ThisEventName, EventStartTime, EventTime):-
	event(ThisEventName,_,_,_,_,[Opening, Closing],_),
	EventStartTime >= Opening,
	EventEndTime is EventStartTime + EventTime,
	EventEndTime =< Closing.


/*----------------------------------------------------------------------------------------------*/


/*
fillTimeLineAllDays(Persons, EventCategories, TimeLine, DayStart, DayEnd, Hotel, HotelCategories, Vehicle, FullBudget, ResultTimeLine)
Persons = [Erwachsene, Kinder]
EventCategories
TimeLine = Restliche Eventliste
DayStart = Startzeit des Tages
DayEnd = Ende des Tages
Hotel = Name des Hotels
HotelCategorie2 = Kategorien des Hotels = ['Kat.Name', ...]
Vehicle = Fahrzeug
FullBudget = Budget
ResultTimeLine = Timeline nach Füllung
Price = Preis der gesamten Tour


Beispiele:
fillTimeLineAllDays([1,0] ,['Bar', 'Freizeit','Bildung'], [['Strelapark',1,600,45,'Car'],['Theater Stralsund',2,1131,20,'Car']], 500, 1320, 'Hiddenseer Hotel', _, 'Car', 500000, FTL)
trace, fillTimeLineAllDays([1,0] ,['Bar', 'Freizeit','Bildung'], [], 500, 1320, 'Hiddenseer Hotel', _, 'Car', 500000, FTL)
trace, fillTimeLineAllDays([1,2] ,['Bar', 'Freizeit','Bildung','Unterhaltung'], [], 500, 1320, _, [4,5], 'Car', 500000, FTL)
fillTimeLineAllDays([1,0] ,['Bar', 'Freizeit','Bildung'], [], 500, 1320, _, [2], 'Car', 500000, FTL)

*/

fillTimeLineAllDays(Persons, EventCategories, TimeLine, DayStart, DayEnd, Hotel1, HotelCategories, Vehicle, FullBudget, ResultTimeLine):-
	% Prüfe ob Hotel vorhanden
	((
		var(Hotel1),
		findHotelsForTrip(HotelCategories, Hotel2, FullBudget, Persons)
	)
	;
	(
		Hotel2 = Hotel1
	)),
	Hotel = Hotel2,

	% Teile Timeline in Tag 1 und Tag 2 auf
	splitList(TimeLine, 1, DayTimeLineBack1, DayTimeLineBack2),

	write("DTLB1" + DayTimeLineBack1), nl,
	write("DTLB2" + DayTimeLineBack2), nl,

	% Prüfe ob TagesTimeline 1 leer und fahre fort mit Generierung
	((
		DayTimeLineBack1 = [],
		OldPrice1 = 0
		)
		;
		(
		checkEventsOnTime(Persons, TimeLine, DayStart, DayEnd, Hotel, HotelCategories, FullBudget, Return, OldPrice1)
	)),
	Budget1 is FullBudget - OldPrice1,
	write(Persons + EventCategories + "PrevEvent" + "Front" + DayTimeLineBack1 + TimeLine + "1" + DayStart + DayEnd + Hotel + HotelCategories + FullBudget + Budget1),
	fillTimeLine(Persons, EventCategories, _, _, DayTimeLineBack1, TimeLine, 1, DayStart, DayEnd, Hotel, HotelCategories, Vehicle, FullBudget, Budget1, ResultTimeLine1),
		write("ResultTimeLine nach Tag 1: " + ResultTimeLine1), nl,
		write("Ende Tag 1"), nl,

	% Prüfe ob TagesTimeline 2 leer und fahre fort mit Generierung
	((
		DayTimeLineBack2 = [],
		OldPrice2 = 0
		)
		;
		(
		checkEventsOnTime(Persons, TimeLine, DayStart, DayEnd, Hotel, HotelCategories, FullBudget, Return, OldPrice2)
	)),
	Budget2 is FullBudget - OldPrice2,
	write(Persons + EventCategories + "PrevEvent" + "Front" + DayTimeLineBack2 + ResultTimeLine1 + "2" + DayStart + DayEnd + Hotel + HotelCategories + FullBudget + Budget2),
	fillTimeLine(Persons, EventCategories, _, _, DayTimeLineBack2, ResultTimeLine1, 2, DayStart, DayEnd, Hotel, HotelCategories, Vehicle, FullBudget, Budget2, ResultTimeLine2),
		write("ResultTimeLine nach Tag 2: " + ResultTimeLine2), nl,
		write("Ende Tag 2"), nl,

	ResultTimeLine = ResultTimeLine2.




 /*
Füllt die bestehende Timeline mit Events
fillTimeLine(Persons, EventCategories, PrevEvent, DayTimeLineFront, DayTimeLineBack,
TimeLine, Day, DayStart, DayEnd, Hotel, HotelCategories, Vehicle, FullBudget, Budget,
ResultTimeLine):-
Persons = [Erwachsene, Kinder]
PrevEvent = VorgängerEvent
DayTimeLineFront = vorderer Teil der Tagesliste (letztes Element der Liste ist das PrevEvent)
DayTimeLineBack = hinterer Teil der Tagesliste
TimeLine = Komplette Eventliste für beide Tage
Day = Tag um den es sich handelt
DayStart = Startzeit des Tages
DayEnd = Ende des Tages
Hotel = Name des Hotels
HotelCategories = Kategorien der Hotels
Vehicle = Fahrzeug
FullBudget = Maximales Budget
Budget = aktuelles Restbudget in der Berechnung
ResultTimeLine = Timeline nach Füllung
Price = Preis der gesamten Tour
*/
fillTimeLine(Persons, EventCategories, PrevEvent, DayTimeLineFront, DayTimeLineBack, TimeLine, Day, DayStart, DayEnd, Hotel, HotelCategories, Vehicle, FullBudget, Budget, ResultTimeLine):-
		nl, write("Starte fillTimeLine"), nl, nl,
	checkRTL(ResultTimeLine, ResultTimeLine1),
		%write("TimeLine: " +TimeLine), nl,
		%write("PE: "+PrevEvent), nl,
		%write("DTLF:"+DayTimeLineFront), nl,
		%write("DTLB:"+DayTimeLineBack), nl,
	((
		% PrevEvent ist nicht angegeben, damit befindet sich die Schleife am Anfang des Tages
		% DayTimeLineFront ist leer, damit befindet sich die Schleife am Anfang des Tages
		% DayTimeLineBack ist leer, damit befindet sich die Schleife am Ende des Tages
		var(PrevEvent),
		var(DayTimeLineFront),
		nonvar(DayTimeLineBack),

		DayTimeLineBack = [],
		nonvar(TimeLine),
			nl, nl, write("Suche für leeren Tag startet"), nl, nl,
		% Prüfe ob Budget überhaupt reicht
		calcHotelPrice(Persons, Hotel, HotelPrice),
		FullHotelPrice is HotelPrice + HotelPrice + HotelPrice,
		RestBudget = Budget - FullHotelPrice,
		((
			RestBudget > 0,
			write(EventCategories + Persons + Budget + Hotel + DayStart + DayEnd + Vehicle), nl,
			findEventForEmptyTimeLine(EventCategories, Persons, Budget, Hotel, TimeLine, Day, DayStart, DayEnd, Vehicle, ResultDayTimeLine1),
				nl, write("RTL nach Suche für erstes Event: " + ResultDayTimeLine1), nl,
			((
				ResultDayTimeLine1 = [],
				ResultTimeLine = TimeLine
				)
				;
				(
				[PrevEventNew] = ResultDayTimeLine1,
				append(TimeLine, ResultDayTimeLine1, ResultFullTimeLine1),
				[EventName, _, _, _, _] = PrevEventNew,
				calcApproachForEvent(Persons, EventsName, _, Hotel, Vehicle, 0,  [_,_,_,_,ApproachPrice]),
				ReturnBudget is FullBudget - FullHotelPrice - ApproachPrice - ApproachPrice,
					nl, nl, write("Suche für leeren Tag beendet"), nl, nl,
					% write(Persons + EventCategories + PrevEventNew + ResultDayTimeLine1 + "[]" + ResultFullTimeLine1 + Day + DayStart + DayEnd + Hotel + HotelCategories + Vehicle + FullBudget + ReturnBudget + ResultFullTimeLine2),
				fillTimeLine(Persons, EventCategories, PrevEventNew, ResultDayTimeLine1, [], ResultFullTimeLine1, Day, DayStart, DayEnd, Hotel, HotelCategories, Vehicle, FullBudget, ReturnBudget, ResultFullTimeLine2),
					% write("RFTL2: " +  ResultFullTimeLine2), nl,
				ResultTimeLine = ResultFullTimeLine2
			))

		)
		;
		(
			% write("Suche Event für leete Timeline unnötig das Geld nicht reicht."), nl,
			ResultTimeLine = []
		))
	)
	;
	(
		% PrevEvent ist angegeben, damit befindet sich die Schleife mitten im Tag
		% DayTimeLineFront ist gefüllt, damit befindet sich die Schleife mitten im Tag
		% DayTimeLineBack ist leer, damit befindet sich die Schleife am Ende des Tages
		[_, _, _, _, _] = PrevEvent,
		nonvar(DayTimeLineFront),
		nonvar(DayTimeLineBack),
		DayTimeLineBack = [],
		checkEventsOnTime(Persons, TimeLine, DayStart, DayEnd, Hotel, HotelCategories, FullBudget, Return, OldPrice),
			nl, nl, write("Suche LastEventOfDay startet"), nl, nl,
		findEventEndOfDay(Persons, EventCategories, PrevEvent, DayTimeLineFront, TimeLine, Budget, DayEnd, Hotel, PrevEventNew, FrontNew, ResultFullTimeLine, ReturnBudget),
			write("RFTL nach Suche für LastEventOfDay: " +ResultFullTimeLine), nl,
			nl, nl, write("Suche LastEventOfDay beendet"), nl, nl,
		ResultTimeLine = ResultFullTimeLine
	)
	;
	(

		% PrevEvent ist angegeben, damit befindet sich die Schleife mitten im Tag
		% DayTimeLineFront und Back sind angegeben, damit befindet sich die Schleife mitten am Tag
		nonvar(PrevEvent),
		[_, _, _, _, _] = PrevEvent,
		nonvar(DayTimeLineFront),
		nonvar(DayTimeLineBack),
		checkEventsOnTime(Persons, TimeLine, DayStart, DayEnd, Hotel, HotelCategories, FullBudget, Return, OldPrice),
			nl, nl, write("Suche zwischen 2 Events startet"), nl, nl,
		findEventBetweenEvents(Persons, EventCategories, PrevEvent, DayTimeLineFront, DayTimeLineBack, TimeLine, Budget, PrevEventNew, FrontNew, TailNew, ResultFullTimeLine, ReturnBudget),
		NewPrice is FullBudget - ReturnBudget,
		NewBudget is FullBudget - NewPrice,
			nl, nl, write("Suche zwischen 2 Events beendet"), nl, nl,
			% write(Persons + EventCategories + PrevEventNew + FrontNew + TailNew + ResultFullTimeLine + Day + DayStart + DayEnd + Hotel + HotelCategories + Vehicle + FullBudget + ReturnBudget + ResultFullTimeLine1),
		fillTimeLine(Persons, EventCategories, PrevEventNew, FrontNew, TailNew, ResultFullTimeLine, Day, DayStart, DayEnd, Hotel, HotelCategories, Vehicle, FullBudget, ReturnBudget, ResultFullTimeLine1),
			nl, nl, write("RFTL: " +ResultFullTimeLine1), nl,
		ResultTimeLine = ResultFullTimeLine1
	)
	;
	(
		% kein PrevEvent angegeben, damit befindet sich die Schleife am Anfang des Tages
		% kein DayTimeLineFront angegben, damit befindet sich die Schleife am Anfang des Tages
		% DayTimeLineBack ist angegeben, somit gibt es folgende Events
		var(PrevEvent),
		var(DayTimeLineFront),
		nonvar(DayTimeLineBack),
		checkEventsOnTime(Persons, TimeLine, DayStart, DayEnd, Hotel, HotelCategories, FullBudget, Return, OldPrice),
			nl, nl, write("Suche FirstEventOfDay startet"), nl, nl,
		[EventHead|_] = DayTimeLineBack,
		RestBudget is FullBudget-OldPrice,
			% write(Persons + EventCategories + DayTimeLineBack + TimeLine + DayStart + DayEnd + Hotel + RestBudget + ResultDayTimeLine1 + ReturnBudget), nl,
		findFirstEventOfDay(Persons, EventCategories, DayTimeLineBack, TimeLine, DayStart, DayEnd, Hotel, RestBudget, ResultDayTimeLine1, ReturnBudget),
		NewPrice is Budget-ReturnBudget,
		[Front|Tail1] = ResultDayTimeLine1,
		refreshTimeLine(ResultDayTimeLine1, Day, TimeLine, ResultFullTimeLine2),
			nl, nl, write("Suche FirstEventOfDay beendet"), nl, nl,
		fillTimeLine(Persons, EventCategories, Front, Front, Tail1, ResultFullTimeLine2, Day, DayStart, DayEnd, Hotel, HotelCategories, Vehicle, FullBudget, ReturnBudget, ResultFullTimeLine),
			nl, write("RFTL: " +ResultFullTimeLine), nl,
		ResultTimeLine = ResultFullTimeLine
	)).

/*----------------------------------------------------------------------------------------------*/


/*
findEventForEmptyTimeLine
wird von fillTimeLine verwendet
alle Parameter wie bei fillTimeLine
*/
findEventForEmptyTimeLine(EventCategories, Persons, Budget, Hotel, TimeLine, Day, DayStart, DayEnd, Vehicle, ResultTimeLine):-
	findEventForFreeTime4(EventCategories, Persons, Budget, Hotel, TimeLine, DayStart, DayEnd, Vehicle, Result),
	((
		Result = [],
		ResultTimeLine = [],
			write("Kein Event für EmptyTimeLine gefunden"), nl
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
			write("Event für EmptyTimeLine" + ResultEvent),
		ResultTimeLine = [ResultEvent]
	)).


/*
findEventEndOfDay
wird von fillTimeLine verwendet
alle Parameter wie bei fillTimeLine
*/
findEventEndOfDay(Persons, EventCategories, PrevEvent, DayTimeLineFrontIn, TimeLine, Budget, DayEnd, Hotel, PrevEventNew1, FrontNew1, ResultFullTimeLine1, ReturnBudget1):-
			write("Find Event for end of day"), nl,
		((
			DayTimeLineFrontIn = [_,_,_,_,X],
			vehicle(X,_,_),
			DayTimeLineFront = [DayTimeLineFrontIn]
			)
			;
			(
			DayTimeLineFront = DayTimeLineFrontIn
		)),
			% write("DTLF" + DayTimeLineFront), nl,
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
		addResultToTimeLineBetween(PrevEvent, Result, DayTimeLineFront, [], Day, Vehicle, ResultEvent, _),
		append(DayTimeLineFront, [ResultEvent], FrontNew),
		refreshTimeLine(FrontNew, Day, TimeLine, ResultFullTimeLine),
		calcApproachForEvent(Persons, PrevEventName, _, Hotel,  Vehicle, 0,  [_,_,_,_,OldApproachPrice]),
		calcApproachForEvent(Persons, PrevEventName, Result, _,  Vehicle, 0,  [_,_,_,_,NewApproachPrice1]),
		calcApproachForEvent(Persons, Result, _, Hotel,  Vehicle, 0,  [_,_,_,_,NewApproachPrice2]),
		calcEventPrice(Persons, Result, EventPrice),
		ReturnBudget is Budget + OldApproachPrice - NewApproachPrice1 - NewApproachPrice2 - EventPrice,
		PrevEventNew = ResultEvent,
			nl, write("Find Event end of Day"), nl,
			% write(Persons + EventCategories + PrevEventNew + FrontNew + ResultFullTimeLine + ReturnBudget + DayEnd + Hotel + PrevEventNew1 + FrontNew1 + ResultFullTimeLine1 + ReturnBudget1),
		findEventEndOfDay(Persons, EventCategories, PrevEventNew, FrontNew, ResultFullTimeLine, ReturnBudget, DayEnd, Hotel, PrevEventNew1, FrontNew1, ResultFullTimeLine1, ReturnBudget1)
	)).


/*
findEventBetweenEvents
wird von fillTimeLine verwendet
alle Parameter wie bei fillTimeLine
*/
findEventBetweenEvents(Persons, EventCategories, PrevEvent, DayTimeLineFrontIn, DayTimeLineBack, TimeLine, Budget, PrevEventNew, DayTimeLineFrontNew, DayTimeLineBackNew, ResultFullTimeLine, ReturnBudget):-
		nl, write("Find Event Between Events startet"), nl,
			% write(PrevEvent), nl,
			% write("DTLF IN" + DayTimeLineFrontIn), nl,
			% write("DTLB" + DayTimeLineBack), nl,
		((
			DayTimeLineFrontIn = [_,_,_,_,X],
			vehicle(X,_,_),
			DayTimeLineFront = [DayTimeLineFrontIn]
			)
			;
			(
			DayTimeLineFront = DayTimeLineFrontIn
		)),
	[NextEvent|TailBack] = DayTimeLineBack,
	[PrevEventName, Day, _, _, Vehicle] = PrevEvent,
	[NextEventName, _, _, _, Vehicle] = NextEvent,
	findEventForFreeTime2(TimeLine, EventCategories, Persons, Budget, PrevEvent, NextEvent, Result),
		write("Gefundenes Event: " + Result), nl,
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
			addResultToTimeLineBetween(PrevEvent, Result, DayTimeLineFront, DayTimeLineBack, Day, Vehicle, ResultEvent, ResultDayTimeLine),
			append(DayTimeLineFront, [ResultEvent], DayTimeLineFrontNew),
			refreshTimeLine(ResultDayTimeLine, Day, TimeLine, ResultFullTimeLine),
			calcApproachForEvent(Persons, PrevEventName, NextEventName, _, Vehicle, 0, [_,_,_,_,PriceOld1]),
			calcApproachForEvent(Persons, PrevEventName, Result, _, Vehicle, 0, [_,_,_,_,PriceNew1]),
			calcApproachForEvent(Persons, Result, NextEventName, _, Vehicle, 0, [_,_,_,_,PriceNew2]),
			calcEventPrice(Persons, Result, PriceNew3),
			BudgetNew is Budget + PriceOld1 - PriceNew1 - PriceNew2 - PriceNew3,
			ReturnBudget = BudgetNew,
			PrevEventNew = ResultEvent

	))
		%write("Neue Eventliste: " + ResultFullTimeLine), nl,
		%write("Neue Tagesliste Front: " +  DayTimeLineFrontNew), nl,
		%write("Neue Tagesliste Back: " +  DayTimeLineBackNew), nl,
		%write("PrevEventNew" + PrevEventNew), nl
	.


/*
findEventBetweenEvents
wird von fillTimeLine verwendet
Erstellt ein Event für den Zeitraum zwischen "Anfang des Tages" bis zum ersten Event
alle Parameter wie bei fillTimeLine
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
				write("Kein Event für Anfang gefunden"), nl
		)
		;
		(
				write("Neues Event für Anfang gefunden: " +Result), nl,
			addResultToTimeLine(Result, DayTimeLine, DayStart, Day, Vehicle, Hotel, ResultDayTimeLine1),
				%write(ResultDayTimeLine1), nl,
			ResultDayTimeLine = ResultDayTimeLine1,
			% Preis für das neues Event und Anfahrt berechnen und von Budget abziehen
			calcApproachForEvent(Persons, _, Result, Hotel, Vehicle, FirstStartTime, [_,_,_,_,PriceNew1]),
			calcEventPrice(Persons, Result, PriceNew2),
			% Preis für alte Anfahrt zum nächsten Event Berechnen und zu Budget hinfügen
			calcApproachForEvent(Persons, _, FirstEvent, Hotel, Vehicle, FirstStartTime, [_,_,_,_,PriceOld1]),
			% Preis für neue Anfahrt zum nächsten Event Berechnen und von Budget abziehen
			calcApproachForEvent(Persons, Result, FirstEvent, Hotel, Vehicle, FirstStartTime, [_,_,_,_,PriceNew3]),
			NewEventPrice is PriceNew1 + PriceNew2 + PriceNew3 - PriceOld1,
			NewBudget is (RestBudget - NewEventPrice),
			ReturnBudget = NewBudget
		))
	).



/*----------------------------------------------------------------------------------------------*/



/*
findEventForFreeTime
wird von findFirstEventOfDay verwendet
Sucht anhand der übergebenen Parameter ein Event heraus
*/
findEventForFreeTime(TimeLine, NextEvent, EventCategories, Persons, Budget, Hotel, Vehicle, DayStart, _, NextRealStartTime, Result):-
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
findEventForFreeTime2
wird von findEventBetweenEvents verwendet
Sucht anhand der übergebenen Parameter ein Event heraus
*/
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

/*
findEventForFreeTime3
wird von findLastEventOfDay verwendet
Sucht anhand der übergebenen Parameter ein Event heraus
*/
findEventForFreeTime3(TimeLine, PrevEvent, EventCategories, Persons, Budget, Hotel, DayEnd, Result):-
	searchEventsOnCategory(EventCategories, Events),
		write(Events), nl,
	searchPossibleEventsOnDuration3(Persons, PrevEvent, Hotel, DayEnd, Events, PossibleEventsOnDuration),
		write(PossibleEventsOnDuration), nl,
	searchPossibleEventsOnBudget3(Budget, Persons, PrevEvent, Hotel, PossibleEventsOnDuration, PossibleEventsOnBudget),
		write(PossibleEventsOnBudget), nl,
	searchPossibleEventsOnAdultChildRatio(Persons, PossibleEventsOnBudget, PossibleEventOnAdultChildRatio),
		write(PossibleEventOnAdultChildRatio), nl,
	searchPossibleEventsOnTimeline(PossibleEventOnAdultChildRatio, TimeLine, PossibleEventsOnTimeline),
		write(PossibleEventsOnTimeline), nl,
	shuffleOneResult(PossibleEventsOnTimeline, Result), nl,
		write(Result), nl
	.

/*
findEventForFreeTime4
wird von findEventForEmptyTimeLine verwendet
Sucht anhand der übergebenen Parameter ein Event heraus
*/
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




/*----------------------------------------------------------------------------------------------*/



/*
searchPossibleEventsOnDuration
wird von findEventForFreeTime verwendet
Sucht anhand der übergebenen Parameter ein Event heraus
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
searchPossibleEventsOnDuration2
wird von findEventForFreeTime2 verwendet
Sucht anhand der übergebenen Parameter ein Event heraus
*/
searchPossibleEventsOnDuration2(_, _, [], PossibleEventsOnDuration):-
	PossibleEventsOnDuration = [].

searchPossibleEventsOnDuration2(PrevEvent, NextEvent, [EventsHead | EventsTail], PossibleEventsOnDuration):-
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
		%write("MaxDuration: " + MaxDuration + " EventDuration " + EventDuration), nl,
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




/*
searchPossibleEventsOnDuration3
wird von findEventForFreeTime3 verwendet
Sucht anhand der übergebenen Parameter ein Event heraus
*/
searchPossibleEventsOnDuration3(_, _, _, _, [], PossibleEventsOnDuration):-
	PossibleEventsOnDuration = [].

searchPossibleEventsOnDuration3(Persons, PrevEvent, Hotel, DayEnd, [EventsHead | EventsTail], PossibleEventsOnDuration):-
	searchPossibleEventsOnDuration3(Persons, PrevEvent,  Hotel, DayEnd, EventsTail, PossibleEventsOnDuration1),
	[PrevEventName, _, PrevEventStartTime, PrevEventTime, Vehicle] = PrevEvent,
	Earliest is PrevEventStartTime + PrevEventTime,
	event(EventsHead, _, _, _, _, [_, Closing], [EventDuration]),
	findLatest(Closing, DayEnd, Latest),
	MaxDuration is Latest - Earliest,
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



/*
searchPossibleEventsOnDuration4
wird von findEventForFreeTime4 verwendet
Sucht anhand der übergebenen Parameter ein Event heraus
*/
searchPossibleEventsOnDuration4(_, _, _, _, [], PossibleEventsOnDuration):-
	PossibleEventsOnDuration = [].

searchPossibleEventsOnDuration4(DayStart, DayEnd, Hotel, Vehicle, [EventsHead | EventsTail], PossibleEventsOnDuration):-
	searchPossibleEventsOnDuration4(DayStart, DayEnd, Hotel, Vehicle, EventsTail, PossibleEventsOnDuration1),
	event(EventsHead, _, _, _, _, [Opening, Closing], [EventDuration]),
	findEarliestLatest(Opening, DayStart, Closing, DayEnd, Earliest, Latest),
	MaxDuration is Latest - Earliest,
	Persons = [0,0],
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



/*----------------------------------------------------------------------------------------------*/



/*
searchPossibleEventsOnBudget
wird von findEventForFreeTime verwendet
Sucht anhand der übergebenen Parameter ein Event heraus
*/
searchPossibleEventsOnBudget(_, _, _, _, _, [], PossibleEventsOnBudget):-
	PossibleEventsOnBudget = [].

searchPossibleEventsOnBudget(Budget, Persons, Vehicle, NextEvent, Hotel, [EventsHead|EventsTail], PossibleEventsOnBudget):-
	searchPossibleEventsOnBudget(Budget, Persons, Vehicle, NextEvent, Hotel, EventsTail, PossibleEventsOnBudget1),
	[NextEventName, _, _, _, _] = NextEvent,
	calcApproachForEvent(Persons, _, EventsHead, Hotel,  Vehicle, 0,  [_,_,_,_,PriceNew1]),
	calcApproachForEvent(Persons, EventsHead, NextEventName, _,  Vehicle, 0,  [_,_,_,_,PriceNew2]),
	calcApproachForEvent(Persons, _, NextEventName, Hotel,  Vehicle, 0,  [_,_,_,_,PriceOld1]),
	calcEventPrice(Persons, EventsHead, EventPrice),
	(
		(
			FullEventPrice is EventPrice + PriceNew1 + PriceNew2 - PriceOld1,
				%nl, write("Budget für neues Event: " + Budget), nl,
				%nl, write("FullEventPrice: " + FullEventPrice), nl,
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
searchPossibleEventsOnBudget2
wird von findEventForFreeTime2 verwendet
Sucht anhand der übergebenen Parameter ein Event heraus
*/
searchPossibleEventsOnBudget2(_, _, _, _, [], PossibleEventsOnBudget):-
	% write("Ende der Suche"), nl,
	PossibleEventsOnBudget = [].

searchPossibleEventsOnBudget2(Budget, Persons, PrevEvent, NextEvent, [EventsHead | EventsTail], PossibleEventsOnBudget):-
	searchPossibleEventsOnBudget2(Budget, Persons,  PrevEvent, NextEvent, EventsTail, PossibleEventsOnBudget1),
	[PrevEventName, _, _, _, Vehicle] = PrevEvent,
	[NextEventName, _, _, _, _] = NextEvent,
	calcApproachForEvent(Persons, PrevEventName, NextEventName, _,  Vehicle, 0,  [_,_,_,_,OldApproachPrice]),
	calcApproachForEvent(Persons, PrevEventName, EventsHead, _,  Vehicle, 0,  [_,_,_,_,NewApproachPrice1]),
	calcApproachForEvent(Persons, EventsHead, NextEventName, _,  Vehicle, 0,  [_,_,_,_,NewApproachPrice2]),
	calcEventPrice(Persons, EventsHead, EventPrice),
	(
		(
			FullEventPrice is EventPrice + NewApproachPrice1 + NewApproachPrice2 - OldApproachPrice,
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
searchPossibleEventsOnBudget3
wird von findEventForFreeTime3 verwendet
Sucht anhand der übergebenen Parameter ein Event heraus
*/
searchPossibleEventsOnBudget3(_, _, _, _, [], PossibleEventsOnBudget):-
	PossibleEventsOnBudget = [].

searchPossibleEventsOnBudget3(Budget, Persons, PrevEvent, Hotel, [EventsHead | EventsTail], PossibleEventsOnBudget):-
	searchPossibleEventsOnBudget3(Budget, Persons,  PrevEvent, Hotel, EventsTail, PossibleEventsOnBudget1),
	[PrevEventName, _, _, _, Vehicle] = PrevEvent,
	calcApproachForEvent(Persons, PrevEventName, _, Hotel,  Vehicle, 0,  [_,_,_,_,OldApproachPrice]),
	calcApproachForEvent(Persons, PrevEventName, EventsHead, _,  Vehicle, 0,  [_,_,_,_,NewApproachPrice1]),
	calcApproachForEvent(Persons, EventsHead, _, Hotel,  Vehicle, 0,  [_,_,_,_,NewApproachPrice2]),
	calcEventPrice(Persons, EventsHead, EventPrice),
	(
		(
			FullEventPrice is EventPrice + NewApproachPrice1 + NewApproachPrice2 - OldApproachPrice,
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
searchPossibleEventsOnBudget4
wird von findEventForFreeTime4 verwendet
Sucht anhand der übergebenen Parameter ein Event heraus
*/
searchPossibleEventsOnBudget4(_, _, _, _, [], PossibleEventsOnBudget):-
	PossibleEventsOnBudget = [].

searchPossibleEventsOnBudget4(Budget, Persons, Hotel, Vehicle, [EventsHead | EventsTail], PossibleEventsOnBudget):-
	searchPossibleEventsOnBudget4(Budget, Persons, Hotel, Vehicle, EventsTail, PossibleEventsOnBudget1),
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



/*----------------------------------------------------------------------------------------------*/



/*
Prüft ob die Events für alle Gruppenmitglieder geeignet sind
wrid von findEventForFreeTime, findEventForFreeTime2, findEventForFreeTime3, findEventForFreeTime4 verwendet

Beispiel:
searchPossibleEventsOnAdultChildRatio([2,3], ['Theater Stralsund','Strelapark','Hansedom','Nautineum'], PossibleEventOnAdultChildRatio)
searchPossibleEventsOnAdultChildRatio([2,0], ['Theater Stralsund','Strelapark','Hansedom','Nautineum'], PossibleEventOnAdultChildRatio)
*/
searchPossibleEventsOnAdultChildRatio(_, [], PossibleEventOnAdultChildRatio):-
	PossibleEventOnAdultChildRatio = [].

searchPossibleEventsOnAdultChildRatio(Persons, [EventsHead|EventsTail], PossibleEventOnAdultChildRatio):-
	searchPossibleEventsOnAdultChildRatio(Persons, EventsTail, PossibleEventOnAdultChildRatio1),
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



/*----------------------------------------------------------------------------------------------*/



/*
searchPossibleEventsOnTimeline
Prüft ob die Events in der TimeLine vorhanden sind und sortiert sie aus
wird von findEventForFreeTime, findEventForFreeTime2, findEventForFreeTime3, findEventForFreeTime4 verwendet
*/
searchPossibleEventsOnTimeline([], _, PossibleEventsOnTimeline):-
	PossibleEventsOnTimeline = [].

searchPossibleEventsOnTimeline([EventsHead|EventsTail], Timeline, PossibleEventsOnTimeline):-
	searchPossibleEventsOnTimeline(EventsTail, Timeline, PossibleEventsOnTimeline1),
	searchEventInTimeLine(EventsHead, Timeline, Result),
	(
		(
			Result = 'false',
			append(PossibleEventsOnTimeline1, [EventsHead], PossibleEventsOnTimeline2)
		)
		;
		(
			Result = 'true',
			PossibleEventsOnTimeline2 = PossibleEventsOnTimeline1
		)
	),
	PossibleEventsOnTimeline = PossibleEventsOnTimeline2.

searchEventInTimeLine(_, [], Result):-
	Result = 'false'.

searchEventInTimeLine(Event, [Head|Tail], Result):-
	[EventHead, _, _, _, _] = Head,
	((
		Event = EventHead,
		Result = 'true'
	)
	;
	(
		searchEventInTimeLine(Event, Tail, Result)
	)).



/*----------------------------------------------------------------------------------------------*/


/*
shuffleOneResult
Sucht aus den übrig gebliebenen Events zufällig eins aus
wrid von findEventForFreeTime, findEventForFreeTime2, findEventForFreeTime3, findEventForFreeTime4 verwendet
*/
shuffleOneResult([], X):-
	X = [],
		write("ShuffleOneResult leer"), nl.

shuffleOneResult(Events, Result) :-
		write("ShullfeOneResult Liste"), nl,
        length(Events, Length),
        random(0, Length, Index),
       	nth0(Index, Events, Result).



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




/*----------------------------------------------------------------------------------------------*/

/* Hilfsfunktionen für fillTimeLineAllDays und Nachfolger*/


/*
addResultToTimeLineBetween
wird von fillTimeLine benutzt
PrevEvent = VorängerEvent = [PrevEventName, Day, PrevStartTime, PrevEventTime, Vehicle]
Result = Name des neuen Events
DayTimeLineFrontIn = vordere Hälfte der TimeLine des Tages
DayTimeLineBack = hintere Hälfte der Timeline des Tages
Day = Tag
Vehicle = Fahrzeugt
ResultEvent = Rückgabe des Events nach zusammenbau =[ResultEventName, Day, ResultStartTime, ResultEventTime, Vehicle]
ResultDayTimeLine = Rückgabe der aktualisierten kompletten Tagesliste
*/
addResultToTimeLineBetween(PrevEvent, Result, DayTimeLineFrontIn, DayTimeLineBack, Day, Vehicle, ResultEvent, ResultDayTimeLine):-
	nl, write("Starte mit AddResult"), nl,
	((
		DayTimeLineFrontIn = [_,_,_,_,X],
		vehicle(X,_,_),
		DayTimeLineFront = [DayTimeLineFrontIn]
		)
		;
		(
		DayTimeLineFront = DayTimeLineFrontIn
	)),
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
		write("ResultEvent: " + ResultEvent), nl,
	append(DayTimeLineFront, DayTimeLineBack, ResultDayTimeLine1),
	ResultDayTimeLine = ResultDayTimeLine1,
		write("DTLF:" + DayTimeLineFront), nl,
		write("RDTL:" + ResultDayTimeLine), nl.


/*
addResultToTimeLine
wird von fillTimeLine und dessen nachfolgern benutzt
Event = [EventName, Day, StartTime, EventTime, Vehicle]
TimeLine = die komplette alte TimeLine
FromTime = Tagesanfang oder Ende des letzten Events
Day = Tag
Vehicle = Fahrzeug
Hotel = Hotelname
ResultTimeLine = Rückgabe der neuen TimeLine
*/
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



/*
findEarliestLatest
Opening = Öffnungzeit des Events
From = Ende des letzten Events oder Tagesanfang
Closing = Schließung des Events
To = Beginn des nächsten Events oder Tagesende
Earliest = Rückgabe
Latest = Rückgabe
*/
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


/*----------------------------------------------------------------------------------------------*/
/* Mehrfachverwendete Hilfsfunktionen*/


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
