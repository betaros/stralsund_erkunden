:- use_module(events).
:- use_module(java_connection_functions).

:- use_module(library(lists)).
 
/*
* Konstanten fuer die Berechnungen
*/

latInKm(X, Res) :-
	Res is X * 111.66.

lonInKm(X, Res) :-
	Res is X * 19.39.

/*----------------------------------------------------------------------------------------------*/

/*
* Sucht alle Kategorien
*/
findAllCategories(Categories):-
	findall(X, event(_,_,X,_), L),
	appendCategories(C1,L),
	Categories = C1.

	
/* searchHotelCategories
*/
findAllHotelCategories(Categories):-
	findall(X, hotel(_,_,_,X), L),
	appendCategories(C1,L),
	Categories = C1.


appendCategories(C1,[R|[]]):-
		C1 = R.
	
appendCategories(C,[R|L]):-
	appendCategories(C1,L),
	append(C1,R,X),
	C = X.

/*----------------------------------------------------------------------------------------------*/

/*
* Berechnet die Entfernung zwischen zwei Veranstaltungen
* Entfernung = sqrt((XA-XB)^2 + (YA-YB)^2)
* calcDistance(Name Veranstaltung A, Name Veranstaltung B, Entfernung
*/	
calcDistance(EventA, EventB, Distance) :-
	(
		event(EventA, [XA, YA], _, _)
		;
		hotel(EventA, [XA, YA], _, _)	
	),
	event(EventB, [XB, YB], _, _),
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
* Gibt die m�glichen Events zu den Kategorien zur�ck, wenn Events leer
*/
searchEventsOnCategory(Categories,Events):-
	findall([X,V], event(X,_,V,_), List),
	compareCategories(List,Categories,Events1),
	Events = Events1.
	
/*
* Gibt m�gliche Hotels zu den Kategorien zur�ck
*/
searchHotelsOnCategory(Categories,Hotels):-
	findall([X,V], hotel(X,_,_,V), List),
	compareCategories(List,Categories,Hotels1),
	Hotels = Hotels1.
	
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
*Pr�ft f�r alle Events der Liste ob sie einzeln nicht zu teuer sind und gibt die zur�ck die 
*Preislich in das Budget nicht �bersteigen
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
		event(Event,_,_,[AdultPrice,ReducedPrice]),
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


/*----------------------------------------------------------------------------------------------*/


/*
Beispiel positiv an einem Tag:
checkEventsOnTime([1,2], [['Haus 8',1,830,100,'Car'],['Zoo',1,1030,100,'Car']],800, 'X Sterne Hotel', 10000, Return, Price).
Beispiel negativ an einem Tag:
checkEventsOnTime([1,2],[['Haus 8',1,830,100,'Car'],['Zoo',1,930,100,'Car']],800, 'Hansedom', 10000, Return, Price).
Beispiel positiv an 2 Tagen:
checkEventsOnTime([1,2],[['Haus 8',1,830,100,'Car'],['Zoo',2,1030,100,'Car']],800, '1 Sterne Hotel', 10000, Return, Price).
Beispiel negativ an 2 Tagen:
checkEventsOnTime([1,2],[['Haus 8',1,830,100,'Car'],['Haus 8',2,830,100,'Car'],['Zoo',2,930,100,'Car']],800, 'Hansedom', 10000, Return, Price).
Beispiel negativ an 2 Tagen weil zu fr�h begonnen:
checkEventsOnTime([1,2],[['Haus 8',1,830,100,'Car'],['Haus 8',2,830,100,'Car'],['Zoo',2,930,100,'Car']],830, 'Hansedom', 10000, Return, Price).
Beispiel negativ an 2 Tagen weil Budget zu gering:
checkEventsOnTime([1,2],[['Haus 8',1,830,100,'Car'],['Haus 8',2,830,100,'Car'],['Zoo',2,1030,100,'Car']],800, 'Hansedom', 4500, Return, Price).
*/ 
checkEventsOnTime(Persons,[EventHead|EventsTail],DayStart, Hotel, Budget, Return, Price):-
	(
		write('Pr�fe Event ohne Vorg�nger'), nl,
		[ThisEvent,_,EventStartTime,_,Vehicle] = EventHead,
		write(Hotel + "zu" + ThisEvent), nl,
		calcApproachForEvent(Persons, _, ThisEvent, Hotel, Vehicle, EventStartTime, [_,_,_,RealStartTime,Price1]),
		write("Startzeit des Tages: "+DayStart), nl,
		write("Startzeit: "+RealStartTime), nl,
		RealStartTime >= DayStart,
		write("Event g�ltig"), nl,
		checkEventsOnTime(Persons, EventHead,EventsTail,DayStart,Hotel,Budget, Return1, Price2),
		Return = Return1,
		calcFullPrice(Persons, Price1, Price2, ThisEvent, Price3),
		Price = Price3,
		write("Entg�ltiger Gesamtpreis: "+Price),
		Budget >= Price
	)
	;
	(
		write("Event ung�ltig"), nl,
		Price = 0,
		Return = false,!
	).

checkEventsOnTime(Persons, PrevEventInput,[EventHead|EventsTail],DayStart, Hotel,Budget, Return, Price):-
	(
		write('Pr�fe Event mit Vorg�nger'), nl,
		[ThisEvent,Day,EventStartTime,_,Vehicle] = EventHead,
		[PrevEvent,PrevDay,PrevEventStartTime,PrevEventTime,_] = PrevEventInput,
		((
			PrevDay \= Day,
			write("Unterschiedliche Tage"), nl,
			calcApproachForEvent(Persons, _, ThisEvent, Hotel, Vehicle, EventStartTime, [_,_,_,RealStartTime,Price1]),
			write("Startzeit des Tages: "+DayStart), nl,
			write("Startzeit: "+RealStartTime), nl,
			RealStartTime >= DayStart			
		)
		;
		(
			write(PrevEvent + "zu" + ThisEvent), nl,
			calcApproachForEvent(Persons, PrevEvent, ThisEvent, Hotel, Vehicle, EventStartTime, [_,_,_,RealStartTime,Price1]),
			PrevEventEndTime is PrevEventStartTime+PrevEventTime,
			write("Ende des letzten Events: "+PrevEventEndTime), nl,
			write("Startzeit: "+RealStartTime), nl,
			RealStartTime >= PrevEventEndTime		
		)),
		write("Event g�ltig"), nl,
		checkEventsOnTime(Persons, EventHead, EventsTail, DayStart, Hotel,Budget, Return1, Price2),
		Return = Return1,
		calcFullPrice(Persons, Price1, Price2, ThisEvent, Price3),
		Price = Price3,
		write("Gesamtpreis bis hier: "+Price), nl,
		Budget >= Price
	)
	;
	(
		write("Event ung�ltig"), nl,
		Price = 0,
		Return = false,!
	).

checkEventsOnTime(_,_,[],_,_,_,Return,0):-
	Return = true.


/*----------------------------------------------------------------------------------------------*/

	
/*
*calcApproachlForEvent
*Berechnet die Anfahrt zum Event
Previousvent = vorheriges Event
ThisEvent = Event zu dem die Anfahrt berechnet wird
Hotel = Das Hotel des Nutzers
Vehicle = Fahrzeug 
EventTime = Startzeit des ThisEvent
Arrivel wird zur�ckgegeben (Arrival = ('Anfahrt', Vehicle, Zeit in Minuten, Startzeit)
*Wenn PreviousEvent (vorheriges Event) leer, dann wird Hotel genommen.
*Beispiel: calcArrivalForEvent('Cinestar', 'Haus 8', 'Hansedom', 'Car', 800, Arrival).
*/
calcApproachForEvent([AdultCount,ReducedCount], PreviousEvent, ThisEvent, Hotel, Vehicle, EventTime, Approach):-
	((
		nonvar(PreviousEvent),
		write('Kalkuliere PrevEvent zu ThisEvent'), nl,
		calcDistance(PreviousEvent, ThisEvent, Distance)
	)
	;
	(
		var(PreviousEvent),
		write('Kalkuliere Hotel zu ThisEvent'), nl,
		calcDistance(Hotel, ThisEvent, Distance)
	)),
	vehicle(Vehicle, [AdultPrice,ReducedPrice], Speed),
	ArrivalTime is ceiling(Distance/Speed*60),
	write("Zeit f�r Anfahrt: "+ ArrivalTime), nl,
	StartTime is EventTime - ArrivalTime,
	Price is (AdultCount*AdultPrice)+(ReducedCount*ReducedPrice),
	write("Preis f�r Fahrt: "+Price), nl,
	Approach = ['Anfahrt', Vehicle, ArrivalTime, StartTime, Price].


/*----------------------------------------------------------------------------------------------*/


/*CalcFullPrice
calcFullPrice(Persons, Price1, Price2, Event, Price),
Berechnet Preis f�r Event mit Anfahrt incl. 2 weiterer Preise
Genutzt werden Price1 und Price2 f�r die Berechnung in der �berpr�fung der Timeline
*/
calcFullPrice([AdultCount,ReducedCount], Price1, Price2, Event, Price):-
	write("Berechne Preis f�r "+Event), nl,
	event(Event,_,_,[AdultPrice,ReducedPrice]),
	Price is (AdultCount*AdultPrice) + (ReducedCount*ReducedPrice) + Price1 + Price2. 
	
