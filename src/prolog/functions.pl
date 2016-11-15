:- use_module(events).
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
* Funktionen
*/

/*
* Sucht alle Kategorien
*/
findAllCategories(Categories):-
	findall(X, event(_,_,X,_), L),
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
		hotel(EventA, [XA, YA], _)	
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

compareCategories([E|L],Categories,Events1):-
	compareCategories(L,Categories,Events2),
	E = [X,Y],
	(  compare_list(Y,Categories)
	-> (
		append([X],Events2,Events3),
	   	Events1 = Events3
	   )
	   ;
	   (
	   	Events1 = Events2
	   )	
	).
	
compareCategories([],_,Events1):-
	Events1 = [].


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

/*
*Check Timeline
*Persons = [Count of Adult, Count of Reduced]
*Budget = Price in cent
*Events = [['eventname', day, starttime in minutes, eventtime in minutes, vehicle], ...]
*daystart = time of start of the day  
*/
checkTimeline(Persons,Budget,Events,Daystart,Hotel):-
	checkEventsOnTime(_, Events, Daystart, Hotel),
	nl.

/*
Beispiel positiv an einem Tag:
checkEventsOnTime(_,[['Haus 8',1,830,100,'Car'],['Zoo',1,1030,100,'Car']],800, 'Hansedom', Return).
Beispiel negativ an einem Tag:
checkEventsOnTime(_,[['Haus 8',1,830,100,'Car'],['Zoo',1,930,100,'Car']],800, 'Hansedom', Return).
Beispiel positiv an 2 Tagen:
checkEventsOnTime(_,[['Haus 8',1,830,100,'Car'],['Zoo',2,1030,100,'Car']],800, 'Hansedom', Return).
Beispiel negativ an 2 Tagen:
checkEventsOnTime(_,[['Haus 8',1,830,100,'Car'],['Haus 8',2,830,100,'Car'],['Zoo',2,930,100,'Car']],800, 'Hansedom', Return).
Beispiel negativ an 2 Tagen weil zu fr�h begonnen:
checkEventsOnTime(_,[['Haus 8',1,830,100,'Car'],['Haus 8',2,830,100,'Car'],['Zoo',2,930,100,'Car']],830, 'Hansedom', Return).
*/
checkEventsOnTime(X,[EventHead|EventsTail],DayStart, Hotel, Return):-
	var(X),
	((
		write('Pr�fe Event ohne Vorg�nger'), nl,
		[ThisEvent,Day,EventStartTime,EventTime,Vehicle] = EventHead,
		write(Hotel + "zu" + ThisEvent), nl,
		calcApproachForEvent(_, ThisEvent, Hotel, Vehicle, EventStartTime, [_,_,_,RealStartTime]),
		write("Startzeit des Tages: "+DayStart), nl,
		write("Startzeit: "+RealStartTime), nl,
		RealStartTime >= DayStart,
		write("Event g�ltig"), nl,
		checkEventsOnTime(EventHead,EventsTail,DayStart,Hotel, Return1),
		Return = Return1
	)
	;
	(
		write("Event ung�ltig"), nl,
		Return = false,!
	)).

checkEventsOnTime(PrevEventInput,[EventHead|EventsTail],DayStart, Hotel, Return):-
	nonvar(PrevEventInput),
	((
		write('Pr�fe Event mit Vorg�nger'), nl,
		[ThisEvent,Day,EventStartTime,EventTime,Vehicle] = EventHead,
		[PrevEvent,PrevDay,PrevEventStartTime,PrevEventTime,_] = PrevEventInput,
		((
			PrevDay \= Day,
			write("Unterschiedliche Tage"), nl,
			calcApproachForEvent(_, ThisEvent, Hotel, Vehicle, EventStartTime, [_,_,_,RealStartTime]),
			write("Startzeit des Tages: "+DayStart), nl,
			write("Startzeit: "+RealStartTime), nl,
			RealStartTime >= DayStart			
		)
		;
		(
			write(PrevEvent + "zu" + ThisEvent), nl,
			calcApproachForEvent(PrevEvent, ThisEvent, Hotel, Vehicle, EventStartTime, [_,_,_,RealStartTime]),
			PrevEventEndTime is PrevEventStartTime+PrevEventTime,
			write("Ende des letzten Events: "+PrevEventEndTime), nl,
			write("Startzeit: "+RealStartTime), nl,
			RealStartTime >= PrevEventEndTime		
		)),
		write("Event g�ltig"), nl,
		checkEventsOnTime(EventHead, EventsTail, DayStart, Hotel, Return1),
		Return = Return1
	)
	;
	(
		write("Event ung�ltig"), nl,
		Return = false,!
	)).

checkEventsOnTime(_,[],_,_,Return):-
	Return = true.
	
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
calcApproachForEvent(PreviousEvent, ThisEvent, Hotel, Vehicle, EventTime, Approach):-
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
	write("Zeit f�r Anfahrt:"+ ArrivalTime), nl,
	StartTime is EventTime - ArrivalTime,
	Approach = ['Anfahrt', Vehicle, ArrivalTime, StartTime].