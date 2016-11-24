:- module('java_connection_functions', [javaFindAllCategories/1]).


javaFindAllCategories(Categories):-
 	findAllCategories(Categories1),
 	Categories = Categories1.
 	
 /*
Füllt die bestehende Timeline mit Events
fillTimeLine(Persons, PrevEvent, EventList, DayStart, DayEnd, Hotel, HotelCategorie, Budget, ResultTimeLine, Return, Price)
fillTimeLine(A, B, C, D, E, F , G, H, I, J, K)
A = Persons = [Erwachsene, Kinder]
B = PrevEvent = Vorhergehendes Event
C = EventList = Restliche Eventliste
D = DayStart = Startzeit des Tages
E = DayEnd = Ende des Tages
F = Hotel = Name des Hotels
G = HotelCategie = Kategorien des Hotels = ['Kat.Name', ...]
H = Budget = Budget
I = ResultTimeLine = Timeline nach Füllung
J = Return = True oder False
K = Price = Preis der gesamten Tour
trace, fillTimeLine([1,2], _, [['Haus 8',1,1030,100,'Car'],['Zoo',1,1230,100,'Car']],800, 2200, 'X Sterne Hotel', _, 1000000, X, Return, Price).
*/


fillTimeLine(Persons, PrevEvent, EventList, DayStart, DayEnd, Hotel, HotelCategorie, Budget, ResultTimeLine, Return, Price):-
	%checkEventsOnTime(Persons, EventList, DayStart, DayEnd, Hotel, HotelCategorie, Budget, Return, Price),
	[EventHead|EventsTail] = EventList,
	checkRTL(ResultTimeLine, ResultTimeLine1),
	((
		% PrevEvent ist angegeben, damit befindet sich die Schleife mitten im Tag
		nonvar(PrevEvent)
	)
	;
	(
		% kein PrevEvent angegeben, damit befindet sich die Schleife am Anfang des Tages
		var(PrevEvent),
		findFirstEventOfDay(Persons, EventList, DayStart, DayEnd, Hotel, HotelCategorie, Budget, ResultTimeLine1, Return, Price)
	)). 
	

findFirstEventOfDay(Persons, EventList, DayStart, DayEnd, Hotel, HotelCategorie, Budget, ResultTimeLine, Return, Price):-
	[FirstName, FirstDay, FirstStartTime, FirstTime, FirstArrival] = EventHead,
	calcApproachForEvent(Persons, FirstEvent, Hotel, Vehicle, FirstStartTime, [_,_,_,RealStartTime,Price1]),
	(
		PrevStart > DayStart,
		findEventForFreeTime(EventList, Persons, DayStart, RealStartTime, Remain)
	).
	

checkRTL(ResultTimeLine, ResultTimeLine1):-
	((
		% RTL ist angegeben
		nonvar(ResultTimeLine),
		write('ResultTimeLine ist bereits angelegt!'), nl
	)
	;
	(
		% RTL ist angegeben
		var(ResultTimeLine),
		ResultTimeLine1 = [],
		write('ResultTimeLine wurde soeben angelegt!'), nl
	)).
