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
	findall(X, event(_,_,_,X,_), L),
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
	event(EventA, XA, YA, _, _),
	event(EventB, XB, YB, _, _),
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
	findall([X,V], event(X,_,_,V,_), List),
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
*Prüft für alle Events der Liste ob sie einzeln nicht zu teuer sind und gibt die zurück die 
*Preislich in das Budget nicht übersteigen
*/
checkEventsForBudget(Persons,Budget,MyEvents,ValidEvents):-
	checkEventForBudget(Persons,Budget,MyEvents,ValidEvents1),
	ValidEvents = ValidEvents1.

checkEventForBudget(_,_,[],ValidEvents):-
	ValidEvents = [].
	
checkEventForBudget(Persons,Budget,[Event|MyEvents],ValidEvents):-
	checkEventForBudget(Persons,Budget,MyEvents,ValidEvents1),
	((
		event(Event,_,_,_,[AdultPrice,ReducedPrice]),
		[AdultCount|ReducedCount] = Persons,
		Price is (AdultCount*AdultPrice)+(ReducedCount*ReducedPrice),
		Budget >= Price,		
		append([Event],ValidEvents1,ValidEvents2),
		ValidEvents = ValidEvents2
	)
	;
	(
		ValidEvents = ValidEvents1
	)).

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
	