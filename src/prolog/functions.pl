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
* Sucht nach Veranstaltung mit passender Kategorie
* findEvent(Liste an Kategorien, Liste an Ergebnissen)
*
* TODO: Logik ausdenken...
*/
findEvent(_, []).
findEvent([Category|Residual], Result) :- 
	findEvent(Residual, Temp),
	member(Category,Temp),
	categorie(Category, Result).

/*
* Sucht alle Events
*/
findAllEvents(E):-
	event(E).

/*
* Sucht alle Kategorien
*/
findAllCategories(Categories):-
	findall(X, event(_,_,_,X), L),
	appendCategories(C1,L),
	nl,
	write(C1),
	nl,
	Categories = C1.

appendCategories(C1,[R|[]]):-
		C1 = R.
	
appendCategories(C,[R|L]):-
	appendCategories(C1,L),
	write(C1),
	nl,
	append(C1,R,X),
	C = X.


/*
* Berechnet die Entfernung zwischen zwei Veranstaltungen
* Entfernung = sqrt((XA-XB)^2 + (YA-YB)^2)
* calcDistance(Name Veranstaltung A, Name Veranstaltung B, Entfernung
*/	
calcDistance(EventA, EventB, Distance) :-
	event(EventA, XA, YA, _),
	event(EventB, XB, YB, _),
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


/*
* Gibt die möglichen events zurück, wenn events leer
*/
getEventsForProfile(Persons,Budget,Categories,Events):-
	searchEventsOnCategory(Categories,Events).
	
searchEventsOnCategory(Categories,Events):-
	findall([X,V], event(X,_,_,V), List),
	write(List),
	nl,
	write(Categories),
	nl,
	compareCategories(List,Categories,Events1),
	Events = Events1.

compareCategories([E|L],Categories,Events1):-
	compareCategories(L,Categories,Events2),
	E = [X,Y],
	(  compare_list(Y,Categories)
	-> (
		append([X],Events2,Events3),
	   	Events1 = Events3,
	   	write("Gefunden")
	   )
	   ;
	   (
	   	Events1 = Events2,
	   	write("Nicht gefunden")
	   )	
	).
	
compareCategories([],_,Events1):-
	Events1 = [].

compare_list([],[]):-false.
compare_list([],_):-false.
compare_list([L1Head|L1Tail], List2):-
    (member(L1Head,List2),
    write(L1Head + List2),
    nl)
    ;
    (compare_list(L1Tail,List2),
    write("Keine Übereinstimmung")).
	