:- use_module(events).

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

append([],L,L).
append([H|T],L2,[H|L3]):-  
	append(T,L2,L3). 

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
	
