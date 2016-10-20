/*
* Konstanten fuer die Berechnungen
*/

latInKm(X, Res) :-
	Res is X * 111.66.

lonInKm(X, Res) :-
	Res is X * 19.39.

/*----------------------------------------------------------------------------------------------*/

/*
* Wissensdatenbank
*/

/*
* Spezifiziert die Position der Veranstaltung
* position(Name der Veranstaltung, Latitude, Longitude) 
*/
position(hansedom, 54.3199026, 13.0416835).
position(citti, 54.3200465, 13.0446653).

/*
* Weist den Veranstaltungen Kategorien hinzu
* category(Name der Veranstaltung, Liste an zutreffenden Kategorien) 
*/
category(hansedom, [sport,hotel,schwimmen]).
category(citti, [einkaufen,grosshandel]).

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
* Berechnet die Entfernung zwischen zwei Veranstaltungen
* Entfernung = sqrt((XA-XB)^2 + (YA-YB)^2)
* calcDistance(Name Veranstaltung A, Name Veranstaltung B, Entfernung
*/	
calcDistance(EventA, EventB, Distance) :-
	position(EventA, XA, YA),
	position(EventB, XB, YB),
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