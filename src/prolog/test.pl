/*
* Konstanten fuer die Berechnungen
*/

latInKm(X, Erg) :-
	Erg is X * 111.66.

lonInKm(X, Erg) :-
	Erg is X * 19.39.

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
* kategorie(Name der Veranstaltung, Liste an zutreffenden Kategorien) 
*/
kategorie(hansedom, [sport,hotel,schwimmen]).
kategorie(citti, [einkaufen,grosshandel]).

/*----------------------------------------------------------------------------------------------*/

/*
* Funktionen
*/

/*
* Sucht nach Veranstaltung mit passender Kategorie
* sucheVeranstaltung(Liste an Kategorien, Liste an Ergebnissen)
*
* TODO: Logik ausdenken...
*/
sucheVeranstaltung(_, []).
sucheVeranstaltung([Kategorie|Rest], Ergebnis) :- 
	sucheVeranstaltung(Rest, ErgebnisRest),
	member(Kategorie,ErgebnisRest),
	kategorie(Kategorie, Ergebnis).

/*
* Berechnet die Entfernung zwischen zwei Veranstaltungen
* Entfernung = sqrt((XA-XB)^2 + (YA-YB)^2)
* berechneEntfernung(Name Veranstaltung A, Name Veranstaltung B, Entfernung
*/	
berechneEntfernung(VeranstaltungA, VeranstaltungB, Entfernung) :-
	position(VeranstaltungA, XA, YA),
	position(VeranstaltungB, XB, YB),
	latInKm(XA, XAinKm),
	latInKm(XB, XBinKm),
	lonInKm(YA, YAinKm),
	lonInKm(YB, YBinKm), 
	TempX is XAinKm - XBinKm,
	TempY is YAinKm - YBinKm,
	PotX is TempX * TempX,
	PotY is TempY * TempY,
	AddBoth is PotX + PotY,
	Entfernung is sqrt(AddBoth).