:- module('events', [event/7,hotel/4,vehicle/3,childCategories/1,adultCategories/1]).

/*
* Wissensdatenbank
*/



/*
* 
* event(Name des Events, [Latitude, Longitude], Liste an Kategorien, Essenskategorien, Preisliste, öffnungszeiten, [Durchschnittliche Dauer]).
*/
event(	'Hansedom', 
	[54.320021,13.043840],
	['Sport','Schwimmen','Sauna'],
	['Fast-Food'],
	[2222,1000],
	[600, 2100],
	[20]).

event(	'Strelapark',
	[54.320678,13.046984],
	['Einkaufen'],
	['Fast-Food'],
	[3333,1000],
	[600, 2100],
	[60]).
	
event(	'Citti', 
	[54.320071,13.047413], 
	['Einkaufen','Grosshandel'],
	['Fast-Food'],
	[0,0],
	[600, 2100],
	[80]). 
	
event(	'Ozeaneum',
	[54.315509,13.097494],
	['Freizeit','Museum','Bildung','Tiere'],
	['Fast-Food'],
	[4444,1000],
	[930, 2100],
	[120]).
	
event(	'Meeresmuseum',
	[54.3123021,13.0845551],
	['Freizeit','Museum','Bildung'],
	['Fast-Food'],
	[5555,1000],
	[900, 2100],
	[20]).
	
event(	'Nautineum',
	[54.305252,13.118912],
	['Freizeit','Museum','Bildung'],
	['Fast-Food'],
	[8111,1000],
	[900, 2100],
	[60]).
	
event(	'Marinemuseum',
	[54.309746,13.119041],
	['Freizeit','Museum','Bildung'],
	[],
	[2332,1000],
	[600, 2100],
	[45]).
	
event(	'Fachhochschule Stralsund',
	[54.339149,13.076232],
	['Bildung','Studium'],
	['Kantinenessen','Mittag'],
	[100,1000],
	[930, 2100],
	[25]).
	
event(	'Zoo',
	[54.319651,13.051815],
	['Tiere'],
	[],
	[3453,1000],
	[930, 2100],
	[30]).
	
event(	'Cinestar',
	[54.311055,13.090076],
	['Freizeit','Unterhaltung'],
	[],
	[5366,1000],
	[930, 2100],
	[20]).
	
event(	'Haus 8',
	[54.340094,13.076638],
	['Bar','Kneipe'],
	[],
	[0,0],
	[930, 2200],
	[20]).
	

/*Hotels
* Unterkunft für die Nacht
* hotel('Name',[Long,Lat],Preis pro Doppelzimmer in Cent pro Nacht, Kategorie).
*/

hotel('X Sterne Hotel',[54.320021,13.043840],1000,[0]).
hotel('1 Sterne Hotel',[54.320021,13.043840],2000,[1]).
hotel('2 Sterne Hotel',[54.320021,13.043840],3000,[2]).
hotel('3 Sterne Hotel',[54.320021,13.043840],4000,[3]).
hotel('4 Sterne Hotel',[54.320021,13.043840],5000,[4]).	
hotel('5 Sterne Hotel',[54.320021,13.043840],6000,[5]).	

/*
* vehicle (Name, Festpreis [Normal, Ermäßigt], Geschwindigkeit in km/Stunde)
*/
vehicle('Car', [100, 100], 35).
vehicle('Bus', [180, 120], 25).
vehicle('Bike', [0,0], 15).
vehicle('Foot', [0,0], 6).

/*
* Kategorien für Kinder und Erwachsene festlegen
*/
childCategories(['Freizeit','Tiere','Schwimmen']).
adultCategories(['Bildung','Bar','Kneipe','Sauna']).