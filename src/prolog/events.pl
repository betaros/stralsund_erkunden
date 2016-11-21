:- module('events', [event/6,hotel/4,vehicle/3]).

/*
* Wissensdatenbank
*/



/*
* 
* event(Name des Events, [Latitude, Longitude], Liste an Kategorien, Essenskategorien, Preisliste, öffnungszeiten).
*/
event(	'Hansedom', 
	[54.320021,13.043840],
	['Sport','Schwimmen','Sauna'],
	['Fast-Food'],
	[2000,1000],
	[930, 2100]).

event(	'Strelapark',
	[54.320678,13.046984],
	['Einkaufen'],
	['Fast-Food'],
	[2000,1000],
	[930, 2100]).
	
event(	'Citti', 
	[54.320071,13.047413], 
	['Einkaufen','Grosshandel'],
	['Fast-Food'],
	[0,0],
	[930, 2100]). 
	
event(	'Ozeaneum',
	[54.315509,13.097494],
	['Freizeit','Museum','Bildung','Tiere'],
	['Fast-Food'],
	[2000,1000],
	[930, 2100]).
	
event(	'Meeresmuseum',
	[54.3123021,13.0845551],
	['Freizeit','Museum','Bildung'],
	['Fast-Food'],
	[2000,1000],
	[930, 2100]).
	
event(	'Nautineum',
	[54.305252,13.118912],
	['Freizeit','Museum','Bildung'],
	['Fast-Food'],
	[2000,1000],
	[930, 2100]).
	
event(	'Marinemuseum',
	[54.309746,13.119041],
	['Freizeit','Museum','Bildung'],
	['Fast-Food'],
	[2000,1000],
	[930, 2100]).
	
event(	'Fachhochschule Stralsund',
	[54.339149,13.076232],
	['Bildung','Studium'],
	['Fast-Food'],
	[0,0],
	[930, 2100]).
	
event(	'Zoo',
	[54.319651,13.051815],
	['Tiere'],
	['Fast-Food'],
	[2000,1000],
	[930, 2100]).
	
event(	'Cinestar',
	[54.311055,13.090076],
	['Freizeit','Unterhaltung'],
	['Fast-Food'],
	[2000,1000],
	[930, 2100]).
	
event(	'Haus 8',
	[54.340094,13.076638],
	['Bar','Kneipe'],
	['Fast-Food'],
	[0,0],
	[930, 2100]).
	

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