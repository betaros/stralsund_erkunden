:- module('events', [event/4,hotel/3,vehicle/3]).

/*
* Wissensdatenbank
*/



/*
* 
* event(Name des Events, [Latitude, Longitude], Liste an Kategorien, Preisliste).
*/
event(	'Hansedom', 
	[54.320021,13.043840],
	['Sport','Hotel','Schwimmen','Sauna'],
	[2000,1000]).

event(	'Strelapark',
	[54.320678,13.046984],
	['Einkaufen'],
	[2000,1000]).
	
event(	'Citti', 
	[54.320071,13.047413], 
	['Einkaufen','Grosshandel'],
	[0,0]). 
	
event(	'Ozeaneum',
	[54.315509,13.097494],
	['Freizeit','Museum','Bildung','Tiere'],
	[2000,1000]).
	
event(	'Meeresmuseum',
	[54.3123021,13.0845551],
	['Freizeit','Museum','Bildung'],
	[2000,1000]).
	
event(	'Nautineum',
	[54.305252,13.118912],
	['Freizeit','Museum','Bildung'],
	[2000,1000]).
	
event(	'Marinemuseum',
	[54.309746,13.119041],
	['Freizeit','Museum','Bildung'],
	[2000,1000]).
	
event(	'Fachhochschule Stralsund',
	[54.339149,13.076232],
	['Bildung','Studium'],
	[0,0]).
	
event(	'Zoo',
	[54.319651,13.051815],
	['Tiere'],
	[2000,1000]).
	
event(	'Cinestar',
	[54.311055,13.090076],
	['Freizeit','Unterhaltung'],
	[2000,1000]).
	
event(	'Haus 8',
	[54.340094,13.076638],
	['Bar','Kneipe'],
	[0,0]).
	
businesshours(	'Hansedom',
		[
			[mon, 930, 2100],
			[tue, 930, 2100],
			[wed, 930, 2100],
			[thu, 930, 2100],
			[fri, 930, 2200],
			[sat, 930, 2200],
			[sun, 930, 2100]
		]).

/*Hotels
* Unterkunft für die Nacht, immer ab 18 bis 8 Uhr
* hotel('Name',[Long,Lat],Preis pro Doppelzimmer in Cent pro Nacht).
*/

hotel('Hansedom',[54.320021,13.043840],3000).
		
/*
* vehicle (Name, Festpreis [Normal, Ermäßigt], Geschwindigkeit in km/Stunde)
*/
vehicle('Car', [100, 100], 35).
vehicle('Bus', [180, 120], 25).
vehicle('Bike', [0,0], 15).
vehicle('Foot', [0,0], 6).