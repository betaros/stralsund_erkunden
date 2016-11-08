:- module('events', [event/4,category/1]).

/*
* Wissensdatenbank
*/

/*
Kategorien
*/

category('Sport').
category('Einkaufen').
category('Hotel').
category('Schwimmen').
category('Sauna').
category('Grosshandel').
category('Freizeit').
category('Bildung').
category('Tiere').
category('Museum').
category('Studium').
category('Unterhaltung').
category('Bar').
category('Kneipe').

/*
* 
* event(Name des Events, Latitude, Longitude, Liste an Kategorien).
*/

event(	'Hansedom', 
	54.320021,
	13.043840,
	['Sport','Hotel','Schwimmen','Sauna']).
	
event(	'Strelapark',
	54.320678,
	13.046984,
	['Einkaufen']).
	
event(	'Citti', 
	54.320071,
	13.047413, 
	['Einkaufen','Grosshandel']). 
	
event(	'Ozeaneum',
	54.315509,
	13.097494,
	['Freizeit','Museum','Bildung','Tiere']).
	
event(	'Meeresmuseum',
	54.3123021,
	13.0845551,
	['Freizeit','Museum','Bildung']).
	
event(	'Nautineum',
	54.305252,
	13.118912,
	['Freizeit','Museum','Bildung']).
	
event(	'Marinemuseum',
	54.309746,
	13.119041,
	['Freizeit','Museum','Bildung']).
	
event(	'Fachhochschule Stralsund',
	54.339149,
	13.076232,
	['Bildung','Studium']).
	
event(	'Zoo',
	54.319651,
	13.051815,
	['Tiere']).
	
event(	'Cinestar',
	54.311055,
	13.090076,
	['Freizeit','Unterhaltung']).
	
event(	'Haus 8',
	54.340094,
	13.076638,
	['Bar','Kneipe']).
	
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