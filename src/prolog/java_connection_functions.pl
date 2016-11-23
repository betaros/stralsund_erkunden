:- module('java_connection_functions', [javaFindAllCategories/1]).


javaFindAllCategories(Categories):-
 	findAllCategories(Categories1),
 	Categories = Categories1.
 	
 /*
Füllt die bestehende Timeline mit Events
*/

fillTimeLine(Persons,[EventHead|EventsTail], DayStart, DayEnd, Hotel, HotelCategories, Budget, ResultTimeLine, Return, Price):-
	ResultTimeLine = [],
	[PrevName, PrevDay, PrevStart, PrevTime, PrevArrival] = EventHead. 