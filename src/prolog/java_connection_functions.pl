:- module('java_connection_functions', [javaFindAllCategories/1]).


javaFindAllCategories(Categories):-
 	findAllCategories(Categories1),
 	Categories = Categories1.
