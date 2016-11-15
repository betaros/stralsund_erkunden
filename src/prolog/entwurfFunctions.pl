:- use_module(library(aggregate)).

% Suchen anklicken
searchUsefulEvents([Adult, Reduced], Budget, [Categories], Events, Schedule) :-
  searchEventsOnCategory(Categories, Events),
  checkEventsForBudget(Persons,Budget,Events,ValidEvents),
  generateSchedule([Adult, Reduced], Budget, ValidEvents, Schedule).

day :- 1440.
sleep :- 480.

busticket :- 180.
busticketReduced :- 120.

% erstellt automatisch einen Zeitplan
% generateSchedule([Adult, Reduced], Budget, Events, Schedule)
generateSchedule([Adult, Reduced], Budget, Events, Schedule).
generateSchedule([Adult|TailReduced], Budget, [Event|TailEvents], Schedule) :-
  generateSchedule(TailReduced, Budget, ),
  /*
  * Eigene Liste fuer jeden Tag
  * Hole Event
  * berechne in Abhaengigkeit zur verbleibenden Zeit und verbleibenden Budget, ob Event moeglich ist
  * fuege Event zur Liste hinzu
  * dichtestes Hotel zu den Events finden
  * Hotel hinzufuegen, nicht in Zeitplan einbeziehen, da es bereits in sleep enthalten ist
  * Ankuftzeit von Tag abziehen
  * wenn Tag 1 vorbei kein weiteres Event fuer den Tag zulassen
  * Tag 2 zu beginn sleep abziehen
  * Tag 2 fuellen
  */
  event(),
  BudgetLeft is Budget,
  Timeleft is day.

% berechnet den Preis eines Events
calcEventPrice(Event, Adult, Reduced, Transportation, Cost) :-
  event(PriceAdult, PriceReduced),
  (
    Transportation == 'Bus'
    -> (
        TravelCostAdult = Adult * busticket,
        TravelCostReduced = Reduced * busticketReduced,
        TravelCost = TravelCostReduced + TravelCostAdult)
        )
    ;
    TravelCost is 0
  ),
  CostAdult is Adult * PriceAdult,
  CostReduced is Reduced * ReducedPrice,
  Cost is CostAdult + CostReduced + TravelCost.

% max 1 Hotel moeglich
% checkHotelCount(CurrentList).
checkHotelCount(X).
checkHotelCount([X|Tail]) :-
  checkHotelCount()
  Counter is Counter1 + 1,
  (Counter == 1
  -> true
  ; fail).

% berechnet Zeit fuer Anfahrt und Event
calculateTravelTime(EventA, EventB, DesiredTime, Transportation, Time) :-
  calcDistance(EventA, EventB, Distance),
  Time is Distance * Transportation + DesiredTime.

%----------------------------------------------------------------------------
% - anklicken
% loescht Event aus EventList
%
% removeItemFromList(c, [a,b,c,d,e], NewList).
% NewList = ["a", "b", "d", "e"]
removeItemFromList(Event, [Event|Tail], Tail).
removeItemFromList(Event, [X|Tail], [X|Tail1]) :-
  removeItemFromList(Event, Tail, Tail1).

%----------------------------------------------------------------------------
% + anklicken
% Fuegt Event einer Liste hinzu
%
% addItemToList([a,b,c,d], e, NewList).
% NewList = ["a", "b", "c", "d", "e"]
addItemToList(CurrentList, Event, NewList) :-
  % Fuege Event zur CurrentList hinzu
  append(CurrentList, [Event], NewList).
