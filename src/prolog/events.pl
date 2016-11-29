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

/*
event(	'Ozeaneum',
	[54.315.691,13.097.708],
	['Bildung','Museum','Wasser','Hafen','Tiere','Kultur'],
	['Cafe','Mittag','regional','deftig','mittlere Preisklasse'],
	[1200,1700],
	[570, 1080],
	[120]).

event(	'Rathaus',
	[54.315.605,13.090.367],
	['Sightseeing','Altstadt','Kultur'],
	[],
	[0,0],
	[360, 1200],
	[15]).

event(	'Deutsches Meeresmuseum',
	[54.312.285,13.086.757],
	['Museum','Altstadt','Bildung','Wasser','Tiere','Kultur'],
	['Mittag','Cafe','Snack','guenstige Preisklasse'],
	[1000,800],
	[600, 1020],
	[120]).

event(	'Gorch Fock',
	[54.316.692,13.098.349],
	['Sightseeing','Hafen','Wasser','Kultur'],
	[],
	[490,250],
	[600, 960],
	[60]).

event(	'St Marienkirche',
	[54.309.919,13.087.711],
	['Sightseeing','Altstadt','Kultur'],
	[],
	[0,0],
	[540, 1080],
	[30]).

event(	'Nikolaikirche',
	[54.315.362,13.090.987],
	['Sightseeing','Altstadt','Kultur'],
	[],
	[0,0],
	[540, 1080],
	[30]).

event(	'Kulturhistorisches Museum Stralsund',
	[54.312.564,13.088.054],
	['Museum','Altstadt','Bildung','Kultur'],
	[],
	[500,250],
	[600, 1020],
	[120]).

event(	'Tierpark Stralsund',
	[54.319.651,13.050.657],
	['Unterhaltung','Tiere', 'Outdoor'],
	['Mittag','Cafe','Abend','guenstige Preisklasse', 'Snack'],
	[700,400],
	[540, 1110],
	[180]).

event(	'Altstadt Shopping',
	[54.314.987,13.090.021],
	['Einkaufen','Altstadt'],
	[],
	[0,0],
	[570, 1140],
	[120]).

event(	'Theater Stralsund',
	[54.317.597,13.088.872],
	['Kultur','Altstadt','Unterhaltung'],
	['Mittag','Snack','mittlere Preisklasse'],
	[2000,2000],
	[540, 1320],
	[120]).

event(	'Strelapark',
	[54.320.700,13.047.758],
	['Einkaufen'],
	['Mittag','Cafe','Snack','Fast-Food','deftig','mittlere Preisklasse','guenstige Preisklasse'],
	[0,0],
	[540, 1200],
	[120]).

event(	'Hansedom',
	[54.319.626,13.044.390],
	['Unterhaltung','Wasser','Schwimmen','Sport'],
	['Mittag','Cafe','Snack','Fast-Food','Abend','deftig','mittlere Preisklasse'],
	[1400,1150],
	[570, 1260],
	[240]).

event(	'Nautineum',
	[54.305.248,13.118.818],
	['Museum','Bildung','Kultur'],
	[],
	[0,0],
	[600, 960],
	[60]).

event(	'Museum für komische Kunst',
	[54.316.301,13.097.320],
	['Museum','Unterhaltung','Kultur','Hafen'],
	[],
	[600,400],
	[600, 1140],
	[90]).

event(	'Hafenrundfahrt',
	[54.317.170,13.095.108],
	['Wasser','Unterhaltung','Hafen'],
	['Mittag','Cafe','Snack','guenstige Preisklasse'],
	[900,400],
	[660, 1050],
	[60]).

event(	'Störtebeker Braumanufaktur',
	[54.291.171,13.094.898],
	['Bar','Unterhaltung'],
	[],
	[1150,290],
	[540, 1020],
	[90]).

event(	'Cinestar Kino',
	[54.310.899,13.090.189],
	['Altstadt','Unterhaltung'],
	['Snack'],
	[750,500],
	[570, 1440],
	[120]).

event(	'Strandbad',
	[54.329695,13.083078],
	['Wasser','Sport','Schwimmen','Unterhaltung'],
	['Snack','Mittag','Cafe','Fast-Food','guenstige Preisklasse'],
	[0,0],
	[0, 1440],
	[180]).

event(	'Katharinenkloster',
	[54.312.754,13.088.072],
	['Sightseeing','Altstadt','Kultur'],
	[],
	[400,200],
	[600, 1020],
	[30]).

event(	'Museumsspeicher',
	[54.313.346,13.091.798],
	['Sightseeing','Hafen','Kultur'],
	[],
	[400,200],
	[600, 1020],
	[30]).

event(	'Hallenkartbahn',
	[54.307.120,13.059.902],
	['Unterhaltung','Sport'],
	[],
	[1100,900],
	[900, 1320],
	[60]).

event(	'Mr. Lucky Spielhalle',
	[54.310.914,13.090.969],
	['Unterhaltung','Altstadt'],
	[],
	[0,0],
	[480, 120],
	[60]).

childCategories(['Tiere','Schwimmen','Wasser']).
adultCategories(['Bildung','Bar','Kultur','Disko']).

% Restaurants

event(	'Burgermeister',
	[54.316.155,13.091.236],
	['Altstadt'],
	['Mittag','Abend','Amerikanisch','mittlere Preisklasse'],
	[0,0],
	[720, 1260],
	[60]).

event(	'Goldener Löwe',
	[54.316.318,13.091.024],
	['Altstadt'],
	['Mittag','Cafe','Abend','Regional','Mediteran','Deftig','mittlere Preisklasse'],
	[0,0],
	[540, 1320],
	[60]).

event(	'Speicher 8',
	[54.316.200,13.097.803],
	['Hafen'],
	['Mittag','Abend','Regional','Fisch','Gourmet','gehobene Preisklasse'],
	[0,0],
	[600, 1320],
	[60]).

event(	'The Black Bull',
	[54.317.141,13.094.705],
	['Hafen'],
	['Abend','Amerikanisch','gehobene Preisklasse'],
	[0,0],
	[900, 1320],
	[60]).

event(	'Fischermanns',
	[54.316.833,13.096.586],
	['Hafen'],
	['Mittag','Cafe','Abend','Fisch','deftig','mittlere Preisklasse'],
	[0,0],
	[720, 1320],
	[60]).

event(	'Ventspils',
	[54.319.306,13.088.673],
	[],
	['Mittag','Cafe','Abend','regional','mittlere Preisklasse'],
	[0,0],
	[600, 1320],
	[60]).

event(	'Torschliesserhaus',
	[54.313.868,13.086.779],
	['Altstadt'],
	['Mittag','Abend','deftig','mittlere Preisklasse'],
	[0,0],
	[720, 1380],
	[60]).

event(	'Wallensteinkeller',
	[54.313.868,13.086.779],
	['Altstadt'],
	['Abend','Deftig','Mittlere Preisklasse'],
	[0,0],
	[1020, 1320],
	[60]).

event(	'Brasserie',
	[54.310.806,13.087.224],
	['Altstadt','Bar'],
	['Mittag','Cafe','Abend','Deftig','Mediteran','Mittlere Preisklasse'],
	[0,0],
	[540, 1440],
	[60]).

event(	'Milchbar',
	[54.311.450,13.088.747],
	['Altstadt'],
	['Mittag','Cafe','Abend','Snack','Guenstige Preisklasse'],
	[0,0],
	[600, 1260],
	[60]).

event(	'Kartoffelhaus',
	[54.311.983,13.088.343],
	['Altstadt'],
	['Mittag','Abend','Deftig','Regional','Mittlere Preisklasse'],
	[0,0],
	[660, 1350],
	[60]).

event(	'Artemis',
	[54.317.631,13.093.391],
	['Altstadt','Bar'],
	['Mittag','Abend','Griechisch','Mittlere Preisklasse'],
	[0,0],
	[690, 1410],
	[60]).

event(	'Zum Scheele',
	[54.316.578,13.092.556],
	['Altstadt'],
	['Mittag','Abend','Regional','Gourmet','Gehobene Preisklasse'],
	[0,0],
	[720, 1380],
	[60]).

event(	'Ristorante Bellini',
	[54.315.638,13.095.388],
	['Hafen','Bar'],
	['Abend','Italienisch','Mittlere Preisklasse'],
	[0,0],
	[1080, 1440],
	[60]).

event(	'Goldener Drache',
	[54.306.308,13.065.769],
	[],
	['Mittag,'Abend','Chinesisch','Guenstige Preisklasse'],
	[0,0],
	[690, 1380],
	[60]).

event(	'Fritz Braugasthaus',
	[54.314.438,13.096.734],
	['Hafen','Bar'],
	['Mittag,'Cafe','Abend','Regional','Deftig','Mittlere Preisklasse'],
	[0,0],
	[660, 1440],
	[60]).

event(	'Bodega de Luca',
	[54.316.024,13.089.814],
	['Altstadt'],
	['Mittag,'Abend','Spanisch','Gehobene Preisklasse'],
	[0,0],
	[660, 1320],
	[60]).

event(	'Wulflamstuben',
	[54.316.240,13.089.957],
	['Altstadt'],
	['Mittag,'Cafe','Abend','Deftig','Mittlere Preisklasse'],
	[0,0],
	[690, 1320],
	[60]).

event(	'Gastmahl am Sund',
	[54.316.890,13.094.575],
	['Hafen','Bar'],
	['Mittag,'Abend','Fisch','Regional','Deftig','Mittlere Preisklasse'],
	[0,0],
	[660, 1320],
	[60]).

event(	'Essbar',
	[54.314.840,13.091.069],
	['Altstadt','Bar'],
	['Mittag,'Abend','Mediterran','Italienisch','Mittlere Preisklasse'],
	[0,0],
	[720, 1410],
	[60]).

event(	'Restaurant Jorgos',
	[54.313.459,13.097.063],
	['Hafen','Bar'],
	['Mittag,'Abend','Griechisch','Mittlere Preisklasse'],
	[0,0],
	[690, 1380],
	[60]).

event(	'Dominos',
	[54.312.019,13.090.285],
	['Altstadt'],
	['Mittag,'Abend','Italienisch','Fast-Food','Guenstige Preisklasse'],
	[0,0],
	[660, 1380],
	[60]).

event(	'Salsarico',
	[54.311.074,13.090.068],
	['Altstadt','Bar'],
	['Abend','Mexikanisch','Mittlere Preisklasse'],
	[0,0],
	[1020, 1440],
	[60]).

event(	'Subway',
	[54.311.307,13.087.460],
	['Altstadt'],
	['Mittag','Abend','Fast-Food','Guenstige Preisklasse'],
	[0,0],
	[570, 1380],
	[60]).

event(	'Mc Donalds',
	[54.320.196,13.049.167],
	[],
	['Mittag','Cafe','Abend','Fast-Food','Guenstige Preisklasse'],
	[0,0],
	[420, 1440],
	[60]).

event(	'La Piazza',
	[54.311.054,13.088.865],
	['Altstadt'],
	['Mittag','Abend','Italienisch','Mittlere Preisklasse'],
	[0,0],
	[600, 1380],
	[60]).

event(	'Pizzaria Isabella',
	[54.311.295,13.088.903],
	['Altstadt'],
	['Mittag','Abend','Italienisch','Mittlere Preisklasse'],
	[0,0],
	[660, 1320],
	[60]).

event(	'China Thai Imbiss',
	[54.310.297,13.088.989],
	['Altstadt'],
	['Mittag','Abend','Chinesisch','Sushi','Guenstige Preisklasse'],
	[0,0],
	[630, 1260],
	[60]).

event(	'Fürst Wizlaw I',
	[54.318.279,13.094.300],
	['Hafen','Bar'],
	['Mittag','Cafe','Abend','Gourmet','Regional','Gehobene Preisklasse'],
	[0,0],
	[720, 1440],
	[60]).

event(	'Häncheneck',
	[54.313.836,13.096.083],
	['Hafen'],
	['Mittag','Abend','Fast-Food','Deftig','Mittlere Preisklasse'],
	[0,0],
	[660, 1260],
	[60]).

event(	'Cocobox Asia',
	[54.312.959,13.096.011],
	['Hafen'],
	['Mittag','Abend','Chinesisch','Sushi','Guenstige Preisklasse'],
	[0,0],
	[660, 1320],
	[60]).

event(	'Croque und Salate',
	[54.325.418,13.064.530],
	[],
	['Mittag','Abend','Fast-Food','Guenstige Preisklasse'],
	[0,0],
	[660, 1320],
	[60]).

event(	'Amaro Croques',
	[54.310.274,13.089.300],
	[],
	['Mittag','Abend','Fast-Food','Guenstige Preisklasse'],
	[0,0],
	[660, 1290],
	[60]).

event(	'Ben Gunn',
	[54.316.584,13.092.061],
	['Bar','Altstadt'],
	['Snacks','Guenstige Preisklasse'],
	[0,0],
	[1140, 1440],
	[60]).

event(	'KULTurschmiede',
	[54.312.893,13.093.748],
	['Bar','Altstadt'],
	['Snacks','Mittlere Preisklasse'],
	[0,0],
	[1140, 1440],
	[60]).

event(	'Coconut',
	[54.314.482,13.095.488],
	['Bar','Altstadt','Disko'],
	['Snacks','Guenstige Preisklasse'],
	[0,0],
	[1140, 1440],
	[60]).

event(	'Shisha und Cocktaillounge',
	[54.314.482,13.095.488],
	['Bar','Altstadt'],
	['Snacks','Mittlere Preisklasse'],
	[0,0],
	[1080, 1440],
	[60]).

event(	'Suthai',
	[54.312.523,13.088.364],
	['Altstadt'],
	['Mittag','Abend','Chinesisch','Sushi','Mittlere Preisklasse'],
	[0,0],
	[660, 1260],
	[60]).

*/