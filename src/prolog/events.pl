:- module('events', [event/7,hotel/4,vehicle/3,childCategories/1,adultCategories/1]).

/*
* Wissensdatenbank
*/
	

/*
Zeitrechnung findet in Minuten statt
z.B. 10 Uhr ist 600
z.B. 20 Uhr ist 1200

Maximaler Wert ist 1440
Minimaler Wert ist 0
*/
	

/*
Hotels
Unterkunft für die Nacht
hotel('Name',[Long,Lat],Preis pro Doppelzimmer in Cent pro Nacht, Kategorie).
*/

/*
hotel('X Sterne Hotel',[54.320021,13.043840],1000,[0]).
hotel('1 Sterne Hotel',[54.320021,13.043840],2000,[1]).
hotel('2 Sterne Hotel',[54.320021,13.043840],3000,[2]).
hotel('3 Sterne Hotel',[54.320021,13.043840],4000,[3]).
hotel('4 Sterne Hotel',[54.320021,13.043840],5000,[4]).	
hotel('5 Sterne Hotel',[54.320021,13.043840],6000,[5]).	
*/

hotel( 'Hotel am Jungfernstieg',
 	[54.309885,13.078082],
 	3900,
 	[3]).

hotel( 'arcona Baltic',
 	[54.309975,13.098187],
 	6500,
 	[4]).

hotel( 'Wyndham Stralsund',
 	[54.320051,13.043869],
 	5900,
 	[4]).

hotel( 'Hotel An den Bleichen',
 	[54.318767,13.074067],
 	6500,
 	[3]).

hotel( 'Kurhaus Devin',
 	[54.266473,13.137222],
 	7600,
 	[3]).

hotel( 'Altstadt Hotel zur Post',
 	[54.310975,13.086837],
 	5100,
 	[4]).

hotel( 'Altstadt Hotel Peiss',
 	[54.310516,13.086334],
 	7000,
 	[3]).

hotel( 'Hiddenseer Hotel',
 	[54.315087,13.098205],
 	5100,
 	[3]).

hotel( 'Hotel Hafenresidenz',
 	[54.318208,13.093879],
 	8200,
 	[4]).

hotel( 'Hotel Kontorhaus',
 	[54.314180,13.098183],
 	9400,
 	[3]).

hotel( 'Pension Villa Beer',
 	[54.314591,13.076280],
 	4900,
 	[0]).

hotel( 'Pension Quast',
 	[54.288643,13.095265],
 	6800,
 	[3]).

hotel( 'Lindenhotel',
 	[54.308867,13.036271],
 	5400,
 	[3]).

hotel( 'InterCityHotel Stralsund',
 	[54.308637,13.079017],
 	4800,
 	[3]).

hotel( 'Ostseehotel Stralsund',
 	[54.302462,13.015229],
 	4300,
 	[3]).

hotel( 'Pension Am Ozeaneum',
 	[54.315315,13.095847],
	 6200,
 	[0]).

hotel( 'Hotel Scheelehof',
 	[54.316621,13.092684],
 	12500,
 	[4]).

hotel( 'Schweriner Hof',
 	[54.310539,13.087392],
 	4400,
 	[2]).

/*----------------------------------------------------------------------------------------------*/	


/*
* vehicle (Name, Festpreis [Normal, Ermäßigt], Geschwindigkeit in km/Stunde)
*/
vehicle('Car', [100, 100], 35).
vehicle('Bus', [180, 120], 25).
vehicle('Bike', [0,0], 15).
vehicle('Foot', [0,0], 6).


/*----------------------------------------------------------------------------------------------*/	


/*
* Kategorien für Kinder und Erwachsene festlegen
*/
childCategories(['Tiere','Unterhaltung','Wasser']).
adultCategories(['Bildung','Bar','Kultur']).


/*----------------------------------------------------------------------------------------------*/	

/*
Events
event(Name,[Long, Lat],[Event-Kategorien],[Foot-Kategorien],[Öffnung, Schließung],[Preis Erw., Preis Kind],[Dauer])
*/

event(	'Ozeaneum',
	[54.315691,13.097708],
	['Bildung','Museum','Wasser','Hafen','Tiere','Kultur'],
	['Cafe','Mittag','regional','deftig','mittlere Preisklasse'],
	[1700,1200],
	[570, 1080],
	[120]).

event(	'Rathaus',
	[54.315605,13.090367],
	['Sightseeing','Altstadt','Kultur'],
	[],
	[0,0],
	[360, 1200],
	[15]).

event(	'Deutsches Meeresmuseum',
	[54.312285,13.086757],
	['Museum','Altstadt','Bildung','Wasser','Tiere','Kultur'],
	['Mittag','Cafe','Snack','guenstige Preisklasse'],
	[1000,800],
	[600, 1020],
	[120]).

event(	'Gorch Fock',
	[54.316692,13.098349],
	['Sightseeing','Hafen','Wasser','Kultur'],
	[],
	[490,250],
	[600, 960],
	[60]).

event(	'St Marienkirche',
	[54.309919,13.087711],
	['Sightseeing','Altstadt','Kultur'],
	[],
	[0,0],
	[540, 1080],
	[30]).

event(	'Nikolaikirche',
	[54.315362,13.090987],
	['Sightseeing','Altstadt','Kultur'],
	[],
	[0,0],
	[540, 1080],
	[30]).

event(	'Kulturhistorisches Museum Stralsund',
	[54.312564,13.088054],
	['Museum','Altstadt','Bildung','Kultur'],
	[],
	[500,250],
	[600, 1020],
	[120]).

event(	'Tierpark Stralsund',
	[54.319651,13.050657],
	['Unterhaltung','Tiere', 'Outdoor'],
	['Mittag','Cafe','Abend','guenstige Preisklasse', 'Snack'],
	[700,400],
	[540, 1110],
	[180]).

event(	'Altstadt Shopping',
	[54.314987,13.090021],
	['Einkaufen','Altstadt'],
	[],
	[0,0],
	[570, 1140],
	[120]).

event(	'Theater Stralsund',
	[54.317597,13.088872],
	['Kultur','Altstadt','Unterhaltung'],
	['Mittag','Snack','mittlere Preisklasse'],
	[2000,2000],
	[540, 1320],
	[120]).

event(	'Strelapark',
	[54.320700,13.047758],
	['Einkaufen'],
	['Mittag','Cafe','Snack','Fast-Food','deftig','mittlere Preisklasse','guenstige Preisklasse'],
	[0,0],
	[540, 1200],
	[120]).

event(	'Hansedom',
	[54.319626,13.044390],
	['Unterhaltung','Wasser','Schwimmen','Sport'],
	['Mittag','Cafe','Snack','Fast-Food','Abend','deftig','mittlere Preisklasse'],
	[1400,1150],
	[570, 1260],
	[240]).

event(	'Nautineum',
	[54.305248,13.118818],
	['Museum','Bildung','Kultur'],
	[],
	[0,0],
	[600, 960],
	[60]).

event(	'Museum für komische Kunst',
	[54.316301,13.097320],
	['Museum','Unterhaltung','Kultur','Hafen'],
	[],
	[600,400],
	[600, 1140],
	[90]).

event(	'Hafenrundfahrt',
	[54.317170,13.095108],
	['Wasser','Unterhaltung','Hafen'],
	['Mittag','Cafe','Snack','guenstige Preisklasse'],
	[900,400],
	[660, 1050],
	[60]).

event(	'Störtebeker Braumanufaktur',
	[54.291171,13.094898],
	['Bar','Unterhaltung'],
	[],
	[1150,290],
	[540, 1020],
	[90]).

event(	'Cinestar Kino',
	[54.310899,13.090189],
	['Altstadt','Unterhaltung'],
	['Snack'],
	[750,500],
	[570, 1440],
	[120]).

event(	'Strandbad',
	[54.329695,13.083078],
	['Wasser','Sport','Schwimmen','Outdoor'],
	['Snack','Mittag','Cafe','Fast-Food','guenstige Preisklasse'],
	[0,0],
	[0, 1440],
	[180]).

event(	'Katharinenkloster',
	[54.312754,13.088072],
	['Sightseeing','Altstadt','Kultur'],
	[],
	[400,200],
	[600, 1020],
	[30]).

event(	'Museumsspeicher',
	[54.313346,13.091798],
	['Sightseeing','Hafen','Kultur'],
	[],
	[400,200],
	[600, 1020],
	[30]).

event(	'Hallenkartbahn',
	[54.307120,13.059902],
	['Unterhaltung','Sport'],
	[],
	[1100,900],
	[900, 1320],
	[60]).

event(	'Mr. Lucky Spielhalle',
	[54.310914,13.090969],
	['Unterhaltung','Altstadt'],
	[],
	[0,0],
	[480, 1440],
	[60]).



/*----------------------------------------------------------------------------------------------*/	

/*
Restaurants
event(event(Name,[Long, Lat],[Event-Kategorien],[Foot-Kategorien],[Öffnung, Schließung],[Preis Erw., Preis Kind],[Dauer])

*/

event(	'Burgermeister',
	[54.316155,13.091236],
	['Altstadt'],
	['Mittag','Abend','Amerikanisch','Mittlere Preisklasse'],
	[0,0],
	[720, 1260],
	[60]).

event(	'Goldener Löwe',
	[54.316318,13.091024],
	['Altstadt'],
	['Mittag','Cafe','Abend','Regional','Mediterran','Deftig','Mittlere Preisklasse'],
	[0,0],
	[540, 1320],
	[60]).

event(	'Speicher 8',
	[54.316200,13.097803],
	['Hafen'],
	['Mittag','Abend','Regional','Fisch','Gourmet','Gehobene Preisklasse'],
	[0,0],
	[600, 1320],
	[60]).

event(	'The Black Bull',
	[54.317141,13.094705],
	['Hafen'],
	['Abend','Amerikanisch','Gehobene Preisklasse'],
	[0,0],
	[900, 1320],
	[60]).

event(	'Fischermanns',
	[54.316833,13.096586],
	['Hafen'],
	['Mittag','Cafe','Abend','Fisch','Deftig','Mittlere Preisklasse'],
	[0,0],
	[720, 1320],
	[60]).

event(	'Ventspils',
	[54.319306,13.088673],
	[],
	['Mittag','Cafe','Abend','Regional','Mittlere Preisklasse'],
	[0,0],
	[600, 1320],
	[60]).

event(	'Torschliesserhaus',
	[54.313868,13.086779],
	['Altstadt'],
	['Mittag','Abend','Deftig','Mittlere Preisklasse'],
	[0,0],
	[720, 1380],
	[60]).

event(	'Wallensteinkeller',
	[54.313868,13.086779],
	['Altstadt'],
	['Abend','Deftig','Mittlere Preisklasse'],
	[0,0],
	[1020, 1320],
	[60]).

event(	'Brasserie',
	[54.310806,13.087224],
	['Altstadt','Bar'],
	['Mittag','Cafe','Abend','Deftig','Mediterran','Mittlere Preisklasse'],
	[0,0],
	[540, 1440],
	[60]).

event(	'Milchbar',
	[54.311450,13.088747],
	['Altstadt'],
	['Mittag','Cafe','Abend','Snack','Guenstige Preisklasse'],
	[0,0],
	[600, 1260],
	[60]).

event(	'Kartoffelhaus',
	[54.311983,13.088343],
	['Altstadt'],
	['Mittag','Abend','Deftig','Regional','Mittlere Preisklasse'],
	[0,0],
	[660, 1350],
	[60]).

event(	'Artemis',
	[54.317631,13.093391],
	['Altstadt','Bar'],
	['Mittag','Abend','Griechisch','Mittlere Preisklasse'],
	[0,0],
	[690, 1410],
	[60]).

event(	'Zum Scheele',
	[54.316578,13.092556],
	['Altstadt'],
	['Mittag','Abend','Regional','Gourmet','Gehobene Preisklasse'],
	[0,0],
	[720, 1380],
	[60]).

event(	'Ristorante Bellini',
	[54.315638,13.095388],
	['Hafen','Bar'],
	['Abend','Italienisch','Mittlere Preisklasse'],
	[0,0],
	[1080, 1440],
	[60]).

event(	'Goldener Drache',
	[54.306308,13.065769],
	[],
	['Mittag','Abend','Chinesisch','Guenstige Preisklasse'],
	[0,0],
	[690, 1380],
	[60]).

event(	'Fritz Braugasthaus',
	[54.314438,13.096734],
	['Hafen','Bar'],
	['Mittag','Cafe','Abend','Regional','Deftig','Mittlere Preisklasse'],
	[0,0],
	[660, 1440],
	[60]).

event(	'Bodega de Luca',
	[54.316024,13.089814],
	['Altstadt'],
	['Mittag','Abend','Spanisch','Gehobene Preisklasse'],
	[0,0],
	[660, 1320],
	[60]).

event(	'Wulflamstuben',
	[54.316240,13.089957],
	['Altstadt'],
	['Mittag','Cafe','Abend','Deftig','Mittlere Preisklasse'],
	[0,0],
	[690, 1320],
	[60]).

event(	'Gastmahl am Sund',
	[54.316890,13.094575],
	['Hafen','Bar'],
	['Mittag','Abend','Fisch','Regional','Deftig','Mittlere Preisklasse'],
	[0,0],
	[660, 1320],
	[60]).

event(	'Essbar',
	[54.314840,13.091069],
	['Altstadt','Bar'],
	['Mittag','Abend','Mediterran','Italienisch','Mittlere Preisklasse'],
	[0,0],
	[720, 1410],
	[60]).

event(	'Restaurant Jorgos',
	[54.313459,13.097063],
	['Hafen','Bar'],
	['Mittag','Abend','Griechisch','Mittlere Preisklasse'],
	[0,0],
	[690, 1380],
	[60]).

event(	'Dominos',
	[54.312019,13.090285],
	['Altstadt'],
	['Mittag','Abend','Italienisch','Fast-Food','Guenstige Preisklasse'],
	[0,0],
	[660, 1380],
	[60]).

event(	'Salsarico',
	[54.311074,13.090068],
	['Altstadt','Bar'],
	['Abend','Mexikanisch','Mittlere Preisklasse'],
	[0,0],
	[1020, 1440],
	[60]).

event(	'Subway',
	[54.311307,13.087460],
	['Altstadt'],
	['Mittag','Abend','Fast-Food','Guenstige Preisklasse'],
	[0,0],
	[570, 1380],
	[60]).

event(	'Mc Donalds',
	[54.320196,13.049167],
	[],
	['Mittag','Cafe','Abend','Fast-Food','Guenstige Preisklasse'],
	[0,0],
	[420, 1440],
	[60]).

event(	'La Piazza',
	[54.311054,13.088865],
	['Altstadt'],
	['Mittag','Abend','Italienisch','Mittlere Preisklasse'],
	[0,0],
	[600, 1380],
	[60]).

event(	'Pizzaria Isabella',
	[54.311295,13.088903],
	['Altstadt'],
	['Mittag','Abend','Italienisch','Mittlere Preisklasse'],
	[0,0],
	[660, 1320],
	[60]).

event(	'China Thai Imbiss',
	[54.310297,13.088989],
	['Altstadt'],
	['Mittag','Abend','Chinesisch','Sushi','Guenstige Preisklasse'],
	[0,0],
	[630, 1260],
	[60]).

event(	'Fürst Wizlaw I',
	[54.318279,13.094300],
	['Hafen','Bar'],
	['Mittag','Cafe','Abend','Gourmet','Regional','Gehobene Preisklasse'],
	[0,0],
	[720, 1440],
	[60]).

event(	'Häncheneck',
	[54.313836,13.096083],
	['Hafen'],
	['Mittag','Abend','Fast-Food','Deftig','Mittlere Preisklasse'],
	[0,0],
	[660, 1260],
	[60]).

event(	'Cocobox Asia',
	[54.312959,13.096011],
	['Hafen'],
	['Mittag','Abend','Chinesisch','Sushi','Guenstige Preisklasse'],
	[0,0],
	[660, 1320],
	[60]).

event(	'Croque und Salate',
	[54.325418,13.064530],
	[],
	['Mittag','Abend','Fast-Food','Guenstige Preisklasse'],
	[0,0],
	[660, 1320],
	[60]).

event(	'Amaro Croques',
	[54.310274,13.089300],
	[],
	['Mittag','Abend','Fast-Food','Guenstige Preisklasse'],
	[0,0],
	[660, 1290],
	[60]).

event(	'Ben Gunn',
	[54.316584,13.092061],
	['Bar','Altstadt'],
	['Snacks','Guenstige Preisklasse'],
	[0,0],
	[1140, 1440],
	[60]).

event(	'KULTurschmiede',
	[54.312893,13.093748],
	['Bar','Altstadt'],
	['Snacks','Mittlere Preisklasse'],
	[0,0],
	[1140, 1440],
	[60]).

event(	'Coconut',
	[54.314482,13.095488],
	['Bar','Altstadt','Disko'],
	['Snacks','Guenstige Preisklasse'],
	[0,0],
	[1140, 1440],
	[60]).

event(	'Shisha und Cocktaillounge',
	[54.314482,13.095488],
	['Bar','Altstadt'],
	['Snacks','Mittlere Preisklasse'],
	[0,0],
	[1080, 1440],
	[60]).

event(	'Suthai',
	[54.312523,13.088364],
	['Altstadt'],
	['Mittag','Abend','Chinesisch','Sushi','Mittlere Preisklasse'],
	[0,0],
	[660, 1260],
	[60]).


/*----------------------------------------------------------------------------------------------*/	


/*
* TESTObjekte
* event(Name des Events, [Latitude, Longitude], Liste an Kategorien, Essenskategorien, Preisliste, öffnungszeiten, [Durchschnittliche Dauer]).
*/

/*
event(	'Hansedom', 
	[54.320021,13.043840],
	['Sport','Schwimmen','Sauna'],
	['Fast-Food'],
	[2222,1000],
	[600, 2100],
	[200]).

event(	'Strelapark',
	[54.320678,13.046984],
	['Einkaufen'],
	['Fast-Food'],
	[3333,1000],
	[600, 2100],
	[600]).
	
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
	[200]).
	
event(	'Nautineum',
	[54.305252,13.118912],
	['Freizeit','Museum','Bildung'],
	['Fast-Food'],
	[8111,1000],
	[900, 2100],
	[600]).
	
event(	'Marinemuseum',
	[54.309746,13.119041],
	['Freizeit','Museum','Bildung'],
	[],
	[2332,1000],
	[600, 2100],
	[450]).
	
event(	'Fachhochschule Stralsund',
	[54.339149,13.076232],
	['Bildung','Studium'],
	['Kantinenessen','Mittag'],
	[100,1000],
	[930, 2100],
	[250]).
	
event(	'Zoo',
	[54.319651,13.051815],
	['Tiere'],
	[],
	[3453,1000],
	[930, 2100],
	[300]).
	
event(	'Cinestar',
	[54.311055,13.090076],
	['Freizeit','Unterhaltung'],
	[],
	[5366,1000],
	[930, 2100],
	[200]).
	
event(	'Haus 8',
	[54.340094,13.076638],
	['Bar','Kneipe'],
	[],
	[0,0],
	[930, 2200],
	[200]).
*/