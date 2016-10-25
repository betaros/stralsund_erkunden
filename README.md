# Stralsund erkunden
# Noch in Entwicklung !!!

Erkunde Stralsund mit diesem hilfrreichen Tool. Es stellt dir für deinen Aufenthalt alles notwendige zur Verfügung. Gebe ein was dich interessiert und schon zeigt es dir, was du hier erleben kannst!

Worauf wartest du noch? Auf nach Stralsund und erlebe einen unvergesslichen Urlaub.

## Entwicklung

Wenn du an dem Tool mit entwickeln möchtest, dann benötigst du dafür folgendes:

- [Eclipse Neon](https://www.eclipse.org)
- [Eclipse WindowBuilder](https://eclipse.org/windowbuilder/)
- [Eclipse PDT (Prolog Develiopment Tool)](https://sewiki.iai.uni-bonn.de/research/pdt/docs/start)
- [SWI Prolog](http://www.swi-prolog.org/)

Das Projekt muss als lokales Maven Projekt importiert werden.

Um das Projekt ausführen zu können muss man folgendes machen:
1. Umgebungsvariable erstellen:
   Name: SWI_HOME_DIR
   Wert: Pfad zu Swipl
2. Pfad hinzufügen
   %SWI_HOME_DIR%\bin
3. Pfad hinzufügen
   %SWI_HOME_DIR%\lib\jpl.jar
4. In Eclipse externe JAR zum Build in Properties hinzufügen, siehe 3.

[siehe StackOverflow](http://stackoverflow.com/a/12309591)

## Verwendete Tools

- [JPL](http://www.swi-prolog.org/packages/jpl/)
- [JXMapViewer2](https://github.com/msteiger/jxmapviewer2)
