# Schiffe Versenken

Entwickelt von Fabian Sinning und Dominik Schmidt in Haskell. Es wurde im Rahmen der Lehrveranstaltung *Funktionale Programmierung* im Wintersemester 18/19 an der Hochschule München geschrieben. Sinn und Zweck dieses Projekts war das Erlernen von Konzepten und Paradigmen in der funktionalen Programmierung i.V.m. der Sprache Haskell. 
### Randnotiz:
Das Projekt wurde über die gesamte Dauer des Semesters entwickelt. Durch eine grundlegende Änderung in der Projektstruktur und Fehlern, die anfangs gemacht wurden, sind die meisten Commits nicht mehr verfügbar. Es soll jedoch nicht der Eindruck entstehen, dass das Projekt innerhalb kurzer Zeit "aus dem Hut gezaubert" wurde.

## About

Das Spiel ist dafür gedacht, dass es von zwei Spielern über ein Netzwerk gespielt werden kann. Die Regeln sind wie üblich beim Schiffe versenken, nachzulesen unter https://de.wikipedia.org/wiki/Schiffe_versenken.

## Build

    stack build
   

## Spielstart

    stack exec Studienarbeit-exe
    
Der Server wird nun gestartet und ist unter 127.0.0.1:8023 erreichbar. 

## Spielverlauf

Nachdem das Spiel im Browser aufgerufen wurde können die Schiffe platziert werden. Danach kann das Spiel mit einem Klick auf *play* gestartet werden. *reset* lässt den Spieler seine Schiffe erneut platzieren. Haben beide Spieler ihre Schiffe platziert kann begonnen werden. 
