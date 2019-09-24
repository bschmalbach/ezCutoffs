# ezCutoffs

Grundsetzlich geht es bei meinem/unserem Programm mit dem glorreichen Namen ezCutoffs darum, Cutoffs - also Grenzwerte, für Fit Indizes in Strukturgleichungsmodellen zu genrieren. Das macht man anhand von simulierten Daten, was sich alles in R lösen lassen würde.

Ihr beiden würdet dann insofern ins Spiel kommen, dass ich das R-Paket ganz gerne auch von R loslösen wollen würde, und vor allem dann auch komfortabler umsetzen wollen würde mit GUI und so.

Der R-Befehl sehe so aus: ezcutoffs(model, data)

Wobei im Hintergrund viel individuell anpassbar ist wie zB
Anzahl der Replikationen, Welche Fit Indizes, Welcher Schätzer, Welches Alpha Niveau etc..

Man kann es entweder bei Shiny als Web App programmieren oder Alex schlug vor qt zu nutzen.

Nötige Funktionen für das Frontend sind:
-Datenimport (.csv, .sav[mittels R-package foreign])<br>
-Model Maker (Modelleingabe erleichtern indem man sie zeichnet o.ä.)
-Eingabe von Simulationsparametern
-Einstellung von Output Parametern
