# ezCutoffs

Grundsetzlich geht es bei unserem Programm mit dem Namen ezCutoffs darum, Cutoffs - also Grenzwerte, für Fit Indizes in Strukturgleichungsmodellen zu genrieren. Das macht man anhand von simulierten Daten, was sich alles in R lösen lassen würde.

Ihr beiden würdet dann insofern ins Spiel kommen, dass wir das R-Paket ganz gerne auch von R loslösen wollen würde, und vor allem dann auch komfortabler umsetzen wollen würde mit GUI und so.

Der R-Befehl sieht so aus: ezcutoffs(model, data, ...)

Wobei unter ... viel anpassbar ist wie zB
Anzahl der Replikationen, Welche Fit Indizes, Welcher Schätzer, Welches Alpha Niveau etc.

Man kann es entweder bei Shiny als Web App programmieren oder Alex schlug vor qt zu nutzen.

Nötige Funktionen für das Frontend sind:
<ul>
  <li>Datenimport (.csv, .sav[mittels R-package foreign])</li>
  <li>Model Maker (Modelleingabe erleichtern indem man sie zeichnet o.ä.)</li>
  <li>Eingabe von Simulationsparametern</li>
  <li>Einstellung von Output Parametern</li>
</ul>
