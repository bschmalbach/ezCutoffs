# ezCutoffs

Grundsetzlich geht es bei meinem/unserem Programm mit dem glorreichen Namen ezCutoffs darum, Cutoffs - also Grenzwerte, für Fit Indizes in Strukturgleichungsmodellen zu genrieren. Das macht man anhand von simulierten Daten, was sich alles in R lösen lassen würde.

Ihr beiden würdet dann insofern ins Spiel kommen, dass ich das R-Paket ganz gerne auch von R loslösen wollen würde, und vor allem dann auch komfortabler umsetzen wollen würde mit GUI und so.

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



Features, die ich noch in R implementieren will, die dann auch das Frontend teilweise tangieren würden:
<ul>
  <li>Multiple group CFAs</li>
  <ul>
    <li>generate model fit and data for all groups separately, then combine and test for invariance</li>
    <li>Konsequenz für Frontend = Man muss die Gruppenvariable deklarieren können</li>
  </ul>
</ul>
