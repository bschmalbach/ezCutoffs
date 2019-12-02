### ezCutoffs 1.0.1

<ul>
  <li>changed the missing_data argument to logical.</li>
  <li>missing_data = "T" now also works correctly when not using missing = "fiml" estimation, leading to list-wise deletion.</li>
  <li>simulation statistics now includes information as to whether FIML estimation or list-wise deletion was used.</li>
  <li>fixed a bug with the grouping variable in multigroup designs when no empirical data is given</li>
  <li>fixed a bug that forced standard Maximum Likelihood estimation in the simulations.</li>
  <li>fixed a bug with the assignment of missings in the simulated data sets.</li>
  <li>split sub-functions internally.</li>
</ul>
