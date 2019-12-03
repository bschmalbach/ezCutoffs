# on attach message
.onAttach <-
  function(libname,pkgname) {
    packageStartupMessage('ezCutoffs is in development. Please report any bugs and check https://github.com/bschmalbach/ezCutoffs for newer versions.')
  }
# define some global variables
empirical_fit <- fit <- dots <- fit_distributions <- NULL
