library(checkmate)
lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]$", full.names = TRUE), source)
lapply(list.files(system.file("testthat", package = "mlr3oml"), pattern = "^helper.*\\.[rR]$", full.names = TRUE), source)
