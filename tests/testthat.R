if (requireNamespace("testthat", quietly = TRUE)) {
  library("testthat")
  library("checkmate")
  library("mlr3oml")
  test_check("mlr3oml")
}
