if (requireNamespace("testthat", quietly = TRUE)) {
  library("testthat")
  library("checkmate")
  library("mlr3oml")
  library("mlr3")
  test_check("mlr3oml")
}
