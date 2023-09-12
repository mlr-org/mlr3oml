library(mlr3misc)
code = {
  runif(1) > 0.8
}


invoke(test_that, "a", code)

