context("OMLData")

test_that("OMLData iris", {
  oml_data = OMLData$new(61)
  expect_oml_data(oml_data)

  expect_identical(x$name, "iris")
  expect_identical(x$nrow, 150L)
  expect_identical(x$ncol, 5L)
  expect_identical(x$target_names, "class")
  expect_is(x$task, "TaskClassif")
})
