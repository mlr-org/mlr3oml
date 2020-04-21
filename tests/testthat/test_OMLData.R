context("OMLData")

test_that("OMLData iris", {
  oml_data = OMLData$new(61)
  expect_oml_data(oml_data)

  expect_identical(oml_data$name, "iris")
  expect_identical(oml_data$nrow, 150L)
  expect_identical(oml_data$ncol, 5L)
  expect_identical(oml_data$target_names, "class")
  expect_is(oml_data$task, "TaskClassif")
})

test_that("data backend", {
  oml_data = OMLData$new(61)
  expect_backend(as_data_backend(oml_data))
})
