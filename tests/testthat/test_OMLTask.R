skip_on_cran()

test_that("OMLTask iris", {
  oml_task = OMLTask$new(59)
  expect_oml_task(oml_task)

  expect_string(oml_task$name, pattern = "iris")
  expect_identical(oml_task$nrow, 150L)
  expect_identical(oml_task$ncol, 5L)
  expect_identical(oml_task$target_names, "class")
  expect_r6(oml_task$task, "TaskClassif")
})

test_that("data backend", {
  oml_task = OMLTask$new(59)
  expect_backend(as_data_backend(oml_task))
})

test_that("TaskSurv", {
  skip_if_not_installed("mlr3proba")
  oml_task = OMLTask$new(7304)
  expect_class(oml_task$task, "TaskSurv")
})
