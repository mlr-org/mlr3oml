skip_on_cran()

test_that("OMLData iris", {
  oml_data = OMLData$new(61)
  expect_oml_data(oml_data)

  expect_identical(oml_data$name, "iris")
  expect_identical(oml_data$nrow, 150L)
  expect_identical(oml_data$ncol, 5L)
  expect_identical(oml_data$target_names, "class")
  expect_r6(oml_data$task(), "TaskClassif")

  data = oml_data$data
  expect_data_table(data, nrows = 150L, ncols = 5L)
})

test_that("data backend", {
  oml_data = OMLData$new(61)
  expect_backend(as_data_backend(oml_data))
})

test_that("no default target column fails gracefully (#1)", {
  data_id = 4535L
  oml_data = OMLData$new(data_id)
  expect_oml_data(oml_data)
  expect_error(oml_data$task(), "default target attribute")
  expect_task(oml_data$task("V10"))
  expect_task(mlr3::tsk("oml", data_id = data_id, target_names = "V10"))
})

test_that("arff with wrong quotes", {
  odata = OMLData$new(42585L)
  tab = odata$data
  expect_data_table(tab, ncols = 7, nrows = 344)
  expect_factor(tab$species, levels = c("Adelie", "Gentoo", "Chinstrap"))

  otask = OMLTask$new(168746L)
  expect_data_table(otask$data$data, nrows = otask$nrow, ncols = otask$ncol)
})

test_that("fallback for sparse files", {
  data_id = 292L
  odata = OMLData$new(data_id)
  if (requireNamespace("RWeka", quietly = TRUE)) {
    expect_data_table(odata$data)
  } else {
    expect_error(odata$data, "RWeka")
  }
})

test_that("unquoting works", {
  task_id = 3L
  expect_false(anyMissing(OMLTask$new(task_id)$data$data))
})

test_that("TaskSurv creation", {
  skip_if_not_installed("mlr3proba")
  data_id = 1228
  odata = OMLData$new(data_id)
  expect_class(odata$task(c("time", "event")), "TaskSurv")
})
