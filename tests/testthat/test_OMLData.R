skip_on_cran()

test_that("OMLData iris arff", {
  oml_data = OMLData$new(61, parquet = FALSE)
  expect_oml_data(oml_data)
  expect_identical(oml_data$name, "iris")
  expect_identical(oml_data$nrow, 150L)
  expect_identical(oml_data$ncol, 5L)
  expect_identical(oml_data$target_names, "class")
  expect_r6(mlr3::as_task(oml_data), "TaskClassif")
  expect_data_table(oml_data$data, nrows = 150L, ncols = 5L)
})

test_that("OMLData iris parquet", {
  oml_data = OMLData$new(61, parquet = TRUE)
  expect_oml_data(oml_data)
  expect_identical(oml_data$name, "iris")
  expect_identical(oml_data$nrow, 150L)
  expect_identical(oml_data$ncol, 5L)
  expect_identical(oml_data$target_names, "class")
  expect_r6(mlr3::as_task(oml_data), "TaskClassif")
  expect_data_table(oml_data$data, nrows = 150L, ncols = 5L)
})

test_that("no default target column fails gracefully (#1)", {
  data_id = 4535L
  oml_data = OMLData$new(data_id, FALSE)
  expect_oml_data(oml_data)
  expect_error(mlr3::as_task(oml_data), "default target attribute")
  expect_task(mlr3::as_task(oml_data, "V10"))
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
  odata = OMLData$new(data_id, FALSE)
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

test_that("parquet works", {
  odata_parquet = OMLData$new(61, FALSE, parquet = TRUE)
  b0 = mlr3misc::get_private(odata_parquet)$.get_backend()
  b1 = mlr3misc::get_private(odata_parquet)$.get_backend()
  expect_true(inherits(b0, "DataBackendDuckDB"))
  odata_arff = OMLData$new(61, FALSE, parquet = FALSE)
  b2 = mlr3misc::get_private(odata_arff)$.get_backend()
  expect_true(inherits(b2, "DataBackendDataTable"))
  expect_identical(b0, b1)
  data_parquet = odata_parquet$data
  data_arff = odata_arff$data
  expect_set_equal(
    names(data_parquet),
    names(data_arff)
  )
  expect_true(nrow(data_parquet) == nrow(data_arff))
})

test_that("Caching works with parquet and custom cache path", {
  dir = tempfile()
  odata = OMLData$new(9, parquet = TRUE, cache = dir)
  odata$desc
  dat = odata$data
  files = list.files(odata$cache_dir)
  expect_set_equal(c("data_desc", "data_features", "data_parquet", "version.json"), files)
  expect_true("9.qs" %in% list.files(file.path(dir, "data_desc")))
  expect_true("9.qs" %in% list.files(file.path(dir, "data_features")))
  expect_true("9.parquet" %in% list.files(file.path(dir, "data_parquet")))

  odata = OMLData$new(9, parquet = FALSE, cache = dir)
  odata$data
  files = list.files(dir)
  expect_true("data" %in% files)
  expect_true("9.qs" %in% list.files(file.path(dir, "data")))
})

test_that("Caching works with parquet and test server", {
  dir = file.path(R_user_dir("mlr3oml", "cache"), "test_server")
  with_test_server()
  odata = OMLData$new(9, parquet = TRUE, cache = TRUE, server = "https://test.openml.org/api/v1")
  odata$desc
  dat = odata$data
  files = list.files(odata$cache_dir)
  expect_true("9.qs" %in% list.files(file.path(dir, "data_desc")))
  expect_true("9.qs" %in% list.files(file.path(dir, "data_features")))
  expect_true("9.parquet" %in% list.files(file.path(dir, "data_parquet")))

  odata = OMLData$new(9, parquet = FALSE, cache = dir)
  odata$data
  files = list.files(dir)
  expect_true("data" %in% files)
  expect_true("9.qs" %in% list.files(file.path(dir, "data")))
})
