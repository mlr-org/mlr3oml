context("OMLData")

skip_on_cran()

test_that("OMLData iris", {
  oml_data = OMLData$new(61)
  expect_oml_data(oml_data)

  expect_identical(oml_data$name, "iris")
  expect_identical(oml_data$nrow, 150L)
  expect_identical(oml_data$ncol, 5L)
  expect_identical(oml_data$target_names, "class")
  expect_is(oml_data$task(), "TaskClassif")

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

test_that("data is cached", {
  path = tempfile()

  oml_data = OMLData$new(9, cache = path)
  expect_file_exists(file.path(path, "version.json"))

  expect_list(oml_data$desc)
  path_desc = file.path(path, "data_desc", "9.qs")
  expect_file_exists(path_desc)
  expect_equal(oml_data$desc, qs::qread(path_desc))

  expect_data_table(oml_data$data)
  path_data = file.path(path, "data", "9.qs")
  expect_file_exists(path_data)
  expect_equal(oml_data$data, qs::qread(path_data))

  # ensure that cache is not overwritten
  mtime_before = file.mtime(path_data)
  OMLData$new(9, cache = path)$data
  expect_equal(mtime_before, file.mtime(path_data))

  # increase version number of data desc
  env = get("CACHE", envir = asNamespace("mlr3oml"))
  env$initialized = setdiff(env$initialized, path)
  version_before = env$versions$data
  on.exit({ env$versions$data = version_before })
  env$versions$data = 9999

  # ensure that cache gets invalidated
  oml_data = OMLData$new(9, cache = path)
  expect_false(test_file_exists(path_data))
  force(oml_data$data)
  expect_lt(mtime_before, file.mtime(path_data))
})
