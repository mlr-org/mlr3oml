skip_on_cran()

test_that("OMLData iris arff", {
  odata = OMLData$new(61, parquet = FALSE)
  expect_oml_data(odata)
  expect_identical(odata$name, "iris")
  expect_identical(odata$nrow, 150L)
  expect_identical(odata$ncol, 5L)
  expect_identical(odata$target_names, "class")
  expect_r6(mlr3::as_task(odata), "TaskClassif")
  expect_data_table(odata$data, nrows = 150L, ncols = 5L)
})

test_that("OMLData iris parquet", {
  odata = OMLData$new(61, parquet = TRUE)
  expect_oml_data(odata)
  expect_identical(odata$name, "iris")
  expect_identical(odata$nrow, 150L)
  expect_identical(odata$ncol, 5L)
  expect_identical(odata$target_names, "class")
  expect_r6(mlr3::as_task(odata), "TaskClassif")
  expect_data_table(odata$data, nrows = 150L, ncols = 5L)
})

test_that("no default target column fails gracefully (#1)", {
  data_id = 4535L
  odata = OMLData$new(data_id)
  expect_oml_data(odata)
  expect_error(mlr3::as_task(odata), "must be available or argument")
  expect_r6(mlr3::as_task(odata, "V10"), "Task")
  expect_r6(mlr3::tsk("oml", data_id = data_id, target_names = "V10"), "Task")
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


test_that("Can open help page for OpenML Data", {
  expect_error(OMLData$new(31)$help(), regexp = NA)
})

test_that("OMLData arff fallback works when parquet does not exist", {
  odata = with_cache(OMLData$new(31, parquet = TRUE), cache = FALSE)
  odata$data
  expect_true(inherits(odata$.__enclos_env__$private$.backend, "DataBackendDuckDB"))

  odata = with_cache(OMLData$new(31, parquet = TRUE), cache = FALSE)
  odata$desc
  # non-existing file
  odata$.__enclos_env__$private$.desc$minio_url = "http://openml1.win.tue.nl/dataset31/dataset_000.pq"
  odata$data
  expect_true(inherits(odata$.__enclos_env__$private$.backend, "DataBackendDataTable"))
})

test_that("as_data_backend falls back to arff when parquet does not exist", {
  odata = with_cache(OMLData$new(31, parquet = TRUE), cache = FALSE)
  odata$desc
  # non-existing file
  odata$.__enclos_env__$private$.desc$minio_url = "http://openml1.win.tue.nl/dataset31/dataset_000.pq"
  backend = as_data_backend(odata)
  expect_r6(backend, "DataBackendDataTable")

  odata = with_cache(OMLData$new(31, parquet = TRUE), cache = FALSE)
  backend = as_data_backend(odata)
  expect_r6(backend, "DataBackendDuckDB")
})

test_that("Logicals are converted to factor", {
  odata = odt(1050)
  backend = as_data_backend(odata)
  # renaming worked
  assert_true("c" %in% backend$colnames)
  expect_class(backend$data(1, "c")[[1L]], "factor")
  expect_oml_data(odata)
})

test_that("strings and nominals are distringuished for parquet and arff files", {
  odata_pq = odt(41701, parquet = TRUE)
  dat = odata_pq$data
  expect_class(dat[["instance_id"]], "character")
  expect_class(dat[["runstatus"]], "factor")

  odata_arff = odt(41701)
  dat = odata_arff$data
  expect_class(dat[["instance_id"]], "character")
  expect_class(dat[["runstatus"]], "factor")
})

test_that("ignore columns are respected when converting to task", {
  odata = odt(6332)
  task = as_task(odata)
  expect_set_equal(odata$feature_names, task$feature_names)
})

test_that("task converter works when using feature as target", {
  odata = odt(6332)
  task = as_task(odata, target = "customer")
  expect_true(task$target_names == "customer")
  expect_true(odata$target_names %in% task$feature_names)

  expect_error(as_task(odata, target = "timestamp"))
})

test_that("task converter works when no default target is present", {
  odata = odt(493)
  target = "station_45"
  task = as_task(odata, target_names = target)
  expect_r6(task, "Task")
  expect_set_equal(task$feature_names, setdiff(odata$feature_names, target))
})

test_that("converted data_backend contains all columns", {
  odata = odt(61)
  backend = as_data_backend(odata)
  expect_set_equal(setdiff(backend$colnames, "..row_id"), odata$features$name)
})


test_that("printer works", {
  local_log_info()
  with_cache({
    oml_data = odt(id = 31)
    observed = capture.output(print(oml_data))[4:5]
    expected = c(
      "<OMLData:31:credit-g> (1000x21)",
      " * Default target: class"
    )
    expect_equal(observed, expected)
  }, cache = FALSE)
})

test_that("download runs without error", {
  local_log_info()
  # simple sanity check
  out = capture.output(with_cache(odt(31)$download(), cache = FALSE))
  expect_true(length(out) == 4L)
})
