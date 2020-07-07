context("manual, expensive tests")

skip("Expensive manual test")

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

test_that("study 99 can be loaded and parsed", {
  data_ids = list_oml_data_sets(tag = "study_99")$data_id
  cache = FALSE
  options(warn = 2L)

  for (data_id in data_ids) {
    odata = OMLData$new(data_id, cache = cache)
    expect_count(odata$id)
    expect_identical(odata$id, data_id)
    if (isFALSE(cache)) {
      expect_false(odata$cache_dir)
    } else {
      expect_string(odata$cache_dir)
    }

    expect_string(odata$name, min.chars = 1L)
    expect_list(odata$desc)
    expect_identical(odata$desc$id, data_id)

    data = odata$data
    expect_data_table(data, nrows = odata$nrow, ncols = odata$ncol)
    expect_names(odata$feature_names, "strict")
    expect_names(odata$target_names, "strict")
    expect_disjunct(odata$target_names, odata$feature_names)
    expect_set_equal(names(data), c(odata$feature_names, odata$target_names))

    qualities = odata$qualities
    expect_data_table(qualities, ncols = 2L, key = "name", min.rows = 2L)
    expect_character(qualities$name, any.missing = FALSE, min.chars = 1L)
    expect_numeric(qualities$value, any.missing = TRUE)
    expect_subset(c("NumberOfFeatures", "NumberOfInstances"), qualities$name)

    features = odata$features
    expect_data_table(features, min.cols = 8L, key = "index")
    expect_integer(features$index, lower = 0L, upper = nrow(features))
    expect_subset(c(odata$target_names, odata$feature_names), features$name)
    expect_set_equal(c(odata$target_names, odata$feature_names), features[!is_row_identifier & !is_ignore, name])
    expect_equal(sum(features$is_target), 1) # only classif and regr
    expect_logical(features$is_ignore, any.missing = FALSE)
    expect_logical(features$is_row_identifier, any.missing = FALSE)
    expect_integer(features$number_of_missing_values, lower = 0L, upper = odata$nrow, any.missing = FALSE)
    expect_list(features$nominal_value, types = c("NULL", "character"))

    task = odata$task()
    expect_task(task)
    expect_identical(odata$nrow, task$nrow)
    expect_identical(odata$ncol, task$ncol)
    expect_set_equal(odata$feature_names, task$feature_names)
    expect_set_equal(odata$target_names, task$target_names)
  }
})
