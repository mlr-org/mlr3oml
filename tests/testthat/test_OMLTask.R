skip_on_cran()

test_that("OMLTask iris", {
  oml_task = OMLTask$new(59, FALSE)
  expect_oml_task(oml_task)
  expect_string(oml_task$name, pattern = "iris")
  expect_identical(oml_task$nrow, 150L)
  expect_identical(oml_task$ncol, 5L)
  expect_identical(oml_task$target_names, "class")
})

test_that("data backend", {
  oml_task = OMLTask$new(59)
  expect_backend(mlr3::as_data_backend(oml_task))
})

test_that("Task 1 works", {
  oml_task = OMLTask$new(1)
  expect_oml_task(oml_task)
})

test_that("Task 1 works with arff", {
  oml_task = OMLTask$new(1, parquet = FALSE)
  expect_oml_task(oml_task)
})

test_that("Task 100 works", {
  oml_task = OMLTask$new(1)
  expect_oml_task(oml_task)
})

test_that("OpenML CC-18 should work: arff", {
  task_ids = OMLCollection$new(99)$task_ids
  # n = length(task_ids)
  # task_ids = sample(task_ids, n)
  task_types = c(
    "Supervised Regression",
    "Supervised Classification"
  )
  for (task_id in task_ids) {
    task = OMLTask$new(task_id, parquet = FALSE)
    # print(task_id)
    if (task$task_type %in% task_types) {
      expect_oml_task(task)
    }
  }
})

test_that("OpenML CC-18 should work: parquet", {
  task_ids = OMLCollection$new(99)$task_ids
  # n = length(task_ids)
  # task_ids = sample(task_ids, n)
  task_types = c(
    "Supervised Regression",
    "Supervised Classification"
  )
  for (task_id in task_ids) {
    task = OMLTask$new(task_id, parquet = TRUE)
    # print(task_id)
    if (task$task_type %in% task_types) {
      expect_oml_task(task)
    }
  }
})

test_that("parquet works", {
  otask = OMLTask$new(31, parquet = TRUE)
  as_task(otask)
  expect_true(inherits(as_data_backend(otask), "DataBackendDuckDB"))
})


test_that("OMLTask components inherit correct cache directory", {
  dir = tempfile()
  orun = OMLTask$new(50, cache = dir)
  expect_true(orun$data$cache_dir == dir)
})

test_that("Error when task does not provide task_splits", {
  otask = OMLTask$new(147517)
  expect_error(as_resampling(otask), "OpenML task with id")
})

test_that("Can open help page for OpenML Task", {
  expect_error(OMLTask$new(31)$help(), regexp = NA)
})
