skip_on_cran()

test_that("OMLTask iris", {
  otask = OMLTask$new(59, FALSE)
  expect_oml_task(otask)
  expect_string(otask$name, pattern = "iris")
  expect_identical(otask$nrow, 150L)
  expect_identical(otask$ncol, 5L)
  expect_identical(otask$target_names, "class")
})

test_that("data backend", {
  otask = OMLTask$new(59)
  expect_backend(mlr3::as_data_backend(otask))
})

test_that("Task 1 works", {
  otask = OMLTask$new(1)
  expect_oml_task(otask)
})

test_that("Task 1 works with arff", {
  otask = OMLTask$new(1, parquet = FALSE)
  expect_oml_task(otask)
})

test_that("Task 100 works", {
  otask = OMLTask$new(1)
  expect_oml_task(otask)
})

test_that("OpenML CC-18 should work: arff", {
  task_ids = OMLCollection$new(99)$task_ids
  # https://github.com/jeroen/curl/issues/430#issuecomment-3801914339
  task_ids <- setdiff(task_ids, 167124)
  # n = length(task_ids)
  # task_ids = sample(task_ids, n)
  task_types = c(
    "Supervised Regression",
    "Supervised Classification"
  )
  for (task_id in task_ids) {
    otask = OMLTask$new(task_id, parquet = FALSE)
    # print(task_id)
    if (otask$task_type %in% task_types) {
      expect_oml_task(otask)
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
    otask = OMLTask$new(task_id, parquet = TRUE)
    # print(task_id)
    if (otask$task_type %in% task_types) {
      expect_oml_task(otask)
    }
  }
})

test_that("parquet works", {
  otask = OMLTask$new(31, parquet = TRUE)
  as_task(otask)
  expect_true(inherits(as_data_backend(otask), "DataBackendDuckDB"))
})


test_that("Error when task does not provide task_splits", {
  otask = OMLTask$new(147517)
  expect_error(as_resampling(otask), "OpenML task with id")
})

test_that("Can open help page for OpenML Task", {
  expect_error(OMLTask$new(31)$help(), regexp = NA)
})

test_that("ignored features are respected when creating tasks", {
  otask = otsk(14954)
  task = as_task(otask)
  expect_set_equal(task$feature_names, otask$feature_names)
})


test_that("printer works", {
  local_log_info()

  oml_task = with_cache({
    oml_task = otsk(31)
    expected = c(
      "<OMLTask:31>",
      " * Type: Supervised Classification",
      " * Data: credit-g (id: 31; dim: 1000x21)",
      " * Target: class",
      " * Estimation: crossvalidation (id: 1; repeats: 1, folds: 10)"
    )
    observed = capture.output(print(oml_task))[5:9]
    expect_equal(observed, expected)
  }, cache = FALSE)

})


test_that("download runs without error", {
  local_log_info()
  # simple sanity check
  out = capture.output(with_cache(otsk(31)$download(), cache = FALSE))
  # data + task_desc + task_splits
  expect_true(length(out) == 6L)
})
