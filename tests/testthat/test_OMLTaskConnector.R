skip_on_cran()

test_that("Construct task from dict", {
  expect_task(tsk("oml", task_id = 9))
})

test_that("Correct backend", {
  task = tsk("oml", task_id = 9, parquet = TRUE)
  expect_true(inherits(task$backend, "DataBackendDuckDB"))

  task = tsk("oml", task_id = 9, parquet = FALSE)
  expect_true(inherits(task$backend, "DataBackendDataTable"))
})
