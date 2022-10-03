skip_on_cran()

test_that("Construct task from dict", {
  expect_r6(tsk("oml", task_id = 9), "Task")
})

test_that("Correct backend", {
  # id 31 is important, because otherwise we might get a DataBackendRename
  task = tsk("oml", task_id = 31, parquet = TRUE)
  expect_true(inherits(task$backend, "DataBackendDuckDB"))

  task = tsk("oml", task_id = 31, parquet = FALSE)
  expect_true(inherits(task$backend, "DataBackendDataTable"))
})
