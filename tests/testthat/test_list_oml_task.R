skip_on_cran()

test_that("list_oml_tasks", {
  tab = list_oml_tasks(limit = 10)
  expect_data_table(tab, nrows = 10, min.cols = 10)

  expect_names(names(tab),
    type = "strict",
    must.include = c("task_id", "task_type", "data_id", "name", "status", "NumberOfFeatures")
  )

  expect_data_table(list_oml_tasks(task_id = c(9, 11)), nrows = 2)
  expect_data_table(list_oml_tasks(task_id = 1), nrows = 0L, ncols = 0L)

  expect_data_table(list_oml_tasks(task_id = 999999999), nrows = 0L)
})

test_that("Can query tasks of certain type", {
  tab = list_oml_tasks(type = "2", limit = 10)
  expect_true(unique(tab$task_type) == "Supervised Regression")

  tab = list_oml_tasks(type = "1", limit = 10)
  expect_true(unique(tab$task_type) == "Supervised Classification")
})

# Test server not working properly
# test_that("list_oml_tasks test server", {
#   tab = list_oml_tasks(limit = 10, test_server = TRUE)
#   expect_data_table(tab, nrows = 10, min.cols = 10)
# })
#
