context("list_oml_tasks")

skip_on_cran()

test_that("list_oml_tasks", {
  tab = list_oml_tasks(limit = 10)
  expect_data_table(tab, nrows = 10, min.cols = 10)

  expect_names(names(tab), type = "strict", must.include = c("task_id", "task_type", "data_id", "name", "status", "NumberOfFeatures"))
})

