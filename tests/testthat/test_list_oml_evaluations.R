skip_on_cran()

test_that("list_oml_evaluations", {
  tab = list_oml_evaluations(limit = 10)
  expect_data_table(tab, nrows = 10)

  expect_names(names(tab), type = "strict",
    must.include = c("run_id", "task_id", "setup_id", "flow_id", "flow_name", "area_under_roc_curve"))

  expect_data_table(list_oml_evaluations(run_id = 999999999), nrows = 0L)
})
