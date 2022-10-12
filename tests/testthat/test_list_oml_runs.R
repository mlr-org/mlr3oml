skip_on_cran()

test_that("list_oml_runs", {
  tab = list_oml_runs(limit = 10)
  expect_data_table(tab, nrows = 10)

  expect_names(names(tab),
    type = "strict",
    must.include = c("run_id", "task_id", "setup_id", "flow_id", "uploader")
  )

  expect_data_table(list_oml_runs(run_id = 999999999), nrows = 0L)
})

# # Test server not working properly
# test_that("list_oml_runs", {
#   tab = list_oml_runs(limit = 10, test_server = TRUE)
#   expect_data_table(tab)
# })
