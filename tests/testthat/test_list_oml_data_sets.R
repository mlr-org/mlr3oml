skip_on_cran()

test_that("list_oml_data_sets", {
  tab = list_oml_data_sets(limit = 10)
  expect_data_table(tab, nrows = 10, min.cols = 10)

  expect_names(names(tab), type = "strict",
    must.include = c("data_id", "name", "version", "status", "NumberOfFeatures"))

  expect_data_table(list_oml_data_sets(data_id = c(9, 11)), nrows = 2)
  expect_data_table(list_oml_data_sets(data_id = 1), nrows = 0L, ncols = 0L)

  expect_data_table(list_oml_data_sets(data_id = 999999999), nrows = 0L)
})
