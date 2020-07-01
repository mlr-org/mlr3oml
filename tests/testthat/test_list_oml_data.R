context("list_oml_data_sets")

skip_on_cran()

test_that("list_oml_data_sets", {
  tab = list_oml_data_sets(limit = 10)
  expect_data_table(tab, nrows = 10, min.cols = 10)

  expect_names(names(tab), type = "strict", must.include = c("data_id", "name", "version", "status", "NumberOfFeatures"))
})
