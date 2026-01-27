skip_on_cran()

test_that("list_oml_data", {
  tab = list_oml_data(limit = 10)
  expect_data_table(tab, nrows = 10, min.cols = 10)

  expect_names(names(tab),
    type = "strict",
    must.include = c("data_id", "name", "version", "status", "NumberOfFeatures")
  )

  expect_data_table(list_oml_data(data_id = c(9, 11)), nrows = 2)
  expect_data_table(list_oml_data(data_id = 1), nrows = 0L, ncols = 0L)

  expect_data_table(list_oml_data(data_id = 999999999), nrows = 0L)
})

test_that("list_oml_data uploader filter", {
  # uploader 1 is the OpenML admin who uploaded many datasets
  tab = list_oml_data(uploader = 1, limit = 5)
  expect_data_table(tab, min.rows = 1, min.cols = 10)

  # non-existent uploader returns empty data.table
  expect_data_table(list_oml_data(uploader = 999999999, limit = 5), nrows = 0L)
})


# listing data from test server does not work:
# https://github.com/openml/OpenML/issues/1159#issuecomment-1225720284
