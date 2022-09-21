skip_on_cran()

test_that("list_oml_collections", {
  tab = list_oml_collections()
  expect_data_table(tab, nrows = 10, min.cols = 10)

  expect_names(names(tab),
    type = "strict",
    must.include = c("id", "alias", "main_entity_type", "name", "status", "creator")
    )
})


test_that("list_oml_tasks test server", {
  tab = list_oml_collections(limit = 10, test_server = TRUE)
  expect_data_table(tab, nrows = 10, min.cols = 10)
})

