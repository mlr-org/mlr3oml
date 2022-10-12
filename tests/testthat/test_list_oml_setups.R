skip_on_cran()

test_that("list_oml_setups", {
  tab = list_oml_setups(limit = 10)
  expect_data_table(tab, nrows = 10)

  expect_names(names(tab),
    type = "strict",
    must.include = c("setup_id", "flow_id", "parameter")
  )

  expect_data_table(list_oml_setups(flow_id = 999999999), nrows = 0L)
})

# Test server not working properly
# test_that("list_oml_setups", {
#   tab = list_oml_setups(limit = 10, test_server = TRUE)
#   expect_data_table(tab)
# })
