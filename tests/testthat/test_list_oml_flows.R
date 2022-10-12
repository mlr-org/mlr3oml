skip_on_cran()

test_that("list_oml_flows", {
  tab = list_oml_flows(limit = 10)
  expect_data_table(tab, nrows = 10)

  expect_names(names(tab),
    type = "strict",
    must.include = c("flow_id", "name", "full_name", "version", "uploader")
  )

  expect_data_table(list_oml_flows(uploader = 999999999), nrows = 0L)
})

# # Test server not working properly
# test_that("list_oml_flows", {
#   tab = list_oml_flows(limit = 10, test_server = TRUE)
#   expect_data_table(tab)
# })
