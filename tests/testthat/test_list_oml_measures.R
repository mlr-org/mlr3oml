skip_on_cran()

test_that("list_oml_measures", {
  tab = list_oml_measures()
  expect_data_table(tab, min.rows = 60)

  expect_names(names(tab),
    type = "strict",
    must.include = "measure"
  )

  expect_subset(c("precision", "recall"), tab$measure)
})

# Test server not working properly
# test_that("list_oml_measures", {
#   tab = list_oml_measures(test_server = TRUE)
#   expect_data_table(tab)
# })
