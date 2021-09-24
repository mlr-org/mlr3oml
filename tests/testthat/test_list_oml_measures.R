skip_on_cran()

test_that("list_oml_measures", {
  tab = list_oml_measures()
  expect_data_table(tab, min.rows = 60)

  expect_names(names(tab), type = "strict",
    must.include = "measure")

  expect_subset(c("precision", "recall"), tab$measure)
})
