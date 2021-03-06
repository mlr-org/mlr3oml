skip_if_not_installed("RWeka")

test_that("Construct task from dict", {
  path = tempfile()
  RWeka::write.arff(iris, path)
  tab = read_arff(path)
  expect_data_table(tab, ncol = 5, nrow = 150, any.missing = FALSE)
  expect_factor(tab$Species)
  expect_numeric(tab$Sepal.Length)
})

test_that("task with single quotes", {
  skip_on_cran()
  expect_data_table(OMLData$new(42727)$data)
})
