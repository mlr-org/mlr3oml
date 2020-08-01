context("read_arff")

test_that("Construct task from dict", {
  path = tempfile()
  foreign::write.arff(iris, path)
  tab = read_arff(path)
  expect_data_table(tab, ncol = 5, nrow = 150, any.missing = FALSE)
  expect_factor(tab$Species)
  expect_numeric(tab$Sepal.Length)
})
