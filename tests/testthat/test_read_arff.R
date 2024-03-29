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
  odata = odt(42727, parquet = FALSE)
  expect_data_table(odata$data)
})

test_that("64 bit ints are converted to double", {
  skip_on_cran()

  id = 42572
  col = "f1c272f04"

  odata = with_cache(odt(id, parquet = FALSE), cache = FALSE)
  expect_equal(as.character(odata$features[.(col), data_type, on = "name"]), "numeric")
  expect_double(odata$data[[col]], any.missing = FALSE)
})
