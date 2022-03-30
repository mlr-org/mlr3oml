test_that("roundtrips", {
  N = 10
  dataset = data.frame(
    date = Sys.Date() + 1:10,
    time = round(Sys.time() + 1:10, "secs"),
    lgl = sample(0:1, 10, replace = TRUE) == 1L,
    int = sample(N),
    dbl = runif(10),
    fct = factor(sample(letters[1:2], 10, replace = TRUE))
  )

  path = tempfile("mlr3oml_test_", fileext = ".arff")

  write_arff(dataset, path)
  read = setDF(read_arff(path))

  for (col in c("int", "dbl", "fct")) {
    expect_equal(dataset[[col]], read[[col]], label = col)
  }

  expect_factor(read$lgl, levels = c("FALSE", "TRUE"))
  expect_posixct(read$date)
  expect_posixct(read$time)
  expect_numeric(as.numeric(read$time - dataset$time), lower = 0, upper = sqrt(.Machine$double.eps))

  file.remove(path)
})
