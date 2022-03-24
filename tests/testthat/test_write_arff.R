test_that("roundtrips", {
  N = 10
  mydata = data.frame(
    time = Sys.time() + 1:10,
    date = Sys.Date() + 1:10,
    lgl = sample(0:1, 10, replace = TRUE) == 1L,
    int = sample(N),
    dbl = runif(10),
    fct = factor(sample(letters[1:2], 10, replace = TRUE))
  )
  datasets = list(mydata, iris, mtcars, airquality)

  for (dataset in datasets) {
    rownames(dataset) = NULL
    path = tempfile("mlr3oml_test_", fileext = ".arff")

    write_arff(dataset, path)
    read = setDF(read_arff(path))
    expect_equal(dataset, read)

    file.remove(path)
  }
})
