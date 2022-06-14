test_that("as_parquet_backend works", {
  odata = OMLData$new(31, cache = FALSE, parquet = TRUE)
  path = tempfile(fileext = ".pq")
  b = as_parquet_backend(odata, path = path)


})
