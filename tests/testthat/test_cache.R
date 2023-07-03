skip_on_cran()

test_that("Seperate caches are used for test and public server", {
  with_public_server()
  cachedir = tempfile()
  odata_public = OMLData$new(31, test_server = FALSE, cache = cachedir)$desc
  expect_true("31.qs" %in% list.files(file.path(cachedir, "public", "data_desc")))
  expect_true("31.qs" %nin% list.files(file.path(cachedir, "test", "data_desc")))
  with_test_server()
  odata_test = OMLData$new(31, test_server = TRUE, cache = cachedir)$desc
  expect_true("31.qs" %in% list.files(file.path(cachedir, "test", "data_desc")))
})

test_that("Caching works for parquet", {
  cachedir = tempfile()
  x = OMLData$new(31, cache = cachedir, parquet = TRUE)$data
  expect_true("31.parquet" %in% list.files(file.path(cachedir, "public", "data_parquet")))

})

test_that("Caching works with parquet and custom cache path", {
  dir = tempfile()
  odata = OMLData$new(9, parquet = TRUE, cache = dir, test_server = FALSE)
  odata$desc
  dat = odata$data
  files = list.files(file.path(odata$cache_dir, "public"))
  expect_set_equal(
    files,
    c("data_desc", "data_features", "data_parquet")
  )
  expect_true("9.qs" %in% list.files(file.path(dir, "public", "data_desc")))
  expect_true("9.qs" %in% list.files(file.path(dir, "public", "data_features")))
  expect_true("9.parquet" %in% list.files(file.path(dir, "public", "data_parquet")))

  odata = OMLData$new(9, parquet = FALSE, cache = dir, test_server = FALSE)
  odata$data
  files = list.files(file.path(dir, "public"))
  expect_true("data" %in% files)
  expect_true("9.qs" %in% list.files(file.path(dir, "public", "data")))
})

# https://github.com/openml/openml-data/issues/50
# test_that("Caching works with parquet and test server", {
#   with_test_server()
#   dir = tempfile()
#   odata = OMLData$new(9, parquet = TRUE, cache = dir, test_server = TRUE)
#   odata$desc
#   dat = odata$data
#   files = list.files(odata$cache_dir)
#   expect_true("9.qs" %in% list.files(file.path(dir, "test", "data_desc")))
#   expect_true("9.qs" %in% list.files(file.path(dir, "test", "data_features")))
#   expect_true("9.parquet" %in% list.files(file.path(dir, "test", "data_parquet")))
#
#   odata = OMLData$new(9, parquet = FALSE, cache = dir, test_server = TRUE)
#   odata$data
#   files = list.files(file.path(dir, "test"))
#   expect_true("data" %in% files)
#   expect_true("9.qs" %in% list.files(file.path(dir, "test", "data")))
# })
