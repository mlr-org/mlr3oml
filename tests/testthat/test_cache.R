skip_on_cran()

test_that("Separate caches are used for test and public server", {
  local_public_server()
  cachedir = tempfile()
  odata_public = with_cache(
    code = OMLData$new(31, test_server = FALSE)$desc,
    cache = cachedir
  )
  expect_true("31.qs2" %in% list.files(file.path(cachedir, "public", "data_desc")))
  expect_true("31.qs2" %nin% list.files(file.path(cachedir, "test", "data_desc")))
  local_test_server()
  odata_test = with_cache(
    code = OMLData$new(31, test_server = TRUE)$desc,
    cache = cachedir
  )
})

test_that("Caching works for parquet", {
  cachedir = tempfile()
  with_cache(
    OMLData$new(31, parquet = TRUE)$data,
    cache = cachedir
  )
  expect_true("31.parquet" %in% list.files(file.path(cachedir, "public", "data_parquet")))
})

test_that("Caching works with parquet and custom cache path", {
  dir = tempfile()
  with_cache({
      odata = OMLData$new(9, parquet = TRUE, test_server = FALSE)
      dat = odata$data
      files = list.files(file.path(odata$cache_dir, "public"))
      expect_set_equal(
        files,
        c("data_desc", "data_features", "data_parquet")
      )
      expect_true("9.qs2" %in% list.files(file.path(dir, "public", "data_desc")))
      expect_true("9.qs2" %in% list.files(file.path(dir, "public", "data_features")))
      expect_true("9.parquet" %in% list.files(file.path(dir, "public", "data_parquet")))
    }, cache = dir)


  with_cache({
    odata = OMLData$new(9, parquet = FALSE, test_server = FALSE)
    odata = odata$data
    files = list.files(file.path(dir, "public"))
    expect_true("data" %in% files)
    expect_true("9.qs2" %in% list.files(file.path(dir, "public", "data")))
    }, cache  = dir)
})

#https://github.com/openml/openml-data/issues/50
#test_that("Caching works with parquet and test server", {
#  local_test_server()
#  dir = tempfile()
#  odata = with_cache(
#    OMLData$new(9, parquet = TRUE, test_server = TRUE),
#    cache = dir
#  )
#  odata = odata$desc
#  dat = odata$data
#  files = list.files(odata$cache_dir)
#  expect_true("9.qs2" %in% list.files(file.path(dir, "test", "data_desc")))
#  expect_true("9.qs2" %in% list.files(file.path(dir, "test", "data_features")))
#  expect_true("9.parquet" %in% list.files(file.path(dir, "test", "data_parquet")))
#
#  odata = with_cache(
#    OMLData$new(9, parquet = FALSE, test_server = TRUE),
#    cache = dir
#  )
#  odata = odata$data
#  files = list.files(file.path(dir, "test"))
#  expect_true("data" %in% files)
#  expect_true("9.qs2" %in% list.files(file.path(dir, "test", "data")))
#})
#
