test_that("get_cache_dir returns correct cache directory.", {
  expect_equal(get_cache_dir("abc", TRUE), "abc/test")
  expect_equal(get_cache_dir("abc", FALSE), "abc/public")

  expect_equal(get_cache_dir(TRUE, TRUE), file.path(R_user_dir("mlr3oml", "cache"), "test"))
  expect_equal(get_cache_dir(TRUE, FALSE), file.path(R_user_dir("mlr3oml", "cache"), "public"))

  expect_equal(get_cache_dir(FALSE, TRUE), FALSE)
  expect_equal(get_cache_dir(FALSE, FALSE), FALSE)
})

test_that("get_cache_dir fails when cache-root cannot be created", {
  # fails when it is a subdirectory of a non-existing directory
  expect_error(get_cache_dir(file.path(tempfile(), "xyz"), TRUE))
  expect_error(get_cache_dir(file.path(tempfile(), "xyz"), FALSE))

  # when directory can be created it works
  expect_error(get_cache_dir(tempfile(), TRUE), regexp = NA)
  expect_error(get_cache_dir(tempfile(), FALSE), regexp = NA)
})

test_that("No issues when upgrading from 0.5.0 to 0.6.0 for public server", {
  # Because from version >= 0.6.0 we allow for the test server, we have to write this information
  # to the cache to avoid that caches for test and public server are mixed.
  # We need to take care that the migration between versions works.

  # 1. Migration from 0.5.0 to 0.6.0:
  # For the public verser
  # The version.json exists (but no server.json)

  withr::defer({unlink(cachedir, recursive = TRUE)})
  cachedir = tempfile()
  withr::defer({CACHE$initialized = setdiff(CACHE$initialized, cachedir)})

  dir.create(file.path(cachedir, "data"), recursive = TRUE)
  writeLines(jsonlite::toJSON(list(data = 1L)), con = file.path(cachedir, "version.json"))
  initialize_cache(cachedir, FALSE)

  # this means that the cache was flushed
  expect_true("data" %nin% list.files(cachedir))
  # this means that we wrote the
  server_info = jsonlite::fromJSON(readLines(file.path(cachedir, "server.json")))
  expect_true(server_info == "public")
  version_info = jsonlite::fromJSON(readLines(file.path(cachedir, "version.json")))
  expect_true(all(mlr3misc::map_lgl(version_info, function(v) v == 2L)))
})

test_that("Initializing cache fails when missing version.", {
  # Because from version >= 0.6.0 we allow for the test server, we have to write this information
  # to the cache to avoid that caches for test and public server are mixed.
  # We need to take care that the migration between versions works.

  # 1. Migration from 0.5.0 to 0.6.0:
  # For the public verser
  # The version.json exists (but no server.json)

  withr::defer({unlink(cachedir, recursive = TRUE)})
  cachedir = tempfile()
  dir.create(cachedir)
  withr::defer({CACHE$initialized = setdiff(CACHE$initialized, cachedir)})

  expect_error(initialize_cache(cachedir, FALSE), "was not initialized by mlr3oml")
  expect_error(initialize_cache(cachedir, TRUE), "was not initialized by mlr3oml")
})

test_that("Initializing cache fails when there is a server mismatch", {
  # Because from version >= 0.6.0 we allow for the test server, we have to write this information
  # to the cache to avoid that caches for test and public server are mixed.
  # We need to take care that the migration between versions works.

  # 1. Migration from 0.5.0 to 0.6.0:
  # For the public verser
  # The version.json exists (but no server.json)

  withr::defer({unlink(cachedir, recursive = TRUE)})
  cachedir = tempfile()
  dir.create(cachedir)
  writeLines(jsonlite::toJSON("public"), file.path(cachedir, "server.json"))
  writeLines(jsonlite::toJSON(list(data = 1)), file.path(cachedir, "version.json"))

  expect_error(initialize_cache(cachedir, TRUE), "mixing cache directories")
})

test_that("It is possible to first initialize test and then public cache", {
  with_test_server()
  withr::defer({unlink(cachedir, recursive = TRUE)})
  cachedir = tempfile()
  OMLData$new(31, test_server = TRUE, cache = cachedir)$desc
  with_public_server()
  expect_error(OMLData$new(31, test_server = FALSE, cache = cachedir)$desc, regexp = NA)
})
