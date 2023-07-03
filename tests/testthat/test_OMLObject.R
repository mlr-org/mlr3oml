skip_on_cran()

test_that("Caching is disabled when cache directory does not exist (anymore)", {
  old_threshold = lg$threshold
  lg$set_threshold("info")
  on.exit({lg$set_threshold(old_threshold)}, add = TRUE)
  cache_dir = tempfile()
  odata = odt(31, cache = cache_dir)
  capture.output(odata$desc)
  unlink(cache_dir, recursive = TRUE)
  out = capture.output(odata$qualities)
  expect_true(grepl(out[[1L]], pattern = "disabling caching for this object"))
  expect_false(odata$cache_dir)
})
