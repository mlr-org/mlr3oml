skip_on_cran()

test_that("Caching directory is correctly handled when it does not exist", {
  old_threshold = lg$threshold
  lg$set_threshold("info")
  on.exit({lg$set_threshold(old_threshold)}, add = TRUE)
  cache_dir = tempfile()
  odata = with_cache(odt(31), cache = cache_dir)
  capture.output(odata$desc)
  unlink(cache_dir, recursive = TRUE)
  dir1 = tempfile()
  out = capture.output(with_cache(odata$qualities, cache = dir1))
  expect_true(grepl(out[[1L]], pattern = "changed since initializing this object"))
  expect_true(grepl(out[[1L]], pattern = dir1))
  expect_true(grepl(out[[1L]], pattern = cache_dir))
  expect_equal(odata$cache_dir, dir1)
})
