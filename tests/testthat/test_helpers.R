skip_on_cran()

test_that("with_cache works", {
  prev_cache_opt = getOption("mlr3oml.cache")
  cache_dir = tempfile()
  oml_data = with_cache(odt(31), cache_dir)
  expect_equal(oml_data$cache_dir, cache_dir)
  expect_equal(prev_cache_opt, getOption("mlr3oml.cache"))
})

test_that("local_log_info works", {
  f = function() {
    local_log_info()
    expect_equal(lg$threshold, 400)
    lg$set_threshold("debug")
  }
  old_threshold = lg$threshold
  f()
  expect_equal(old_threshold, lg$threshold)
})
