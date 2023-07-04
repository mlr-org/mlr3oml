test_that("with_cache works", {
  prev_cache_opt = getOption("mlr3oml.cache")
  cache_dir = tempfile()
  oml_data = with_cache(odt(31), cache_dir)
  expect_equal(oml_data$cache_dir, cache_dir)
  expect_equal(prev_cache_opt, getOption("mlr3oml.cache"))
})
