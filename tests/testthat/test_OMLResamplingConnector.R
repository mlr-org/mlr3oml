skip_on_cran()

test_that("Construct resampling from dict", {
  expect_resampling(rsmp("oml", task_id = 9), task = tsk("oml", task_id = 9))
  expect_resampling(rsmp("oml", task_id = 31))
  expect_r6(rsmp("oml", task_id = 31), "ResamplingCustom")
})
