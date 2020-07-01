context("OMLResamplingConnector")

skip_on_cran()

test_that("Construct resampling from dict", {
  expect_resampling(mlr3::rsmp("oml", task_id = 9), task = mlr3::tsk("oml", task_id = 9))
})
