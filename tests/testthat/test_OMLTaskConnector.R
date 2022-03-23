skip_on_cran()

test_that("Construct task from dict", {
  expect_task(tsk("oml", task_id = 9))
})
