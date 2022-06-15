test_that("get_id works for Task", {
  otask = OMLTask$new(31)
  task = as_task(otask)
  expect_integer(get_id(task))
  task$set_col_roles("job", roles = "target")
  task$set_col_roles("class", roles = "feature")
  expect_message(id <<- get_id(task), "originally from OpenML")
  expect_true(is.null(id))
})


test_that("get_id works for Resampling", {
  otask = OMLTask$new(31)
  task_mtcars = tsk("mtcars")
  resampling = as_resampling(otask)
  expect_integer(get_id(resampling))
  resampling$instantiate(task_mtcars)
  expect_message(get_id(resampling), "originally from OpenML")
  expect_true(suppressMessages(is.null(get_id(resampling))))
})
