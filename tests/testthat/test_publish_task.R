skip("OpenML Test server is unstable")

test_that("Can publish task on public server", {
  test_server = FALSE
  withr::defer(delete(type = "task", id = task_id, test_server = test_server))

  data_id = 45650

  Sys.sleep(5)

  task_id = publish_task(id = data_id, type = "regr", target = "Target", estimation_procedure = 7)
  expect_message({
    task_id2 <<- publish_task(id = data_id, type = "regr", target = "Target", estimation_procedure = 7)},
    "already exists"
  )
  expect_equal(task_id, task_id2)

  otask = otsk(task_id)
  expect_oml_task(otask)

  expect_equal(otask$estimation_procedure$id, 7L)
  expect_equal(otask$data_id, data_id)
  expect_equal(otask$target_names, "Target")
  expect_equal(otask$task_type, "Supervised Regression")
})
