skip_on_cran()

test_that("Can publish task on test server", {
  test_server = TRUE
  withr::defer(delete(type = "task", id = task_id, test_server = test_server))
  withr::defer(delete(type = "task", id = task_id2, test_server = test_server))

  data_id = 150 # iris

  f = function() {
    publish_task(id = data_id, type = "classif", target = "Species", estimation_procedure = 6, test_server = test_server) # nolint
  }

  expect_message({task_id <<- f()}, regexp = NA) # nolint
  Sys.sleep(5)
  expect_message(task_id2 <<- f(), "already exists")
  expect_equal(task_id, task_id2)

  otask = otsk(task_id, test_server = TRUE)
  expect_oml_task(otask)

  expect_equal(otask$estimation_procedure$id, 6L)
  expect_equal(otask$data_id, data_id)
  expect_equal(otask$target_names, "Species")
  expect_equal(otask$task_type, "Supervised Classification")
})
