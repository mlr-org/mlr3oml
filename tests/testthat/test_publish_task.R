skip()

test_that("Can publish task on test server", {
  test_server = TRUE
  withr::defer(delete(type = "task", id = task_id, test_server = test_server))
  withr::defer(delete(type = "task", id = task_id2, test_server = test_server))

  data_id = 128 # iris

  f = function() {
    publish_task(id = data_id, type = "classif", target = "class", estimation_procedure = 6, test_server = test_server) # nolint
  }

  expect_message({task_id <<- f()}, regexp = NA) # nolint
  Sys.sleep(10)
  expect_message(task_id2 <<- f(), "already exists")
  expect_equal(task_id, task_id2)

  otask = otsk(task_id, test_server = TRUE)
  expect_oml_task(otask)

  expect_equal(otask$estimation_procedure$id, 6L)
  expect_equal(otask$data_id, data_id)
  expect_equal(otask$target_names, "Species")
  expect_equal(otask$task_type, "Supervised Classification")
})

test_that("survival", {
  test_server = TRUE
  withr::defer(delete(type = "data", id = data_id, test_server = test_server))
  withr::defer(delete(type = "task", id = task_id, test_server = test_server))
  data = data.frame(status = sample(c(1, 2), size = 100, replace = TRUE), time = runif(100), x = rnorm(100))
  data_id = publish_data(data, name = "test_surv", desc = "test rats", test_server = test_server)
  expect_integer(data_id)
  Sys.sleep(10)
  task_id = publish_task(id = data_id, type = "surv", estimation_procedure = 19,
    target = c(event = "status", right = "time"), test_server = test_server)

  Sys.sleep(5)
  otask = otsk(task_id, test_server = test_server)
  expect_equal(otask$task_type, "Survival Analysis")
})
