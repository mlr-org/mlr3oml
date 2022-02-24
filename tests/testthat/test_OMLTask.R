skip_on_cran()

test_that("OMLTask iris", {
  oml_task = OMLTask$new(59)
  expect_oml_task(oml_task)

  expect_string(oml_task$name, pattern = "iris")
  expect_identical(oml_task$nrow, 150L)
  expect_identical(oml_task$ncol, 5L)
  expect_identical(oml_task$target_names, "class")
  expect_r6(oml_task$task, "TaskClassif")
})

test_that("data backend", {
  oml_task = OMLTask$new(59)
  expect_backend(as_data_backend(oml_task))
})

test_that("TaskSurv", {
  skip_if_not_installed("mlr3proba")
  oml_task = OMLTask$new(7304)
  expect_class(oml_task$task, "TaskSurv")
})

test_that("Task 1 works", {
  oml_task = OMLTask$new(1)
  expect_oml_task(oml_task)
})

test_that("Task 100 works", {
  oml_task = OMLTask$new(1)
  expect_oml_task(oml_task)
})

test_that("Randomized download test", {
  # this almost never runs
  n = 10
  task_types = c(
    "Supervised Regression",
    "Supervised Classification",
    "Survival Analysis"
  )
  task_ids = sample(load_ids("task"), size = n)
  for (task_id in task_ids) {
    task = OMLTask$new(task_id)
    if (task$task_type %in% task_types) {
      expect_oml_task(OMLTask$new(task_id))
    }
  }
})
