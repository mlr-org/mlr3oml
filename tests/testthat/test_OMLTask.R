skip_on_cran()

# What we want to test:
# `expect_oml_task` forks for: classif, regr, surv

test_that("OMLTask iris", {
  oml_task = OMLTask$new(59, FALSE)
  expect_oml_task(oml_task)
  expect_string(oml_task$name, pattern = "iris")
  expect_identical(oml_task$nrow, 150L)
  expect_identical(oml_task$ncol, 5L)
  expect_identical(oml_task$target_names, "class")
})

test_that("data backend", {
  oml_task = OMLTask$new(59)
  expect_backend(mlr3::as_data_backend(oml_task))
})

# test_that("TaskSurv", {
#   skip_if_not_installed("mlr3proba")
#   oml_task = OMLTask$new(7304)
#   expect_class(mlr3::as_task(oml_task), "TaskSurv")
# })

test_that("Task 1 works", {
  oml_task = OMLTask$new(1)
  expect_oml_task(oml_task)
})

test_that("Task 100 works", {
  oml_task = OMLTask$new(1)
  expect_oml_task(oml_task)
})

test_that("Randomized download test", {
  n = 10L
  task_ids = OMLCollection$new(99)$task_ids
  task_ids = sample(task_ids, n)
  task_types = c(
    "Supervised Regression",
    "Supervised Classification"
  )
  for (task_id in task_ids) {
    task = OMLTask$new(task_id)
    print(task_id)
    if (task$task_type %in% task_types) {
      expect_oml_task(OMLTask$new(task_id, cache = FALSE))
    }
  }
})

