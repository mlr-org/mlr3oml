skip_on_cran()

test_that("S3 converter work for run", {
  id = 10587724
  run = OMLRun$new(id)
  expect_r6(mlr3::as_task(run), "Task")
  expect_r6(mlr3::as_resampling(run), "Resampling")
  # expect_r6(mlr3::as_data_backend(run), "DataBackend")
  expect_r6(mlr3::as_resample_result(run), "ResampleResult")
})

test_that("S3 converter work for flow", {
  id = 19082
  flow = OMLFlow$new(id)
  expect_r6(mlr3::as_learner(flow, task_type = "classif"), "LearnerClassif")
})

test_that("S3 converter work for task", {
  id = 31
  task = OMLTask$new(id)
  expect_r6(mlr3::as_task(task), "Task")
  expect_r6(mlr3::as_resampling(task), "Resampling")
  # expect_r6(mlr3::as_data_backend(task), "DataBackend")
})

test_that("S3 converter work for data", {
  id = 31
  data = OMLData$new(id)
  expect_r6(mlr3::as_task(data), "Task")
  # expect_r6(mlr3::as_data_backend(data), "DataBackend")
})

test_that("S3 converter work for collection", {
  id = 232
  collection = OMLCollection$new(id)
  collection$runs
  expect_r6(mlr3::as_benchmark_result(collection), "BenchmarkResult")
  expect_true(all(map_lgl(mlr3::as_tasks(collection), function(x) test_r6(x, "Task"))))
  expect_true(all(map_lgl(mlr3::as_resamplings(collection), function(x) test_r6(x, "Resampling"))))
})
