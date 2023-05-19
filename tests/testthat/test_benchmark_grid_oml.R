test_that("benchmark_grid_oml works", {
  # Main tests are now in mlr3
  learner = lrn("regr.rpart")
  task = tsk("mtcars")
  resampling = rsmp("cv")
  resampling$instantiate(task)
  expect_warning({design = benchmark_grid_oml(task, learner, resampling)}) # nolint
  expect_data_table(design)
})
