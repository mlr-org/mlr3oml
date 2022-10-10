test_that("benchmark_grid_oml works on non-oml inputs", {
  tasks = mlr3::tsks(c("pima", "iris"))
  learners = suppressWarnings(mlr3::lrns(c("classif.featureless", "classif.rpart")))
  resampling = mlr3::rsmp("cv")
  resamplings = pmap(
    list(tasks, mlr3::rsmps(c("cv", "holdout"))),
    function(task, resampling) resampling$instantiate(task)
  )
  design = benchmark_grid_oml(tasks, learners, resamplings)
  # design[, identical(task), by = task]]
  # expect(identical(design$resampling[class(learner)[[1]] ==)]))
  expect_true(nrow(design) == 4L) #
  expect_true(identical(design$task[[1]], design$task[[2]]))
  expect_true(identical(design$task[[3]], design$task[[4]]))
  expect_false(identical(design$task[[1]], design$task[[3]]))

  expect_true(identical(design$resampling[[1]], design$resampling[[2]]))
  expect_true(identical(design$resampling[[3]], design$resampling[[4]]))
  expect_false(identical(design$resampling[[1]], design$resampling[[3]]))

  expect_true(identical(design$learner[[1]], design$learner[[3]]))
  expect_true(identical(design$learner[[2]], design$learner[[4]]))
  expect_false(identical(design$learner[[2]], design$learner[[3]]))
})

test_that("Resamplings must be instantiated", {
  tasks = mlr3::tsks(c("pima", "iris"))
  learners = suppressWarnings(mlr3::lrns(c("classif.featureless", "classif.rpart")))
  resamplings = mlr3::rsmps(c("cv", "holdout"))
  expect_error(benchmark_grid_oml(tasks, learners, resamplings))
})

test_that("Resamplings and tasks must have the same length", {
  tasks = mlr3::tsks(c("pima", "iris"))
  learners = suppressWarnings(mlr3::lrns(c("classif.featureless", "classif.rpart")))
  resamplings = pmap(
    list(tasks, mlr3::rsmps(c("cv", "holdout"))),
    function(task, resampling) resampling$instantiate(task)
  )
  resamplings = c(resamplings, resamplings)
  expect_error(benchmark_grid_oml(tasks, learners, resamplings))
})

test_that("Resamplings and tasks must have corresponding hashes", {
  tasks = mlr3::tsks(c("pima", "iris"))
  learners = suppressWarnings(mlr3::lrns(c("classif.featureless", "classif.rpart")))
  resamplings = pmap(
    list(tasks, mlr3::rsmps(c("cv", "holdout"))),
    function(task, resampling) resampling$instantiate(task)
  )
  resamplings = rev(resamplings)
  expect_error(benchmark_grid_oml(tasks, learners, resamplings))
})

skip_on_cran()

test_that("benchmark_grid_oml works on oml inputs", {
  with_public_server()
  learners = suppressWarnings(mlr3::lrns(c("classif.featureless", "classif.rpart")))
  suite = OMLCollection$new(268L)
  tasks = mlr3::as_tasks(suite)
  resamplings = mlr3::as_resamplings(suite)
  design = benchmark_grid_oml(tasks, learners, resamplings)
  expect_true(nrow(design) == 16L)
})

