test_that("benchmark_design works on non-oml inputs", {
  tasks = tsks(c("pima", "iris"))
  learners = lrns(c("classif.featureless", "classif.rpart"))
  resampling = rsmp("cv")
  resamplings = pmap(
    list(tasks, rsmps(c("cv", "holdout"))),
    function(task, resampling) resampling$instantiate(task)
  )
  design = benchmark_design(tasks, learners, resamplings)
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

test_that("benchmark_design works on oml inputs", {
  with_public_server()
  learners = lrns(c("classif.featureless", "classif.rpart"))
  suite = OMLCollection$new(268L)
  tasks = suite$tasks$convert()
  resamplings = suite$resamplings$convert()
  design = benchmark_design(tasks, learners, resamplings)
  expect_true(nrow(design) == 16L)
})

test_that("Resamplings must be instantiated", {
  tasks = tsks(c("pima", "iris"))
  learners = lrns(c("classif.featureless", "classif.rpart"))
  resamplings = rsmps(c("cv", "holdout"))
  expect_error(benchmark_design(tasks, learners, resamplings))
})

test_that("Resamplings and tasks must have the same length", {
  tasks = tsks(c("pima", "iris"))
  learners = lrns(c("classif.featureless", "classif.rpart"))
  resamplings = pmap(
    list(tasks, rsmps(c("cv", "holdout"))),
    function(task, resampling) resampling$instantiate(task)
  )
  resamplings = c(resamplings, resamplings)
  expect_error(benchmark_design(tasks, learners, resamplings))
})

test_that("Resamplings and tasks must have corresponding hashes", {
  tasks = tsks(c("pima", "iris"))
  learners = lrns(c("classif.featureless", "classif.rpart"))
  resamplings = pmap(
    list(tasks, rsmps(c("cv", "holdout"))),
    function(task, resampling) resampling$instantiate(task)
  )
  resamplings = rev(resamplings)
  expect_error(benchmark_design(tasks, learners, resamplings))
})
