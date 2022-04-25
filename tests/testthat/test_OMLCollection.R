skip_on_cran()

test_that("Collection 232", {
  with_public_server()
  collection = OMLCollection$new(232L, FALSE)
  expect_oml_collection(collection)
  expect_equal(collection$name, "Test-Study")
  expect_equal(collection$main_entity_type, "run")
  expect_equal(collection$flow_ids, c(17374L, 17369L))
  expect_equal(collection$data_ids, c(3L, 6L))
  expect_equal(collection$run_ids, c(10417460L, 10417461L, 10417462L, 10417463L))
  expect_equal(collection$task_ids, c(3L, 6L))
})


test_that("Can convert run collection to benchmark result", {
  col = OMLCollection$new(232, FALSE)
  tasks = mlr3::as_tasks(col)
  resamplings = mlr3::as_resamplings(col)
  expect_true(all(map_lgl(tasks, function(x) inherits(x, "Task"))))
  expect_true(all(map_lgl(resamplings, function(x) inherits(x, "Resampling"))))
  bmr = suppressWarnings(mlr3::as_benchmark_result(col))
  expect_r6(bmr, "BenchmarkResult")
  expect_error(bmr$score(msr("classif.ce")), regexp = NA)
})

if (FALSE) {
  test_that("Can benchmark <-> collection", {
    with_test_server()
    tasks = lapply(c("penguins", "sonar"), tsk)
    tasks = map(c(1197L, 403L), function(x) tsk("oml", task_id = x))
    resamplings = map(c(1197L, 403L), function(x) rsmp("oml", task_id = x))
    learners = lrns(c("classif.featureless", "classif.rpart"))

    design = benchmark_grid(tasks, learners, resamplings)
    print(design)
    set.seed(123)
    bmr = benchmark(design)
    publish(bmr)

    publish(bmr$learners$learner[[1]])
  })
}
