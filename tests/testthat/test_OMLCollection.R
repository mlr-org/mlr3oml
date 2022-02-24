skip_on_cran()

test_that("Collection 232", {
  with_public_server()
  coll = OMLCollection$new(232L)
  expect_equal(coll$name, "Test-Study")
  expect_equal(coll$main_entity_type, "run")
  expect_equal(coll$flow_ids, c(17374L, 17369L))
  expect_equal(coll$data_ids, c(3L, 6L))
  expect_equal(coll$run_ids, c(10417460L, 10417461L, 10417462L, 10417463L))
  expect_equal(coll$task_ids, c(3L, 6L))
  expect_r6(coll$runs, "OMLDictionaryRun")
  expect_r6(coll$flows, "OMLDictionaryFlow")
  expect_r6(coll$data, "OMLDictionaryData")
  expect_r6(coll$tasks, "OMLDictionaryTask")
})

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
  debugonce(publish.BenchmarkResult)
  publish(bmr)

  debugonce(publish)
  publish(bmr$learners$learner[[1]])
})

test_that("Can convert run collection to benchmark result", {
  col = OMLCollection$new(232)
  bmr = suppressWarnings(col$convert())
  expect_r6(bmr, "BenchmarkResult")
  expect_error(bmr$score(msr("classif.ce")), regexp = NA)
})

test_that("Can convert main_entity_type task to list of tasks and resamplings", {
  col = OMLCollection$new(258)
  output = col$convert()
  expect_equal(names(output), c("task", "resampling"))
  expect_true(all(map_lgl(output[["tasks"]], function(x) inherits(x, "Task"))))
  expect_true(all(map_lgl(output[["tasks"]], function(x) inherits(x, "Resampling"))))
})
