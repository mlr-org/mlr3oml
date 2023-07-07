skip_on_cran()

test_that("Collection 232", {
  local_public_server()
  collection = OMLCollection$new(232L)
  expect_equal(collection$id, 232L)
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

test_that("Can open help page for OpenML Collection", {
  expect_error(OMLCollection$new(232)$help(), regexp = NA)
})

test_that("printer works", {
  local_log_info()

  # task collection
  oml_collection = ocl(99)
  observed = capture.output(oml_collection)[2:4]
  expected = c(
    "<OMLCollection: 99> OpenML-CC18 Curated Class[...]",
    " * data:  72",
    " * tasks: 72"
  )
  expect_equal(observed, expected)

  # same for run collection
  oml_collection1 = ocl(275)
  observed1 = capture.output(oml_collection1)[2:6]
  expected1 = c(
    "<OMLCollection: 275> CC18-Example",
    " * data:  39",
    " * tasks: 39",
    " * flows: 1",
    " * runs:  39"
  )
  expect_equal(observed1, expected1)
})

test_that("download runs without error", {
  local_log_info()

  # simple sanity check
  out = capture.output(with_cache(ocl(72)$download(), cache = FALSE))
  # just the desc
  expect_true(length(out) == 1L)
})


