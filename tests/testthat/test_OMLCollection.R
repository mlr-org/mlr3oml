skip_on_cran()

test_that("Collection 232", {
  with_public_server()
  collection = OMLCollection$new(232L)
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

test_that("OMLTask components inherit correct cache directory", {
  dir = tempfile()
  orun = OMLCollection$new(232, cache = dir)
  expect_true(orun$tasks$task[[1L]]$cache_dir == dir)
  expect_true(orun$runs$run[[1L]]$cache_dir == dir)
  expect_true(orun$flows$flow[[1L]]$cache_dir == dir)
  expect_true(orun$data$data[[1L]]$cache_dir == dir)
})



test_that("Can open help page for OpenML Collection", {
  expect_error(OMLCollection$new(232)$help(), regexp = NA)
})
