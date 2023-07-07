skip_on_cran()

# What we want to test:
# For task types: classif, regr, surv
# expect_oml_run works
#
# When the test server works:
# 1. Create run for clasisf, regr and surv (for different predict_types)
# 2. Publish the run.
# 3. Download the run and convert it.
#

test_that("Run 538858", {
  local_public_server()
  id = 538858L
  run = OMLRun$new(id)
  expect_oml_run(run)
})

test_that("Runs 10417460 10417461 10417462 10417463", {
  local_public_server()
  ids = OMLCollection$new(232L)$run_ids
  runs = map(
    ids,
    function(id) {
      run = OMLRun$new(id)
      run$desc
      return(run)
    }
  )
  for (run in runs) {
    expect_oml_run(run)
  }
})

test_that("classification, mlr, 538858", {
  local_public_server()
  run_id = 538858L

  run = OMLRun$new(run_id, FALSE)
  expect_equal(run$task_type, "Supervised Classification")
  expect_equal(run$task_id, 3815L)
  expect_equal(run$tags, c("mlr", "randomBot"))
  expect_equal(run$flow_id, 3364)
  expect_equal(run$id, 538858L)
  expect_equal(names(run$parameter_setting), c("name", "value", "component"))
  expect_data_table(run$parameter_setting)
})

test_that("classification, mlr, 8000000", {
  local_public_server()
  run_id = 8000000
  run = OMLRun$new(run_id)
  expect_equal(run$task_type, "Supervised Classification")
  expect_equal(run$task_id, 3903L)
  expect_equal(run$tags, c("botV1", "mlrRandomBot", "sciBenchV1.0"))
  expect_equal(run$flow_id, 5965L)
  expect_equal(run$id, 8000000L)
  expect_equal(names(run$parameter_setting), c("name", "value", "component"))
  expect_data_table(run$parameter_setting)
})

test_that("id = 10587951", {
  local_public_server()
  id = 10587951
  run = OMLRun$new(id)
  flow = run$flow
  expect_error(mlr3::as_resample_result(run), regexp = NA)
})

test_that("Can extract prediction for sklearn", {
  local_public_server()
  id = 10587656L
  run = OMLRun$new(id, FALSE)
  expect_error(rr <- mlr3::as_resample_result(run), regexp = NA)
  expect_error(rr$score(msr("classif.ce")), regexp = NA)
})

test_that("Can extract prediction for mlr", {
  local_public_server()
  id = 10587674L
  run = OMLRun$new(id, FALSE)
  expect_error(mlr3::as_resample_result(run), regexp = NA)
})


test_that("Can open help page for OpenML Run", {
  expect_error(OMLRun$new(51)$help(), regexp = NA)
})

test_that("printer works", {
  local_log_info()

  with_cache({
    oml_run = orn(10593878)
    observed = capture.output(print(oml_run))[4:6]
    expected = c(
      "<OMLRun:10593878>",
      " * Task: kr-vs-kp (id: 3)",
      " * Flow: sklearn.pipeline.Pipeline[...] (id: 19521)"
    )
    expect_equal(observed, expected)
   }, cache = FALSE)
})


test_that("download runs without error", {
  local_log_info()
  # simple sanity check
  out = capture.output(with_cache(orn(10593878)$download(), cache = FALSE))
  # flow (1) + task (6) + prediction + desc
  expect_true(length(out) == 9L)
})
