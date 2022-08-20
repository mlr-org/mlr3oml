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
  with_public_server()
  id = 538858L
  run = OMLRun$new(id)
  expect_oml_run(run)
})

test_that("Runs 10417460 10417461 10417462 10417463", {
  with_public_server()
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
  with_public_server()
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
  with_public_server()
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

test_that("id = 2081174L", {
  with_public_server()
  id = 2081174L
  run = OMLRun$new(id)
  flow = run$flow
  expect_error(mlr3::as_resample_result(run), regexp = NA)
})

test_that("Can extract prediction for sklearn", {
  with_public_server()
  id = 10587656L
  run = OMLRun$new(id, FALSE)
  expect_error(rr <- mlr3::as_resample_result(run), regexp = NA)
  expect_error(rr$score(msr("classif.ce")), regexp = NA)
})

test_that("Can extract prediction for mlr", {
  with_public_server()
  id = 10587674L
  run = OMLRun$new(id, FALSE)
  expect_error(mlr3::as_resample_result(run), regexp = NA)
})

test_that("OMLFlow conversion throws the correct warnings", {
  run = OMLRun$new(10587742L)
  learner = mlr3::as_learner(run$flow, task_type = "classif")
  valid_params = run$parameter_setting
  invalid_params = data.table(
    name = "xyz",
    value = list(1),
    component = 1
  )
  run$.__enclos_env__$private$.desc$parameter_setting = invalid_params
  expect_warning(
    mlr3::as_resample_result(run),
    regexp = "Problem assigning"
  )
})

