skip_on_cran()

test_that("Download run 538858", {
  with_public_server()
  id = 538858L
  run = OMLRun$new(id)
  expect_oml_run(run)
})

test_that("Randomized download test", {
  with_public_server()
  set.seed(1)
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



test_that("Warning when parameters of run and flow don't match", {
  with_public_server()
  id = 2081174L
  run = OMLRun$new(id)
  expect_error(rr <- as_resample_result(run), regexp = NA)
  expect_true(all(is.na(rr$learners[[1]]$param_set$values)))
  expect_error(rr$score(msr("classif.ce")), regexp = NA)
})

test_that("Can extract prediction for sklearn", {
  with_public_server()
  id = 10587656L
  run = OMLRun$new(id, FALSE)
  expect_warning(rr <- as_resample_result(run), "Problematic parameter setting, setting all parameters to NA.")
  expect_error(rr$score(msr("classif.ce")), regexp = NA)
})

test_that("Can extract prediction for mlr", {
  with_public_server()
  id = 10587674L
  run = OMLRun$new(id, FALSE)
  expect_error(as_resample_result(run), regexp = NA)
})


if (FALSE) { # Upload tests
  # TODO: check that it works also with weird parameters (lists and functions)
  # TODO: Check that this works for regression, classification and survival for the various
  # predict types (especially probs for classification)
  test_that("Upload + download for mlr3 classif (prob) works", {
    with_test_server()
    learner = lrn("classif.rpart", predict_type = "prob")
    otask = OMLTask$new(31)
    task = as_task(otask)
    resampling = as_resampling(otask)
    rr = resample(task, learner, resampling)
    run_id = publish(rr, confirm = FALSE)$run_id
    run = OMLRun$new(run_id)
    rr_rec = as_resample_result(run)
  })

  test_that("Can publish run of flow mlr3.rpart on task 1308 (Iris)", {
    with_test_server()
    withr::defer({
      delete("run", ids[["run_id"]])
      delete("flow", ids[["flow_id"]])
    })
    learner = lrn("classif.rpart", cp = 0.5)
    oml_task = OMLTask$new(1308L)
    task = as_task(oml_task)
    resampling = as_resampling(oml_task)
    rr = resample(task, learner, resampling)
    ids = publish(rr, confirm = FALSE)
    # OMLRun$debug("convert")
    run = OMLRun$new(ids[["run_id"]])
    rr_rec = as_resample_result(run)
    expect_equal(rr_rec$task, rr$task)
    expect_equal(rr_rec$task_type, rr$task_type)
    expect_equal(rr_rec$learners, rr$learners)
    expect_equal(rr_rec$iters, rr$iters)
    expect_equal(rr_rec$prediction(), rr$prediction())

    ce = rr$score(msr("classif.ce"))$classif.ce
    ce_rec = rr_rec$score(msr("classif.ce"))$classif.ce
    expect_true(all(ce == ce_rec))
  })
}


test_that("OMLFlow conversion throws the correct warnings", {
  run = OMLRun$new(10587742L)
  learner = as_learner(run$flow, "classif")
  expect_message(as_resample_result(run), regexp =
    "Constructed non-executable pseudo-learner."
  )
  valid_params = run$parameter_setting
  invalid_params = data.table(
    name = "xyz",
    value = list(1),
    component = 1
  )
  run$.__enclos_env__$private$.desc$parameter_setting = invalid_params
  expect_warning(
    as_resample_result(run),
    regexp = "Problem assigning parameter_setting to learner, setting all params to NA."
  )

})
