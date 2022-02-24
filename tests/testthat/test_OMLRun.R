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
  n = 10
  ids = sample(load_ids("run"), size = n)
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

  run = OMLRun$new(run_id)
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
  expect_equal(run$task_evaluation_measure, NULL)
  expect_equal(run$tags, c("botV1", "mlrRandomBot", "sciBenchV1.0"))
  expect_equal(run$flow_id, 5965L)
  expect_equal(run$id, 8000000L)
  expect_equal(names(run$parameter_setting), c("name", "value", "component"))
  expect_data_table(run$parameter_setting)
})


test_that("Can publish run of flow mlr3.rpart on task 1308 (Iris)", {
  with_test_server()
  withr::defer({
    delete("run", ids[["run_id"]])
    delete("flow", ids[["flow_id"]])
  })
  learner = lrn("classif.rpart", cp = 0.5)
  oml_task = OMLTask$new(1308L)
  task = oml_task$convert()
  resampling = oml_task$resampling$convert()
  rr = resample(task, learner, resampling)
  ids = publish(rr, confirm = FALSE)
  # OMLRun$debug("convert")
  run = OMLRun$new(ids[["run_id"]])
  rr_rec = run$convert()
  expect_equal(rr_rec$task, rr$task)
  expect_equal(rr_rec$task_type, rr$task_type)
  expect_equal(rr_rec$learners, rr$learners)
  expect_equal(rr_rec$iters, rr$iters)
  expect_equal(rr_rec$prediction(), rr$prediction())

  ce = rr$score(msr("classif.ce"))$classif.ce
  ce_rec = rr_rec$score(msr("classif.ce"))$classif.ce
  expect_true(all(ce == ce_rec))
})

test_that("Warning when parameters of run and flow don't match", {
  with_public_server()
  id = 2081174L
  run = OMLRun$new(id)
  expect_warning(rr <- run$convert(), "Problematic parameter setting, setting all parameters to NA.")
  expect_true(all(is.na(rr$learners[[1]]$param_set$values)))
  expect_error(rr$score(msr("classif.ce")), regexp = NA)
})

test_that("Can extract prediction for sklearn", {
  with_public_server()
  id = 10587656L
  run = OMLRun$new(id)
  expect_warning(rr <- run$convert(), "Problematic parameter setting, setting all parameters to NA.")
  expect_error(rr$score(msr("classif.ce")), regexp = NA)
})

test_that("Can extract prediction for mlr", {
  with_public_server()
  id = 10587674L
  run = OMLRun$new(id)
  run$convert()
  expect_error(run$convert(), regexp = NA)
})
