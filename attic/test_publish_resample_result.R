test_that("Can publish resample result", {
  with_public_server()
  withr::defer({delete("run", run_id, confirm = FALSE)})
  withr::defer({delete("flow", flow_id, confirm = FALSE)})
  otask = OMLTask$new(1)
  task = as_task(otask)
  resampling = as_resampling(otask)
  learner = lrn("classif.rpart")
  rr = resample(task, learner, resampling)
  run_id = publish(rr, confirm = FALSE)

  orun = OMLRun$new(run_id)
  flow_id = orun$flow$id
  expect_oml_run(orun)

  task = tsk("iris")
  resampling = rsmp("holdout")
  learner = lrn("classif.rpart")
  rr = resample(task, learner, resampling)
  expect_error(
    publish(rr, confirm = FALSE),
    regexp = "You can only upload benchmark results using (unmodified) OpenML tasks.",
    fixed = TRUE
  )

  task = as_task(otask)
  rr = resample(task, learner, resampling)
  expect_error(
    publish(rr, confirm = FALSE),
    regexp = "You can only upload benchmark results using (unmodified) OpenML data splits.",
    fixed = TRUE
  )
})

test_that("Modifying task / resampling does not allow to upload", {
  with_public_server()
  otask = OMLTask$new(31)
  task = as_task(otask)
  resampling = as_resampling(otask)
  task$col_roles$feature = "age"
  learner = lrn("classif.rpart")
  rr = resample(task, learner, resampling)
  expect_error(
    publish(rr, confirm = FALSE),
    regexp = "You can only upload benchmark results using (unmodified) OpenML tasks.",
    fixed = TRUE
  )
  task = as_task(otask)
  resampling$param_set$values$folds = 3L
  rr = resample(task, learner, resampling)
  expect_error(
    publish(rr, confirm = FALSE),
    regexp = "You can only upload benchmark results using (unmodified) OpenML data splits.",
    fixed = TRUE
  )
})

test_that("Cannot upload resample results with errors", {
  with_public_server()
  learner = lrn("classif.debug", error_train = 1)
  learner$fallback = lrn("classif.rpart")
  task = as_task(otask)
  resampling = as_resampling(otask)
  rr = resample(task, learner, resampling)
  expect_error(
    publish(rr, confirm = FALSE),
    regexp = "Cannot publish resample result containing errors.",
    fixed = TRUE
  )
})

test_that("Can publish resample result where learner has weird parameters.", {
  with_public_server()
  withr::defer(delete("run", run_id, confirm = FALSE))
  withr::defer(delete("flow", flow_id, confirm = FALSE))
  learner = lrn("classif.rpart")
  class(learner) = c(class(learner), paste0(sample(letters, 20), collapse = ""))
  learner$.__enclos_env__$private$.param_set$add(paradox::ParamUty$new(id = "unused1"))
  learner$.__enclos_env__$private$.param_set$add(paradox::ParamUty$new(id = "unused2"))
  learner$param_set$values$unused1 = list(na.omit, "a", b = 1)
  learner$param_set$values$unused2 = function(x) print("hallo")
  otask = OMLTask$new(7306)
  rr = resample(otask, learner, otask)

  run_id = publish(rr, confirm = FALSE)
  orun = OMLRun$new(run_id, FALSE)
  flow_id = orun$flow_id

  values = orun$parameter_setting$value

  # normal parameter is correctly parsed
  expect_true(values[[1L]] == 0)

  expected = list(
    "1" = c("function (object, ...) ", "UseMethod(\"na.omit\")"),
    "2" = "a",
    "b" = 1
  )
  expect_true(all.equal(values[[2L]], expected))
  expect_true(all.equal(values[[3L]], c("function (x) ", "print(\"hallo\")")))
})

test_that("Cannot publish run with non-atomic parameter values", {
  rr = list(learner = list(param_set = list(values = list(fun = na.action))))
  class(rr) = "ResampleResult"
  expect_error(publish(rr, confirm = FALSE),
    regex = "Can currently only publish flows with atomic parameter values."
  )
})

test_that("Test publish, download convert, publish lifecycle", {
  with_test_server()
  l = lrn("classif.rpart")
  publish(l)
  # TODO:
  # 1. publish flow
  # 2. Download flow
  # 3. convert flow using as_learner and compare
  # 4. Upload again and check that same id is returned
  # 5. delete flow
})

test_that("Upload + download for mlr3 classif (prob) works", {
  with_test_server()
  learner = lrn("classif.rpart", predict_type = "prob")
  otask = OMLTask$new(31)
  task = mlr3::as_task(otask)
  resampling = mlr3::as_resampling(otask)
  rr = resample(task, learner, resampling)
  run_id = publish(rr, confirm = FALSE)$run_id
  run = OMLRun$new(run_id)
  rr_rec = mlr3::as_resample_result(run)
})

test_that("Can publish run of flow mlr3.rpart on task 1308 (Iris)", {
  with_test_server()
  withr::defer({
    delete("run", ids[["run_id"]])
    delete("flow", ids[["flow_id"]])
  })
  learner = lrn("classif.rpart", cp = 0.5)
  oml_task = OMLTask$new(31L)
  task = mlr3::as_task(oml_task)
  resampling = mlr3::as_resampling(oml_task)
  rr = resample(task, learner, resampling)
  ids = publish(rr, confirm = FALSE)
  # OMLRun$debug("convert")
  run = OMLRun$new(ids[["run_id"]])
  rr_rec = mlr3::as_resample_result(run)
  expect_equal(rr_rec$task, rr$task)
  expect_equal(rr_rec$task_type, rr$task_type)
  expect_equal(rr_rec$learners, rr$learners)
  expect_equal(rr_rec$iters, rr$iters)
  expect_equal(rr_rec$prediction(), rr$prediction())

  ce = rr$score(msr("classif.ce"))$classif.ce
  ce_rec = rr_rec$score(msr("classif.ce"))$classif.ce
  expect_true(all(ce == ce_rec))
})
