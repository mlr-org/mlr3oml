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
