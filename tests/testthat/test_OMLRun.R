# skip_on_cran()
run_ids = c(10560781, 10560247, 99999999)

test_that("Download run 538858", {
  with_public_server()
  id = 538858L
  set.seed(1)
  ids = load_ids("run")
  id = sample(ids, 1)
  run = OMLRun$new(id)
  expect_oml_run(run)
})

test_that("Randomized download test", {
  with_public_server()
  n = 1
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
  expect_equal(run$setup_string, list())
  expect_equal(run$task_type, "Supervised Classification")
  expect_equal(run$task_id, 3815L)
  expect_equal(run$task_evaluation_measure, "predictive_accuracy")
  expect_equal(run$uploader_name, "Random Bot")
  expect_equal(
    run$input_data,
    list("dataset" = list(
      did = 952L,
      name = "prnn_fglass",
      url = "https://www.openml.org/data/download/53486/prnn_fglass.arff"
    ))
  )
  expect_equal(run$tag, c("mlr", "randomBot"))
  expect_list(run$output_data)
  expect_equal(names(run$output_data), c("file", "evaluation"))
  expect_equal(run$uploader, 1160L)
  expect_equal(run$flow_id, 3364)
  expect_equal(run$flow_name, "classif.boosting(8)")
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
  expect_equal(run$tag, c("botV1", "mlrRandomBot", "sciBenchV1.0"))
  expect_equal(run$flow_id, 5965L)
  expect_equal(run$flow$name, "mlr.classif.ranger")
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
