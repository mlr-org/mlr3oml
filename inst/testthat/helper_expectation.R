task_type_translator = function(tt, to = "mlr3") {
  if (to == "mlr3") {
    converted = switch(tt,
      "Supervised Regression" = "regr",
      "Supervised Classification" = "classif",
      "Survival Analysis" = "surv",
      "Clustering" = "clust",
      NULL
    )
  }
  if (to == "oml") {
    converted = switch(tt,
      "regr" = "Supervised Regression",
      "classif" = "Supervised Classification",
      "surv" = "Survival Analysis",
      "clust" = "Clustering",
      NULL
    )
  }
  return(converted)
}



expect_oml_flow = function(flow) {
  expect_r6(flow, "OMLFlow")
  expect_true(test_logical(flow$cache_dir) || test_character(flow$cache_dir))
  expect_count(flow$id)
  expect_list(flow$desc)
  expect_data_table(flow$parameter)
  testthat::expect_equal(
    names(flow$parameter),
    c("name", "data_type", "default_value")
  )
  expect_character(flow$dependencies)
  expect_string(flow$name, min.chars = 1L)
  expect_r6(as_learner(flow, "classif"), "LearnerClassif")
  expect_r6(as_learner(flow, "regr"), "LearnerRegr")
}

expect_oml_data = function(data) {
  expect_r6(data, "OMLData")
  expect_true(test_logical(data$cache_dir) || test_character(data$cache_dir))
  expect_count(data$quality("NumberOfInstances"))
  expect_string(data$name, min.chars = 1L)
  expect_list(data$desc, names = "unique")
  expect_data_table(data$qualities, ncols = 2L)
  expect_equal(colnames(data$qualities), c("name", "value"))
  expect_data_table(data$features)
  expect_data_table(data$data, nrows = data$nrow, ncols = data$ncol)
  expect_names(data$target_names, "strict")
  expect_subset(data$target_names, colnames(data$data))
  expect_names(data$feature_names, "strict")
  expect_subset(data$feature_names, colnames(data$data))
  expect_disjunct(data$target_names, data$feature_names)
  # can't do this because after OpenML's parquet transition some features seem to be missing
  # expect_set_equal(names(data$data), c(data$feature_names, data$target_names))
  expect_count(data$nrow)
  expect_count(data$ncol)
  expect_character(data$tags, null.ok = TRUE)
  expect_character(data$license)
  expect_character(data$server)
  if (length(data$target_names)) {
    expect_r6(as_task(data), "Task")
  }
  expect_flag(data$parquet)
  backend = as_data_backend(data)
  expect_r6(backend, paste0("DataBackend"))
}

expect_oml_task = function(task) {
  expect_r6(task, "OMLTask")
  testthat::expect_true(test_logical(task$cache_dir) || test_character(task$cache_dir))
  expect_string(task$data_name, min.chars = 1L)
  expect_string(task$name, min.chars = 1L)
  expect_character(task$task_type, len = 1L)
  expect_list(task$desc, names = "unique")
  expect_count(task$data_id)
  expect_r6(task$data, "OMLData")
  expect_count(task$nrow)
  expect_count(task$ncol)
  expect_data_table(task$data$data, nrows = task$nrow, ncols = task$ncol)
  expect_names(task$target_names, "strict")
  expect_names(task$feature_names, "strict")
  expect_choice(task$target_names, colnames(task$data$data))
  expect_subset(task$feature_names, colnames(task$data$data))
  expect_disjunct(task$target_names, task$feature_names)
  tt = task_type_translator(task$task_type)
  if (!is.null(tt)) {
    if (tt == "regr") {
      expect_r6(as_task(task), "TaskRegr")
    } else if (tt == "classif") {
      expect_r6(as_task(task), "TaskClassif")
    }
  }
  if (!is.null(task$desc$input$estimation_procedure$task_splits_url)) {
    expect_r6(as_resampling(task), "Resampling")
  }
  expect_character(task$tags, null.ok = TRUE)
  expect_identical(task$data_id, task$data$id)
  task_splits = task$task_splits
}

expect_oml_run = function(run) {
  expect_r6(run, "OMLRun")
  expect_count(run$id)
  testthat::expect_true(test_logical(run$cache_dir) || test_character(run$cache_dir))
  expect_list(run$desc, names = "unique")
  expect_count(run$flow_id)
  expect_r6(run$flow, "OMLFlow")
  expect_count(run$task_id)
  expect_r6(run$task, "OMLTask")
  expect_count(run$data_id)
  expect_r6(run$data, "OMLData")
  expect_character(run$task_type, len = 1L)
  expect_data_table(run$parameter_setting)
  expect_data_table(run$prediction)
  expect_data_table(run$evaluation)
  task_type = task_type_translator(run$task_type, to = "mlr3")

  if (!is.null(task_type)) {
    rr = suppressWarnings(as_resample_result(run))

    if (task_type %in% c("regr", "classif")) {
      expect_r6(rr, "ResampleResult")
    }
    if (task_type == "classif") {
      expect_error(rr$score(msr("classif.ce")), regexp = NA)
    }
    if (task_type == "regr") {
      expect_error(rr$score(msr("regr.mse")), regexp = NA)
    }
  }
  expect_r6(as_resampling(run), "Resampling")
  expect_r6(as_task(run), "Task")
  expect_character(run$tags, null.ok = TRUE)
}

expect_oml_collection = function(collection) {
  expect_count(collection$id)
  testthat::expect_true(test_logical(collection$cache_dir) || test_character(collection$cache_dir))
  expect_list(collection$desc, names = "unique")
  expect_string(collection$name, min.chars = 1L)
  expect_choice(collection$main_entity_type, c("task", "run"))
  expect_integer(collection$task_ids)
  expect_integer(collection$data_ids)
  if (collection$main_entity_type == "run") {
    expect_integer(collection$flow_ids)
    expect_integer(collection$run_ids)
    expect_r6(suppressWarnings(as_benchmark_result(collection)), "BenchmarkResult")
  }
}
