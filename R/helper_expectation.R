expect_oml_flow = function(flow) {
  expect_r6(flow, "OMLFlow")
  expect_true(test_logical(flow$cache_dir) || test_character(flow$cache_dir))
  expect_count(flow$id)
  expect_list(flow$desc)
  expect_data_table(flow$parameters)
  testthat::expect_equal(
    names(flow$parameters),
    c("name", "data_type", "default_value")
  )
  expect_character(flow$dependencies)
  expect_string(flow$name, min.chars = 1L)
  if (startsWith(flow$name, "mlr3.")) {
    expect_r6(as_learner(flow), "Learner")
  } else {
    expect_warning(null_learner <<- as_learner(flow))
    expect_true(is.null(null_learner))
    expect_r6(as_learner(flow, "classif"), "LearnerClassif")
    expect_r6(as_learner(flow, "regr"), "LearnerRegr")
    expect_r6(as_learner(flow, "surv"), "LearnerSurv")
  }
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
  expect_data_table(data$data, nrow = data$nrow, ncol = data$ncol)
  expect_names(data$target_names, "strict")
  expect_subset(data$target_names, colnames(data$data))
  expect_names(data$feature_names, "strict")
  expect_subset(data$feature_names, colnames(data$data))
  expect_disjunct(data$target_names, data$feature_names)
  expect_set_equal(names(data$data), c(data$feature_names, data$target_names))
  expect_count(data$nrow)
  expect_count(data$ncol)
  expect_character(data$tags, null.ok = TRUE)
  expect_character(data$license)
  expect_r6(as_data_backend(data), "DataBackendDataTable")
  if (length(data$target_names)) {
    expect_r6(as_task(data), "Task")
  }
  expect_r6(as_data_backend(data), "DataBackendDataTable")
}

expect_oml_task = function(task) {
  expect_r6(task, "OMLTask")
  testthat::expect_true(test_logical(task$cache_dir) || test_character(task$cache_dir))
  expect_string(task$data_name, min.chars = 1L)
  expect_string(task$name, min.chars = 1L)
  expect_choice(task$task_type, oml_reflections$task_types)
  expect_list(task$desc, names = "unique")
  expect_count(task$data_id)
  expect_r6(task$data, "OMLData")
  expect_count(task$nrow)
  expect_count(task$ncol)
  expect_data_table(task$data$data, nrow = task$nrow, ncol = task$ncol)
  expect_names(task$target_names, "strict")
  expect_names(task$feature_names, "strict")
  expect_choice(task$target_names, colnames(task$data$data))
  expect_subset(task$feature_names, colnames(task$data$data))
  expect_disjunct(task$target_names, task$feature_names)
  expect_r6(task$data_split, "OMLDataSplit")
  tt = task_type_translator(task$task_type)
  if (!is.null(tt)) {
    if (tt == "regr") {
      expect_r6(as_task(task), "TaskRegr")
    }
    if (tt == "classif") {
      expect_r6(as_task(task), "TaskClassif")
    }
    if (tt == "surv") {
      expect_r6(as_task(task), "TaskSurv")
    }
  }
  if (!is.null(task$desc$input$estimation_procedure$data_splits_url)) {
    expect_r6(as_resampling(task), "Resampling")
  }
  expect_character(task$tags)
  expect_identical(task$data_id, task$data$id)
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
  expect_r6(run$data_split, "OMLDataSplit")
  expect_choice(run$task_type, oml_reflections$task_types)
  expect_data_table(run$parameter_setting)
  expect_data_table(run$prediction)
  rr = suppressWarnings(as_resample_result(run))
  expect_r6(rr, "ResampleResult")
  task_type = task_type_translator(run$task_type, to = "mlr3")
  if (!is.null(task_type)) {
    if (task_type == "classif") {
      expect_error(rr$score(msr("classif.ce")), regexp = NA)
    }
    if (task_type == "regr") {
      expect_error(rr$score(msr("regr.mse")), regexp = NA)
    }
    expect_r6(as_learner(run, task_type), "Learner")
  }
  expect_r6(as_resampling(run), "Resampling")
  expect_r6(as_task(run), "Task")
  expect_character(run$tags, null.ok = TRUE)
}

expect_oml_data_split = function(data_split) {
  expect_r6(data_split, "OMLDataSplit")
  expect_count(data_split$task_id)
  expect_r6(data_split$task, "OMLTask")
  expect_choice(data_split$type, oml_reflections$estimation_procedures)
  expect_data_table(data_split$parameters, nrows = 4L, ncols = 2L)
  expect_equal(colnames(data_split$parameters), c("name", "value"))
  expect_r6(as_resampling(data_split), "Resampling")
  expect_data_table(data_split$splits)
  expect_named(data_split$splits, c("type", "rowid", "rep", "fold"))
  expect_data_table(data_split$splits)
}

expect_oml_collection = function(collection) {
  expect_count(collection$id)
  testthat::expect_true(test_logical(collection$cache_dir) || test_character(collection$cache_dir))
  expect_list(collection$desc, names = "unique")
  expect_string(collection$name, min.chars = 1L)
  expect_character(collection$tags, null.ok = TRUE)
  expect_choice(collection$main_entity_type, c("task", "run"))
  expect_integer(collection$task_ids)
  expect_integer(collection$data_ids)
  expect_data_table(collection$tasks, key = "id", nrows = length(collection$task_ids), ncols = 12L)
  expect_named(collection$tasks,
    c("id", "task", "data", "task_type", "target", "nrow", "ncol", "missing", "numeric",
      "symbolic", "binary", "data_split"
    )
  )
  expect_data_table(collection$data, key = "id")
  expect_named(collection$data,
    c("id", "data", "name", "nrow", "ncol", "missing", "numeric", "symbolic", "binary")
  )
  if (collection$main_entity_type == "run") {
    expect_integer(collection$flow_ids)
    expect_data_table(collection$flows, key = "id", nrows = length(collection$flow_ids), ncols = 3L)
    expect_named(collection$flows,
      c("id", "flow", "name")
    )
    expect_integer(collection$run_ids)
    expect_data_table(collection$runs, key = "id", nrows = length(collection$run_ids), ncols = 6L)
    expect_named(collection$runs,
      c("id", "run", "task_type", "data", "flow", "data_split")
    )
  } else {
    testthat::expect_message(collection$runs, "Main entity type is task, returning NULL.")
    expect_true(is.null(collection$runs))
    testthat::expect_message(collection$flows, "Main entity type is task, returning NULL.")
    expect_true(is.null(collection$flows))
  }
  expect_benchmark_result(suppressWarnings(as_benchmark_result(collection)))
}
