convert_task_splits = function(x, task = NULL) {
  estimation_procedure = x$estimation_procedure
  task_splits = x$task_splits
  if (estimation_procedure$type == "testthentrain") {
    stopf("Not supported yet.")
    # https://github.com/openml/openml.org/issues/252
  }

  resampling = switch(estimation_procedure$type,
    crossvalidation = convert_cv(estimation_procedure, task_splits),
    leaveoneout = convert_loo(estimation_procedure, task_splits),
    holdout = convert_holdout(estimation_procedure, task_splits),
    stop("Estimation procedure not (yet) supported.")
  )
  # this is expensive when we do no caching because the dataset has to be downloaded
  if (is.null(task)) {
    resampling$task_hash = as_task(x)$hash
  } else {
    resampling$task_hash = task$hash
  }
  resampling$task_nrow = task_splits$task$nrow

  resampling$.__enclos_env__$private$oml$id = x$id
  resampling$.__enclos_env__$private$oml$hash = resampling$hash

  return(resampling)
}


convert_cv = function(task_split, task_splits) {
  nfolds = as.integer(task_split$parameter[get("name") == "number_folds", "value"][[1]])
  repeats = as.integer(task_split$parameter[get("name") == "number_repeats", "value"][[1]])

  if (repeats == 1) {
    resampling = convert_cv_simple(task_split, task_splits, nfolds)
  } else {
    resampling = convert_repeated_cv(task_split, task_splits, nfolds, repeats)
  }
  return(resampling)
}

convert_cv_simple = function(task_split, task_splits, nfolds) {
  # instance: [row_id | fold ]
  resampling = ResamplingCV$new()
  resampling$param_set$values$folds = nfolds
  task_splits_subset = task_splits[get("type") == "TEST",
    list(
      row_id = as.integer(get("rowid")) + 1L,
      fold = as.integer(get("fold")) + 1L
    )
  ]
  resampling$instance = data.table(
    row_id = task_splits_subset[["row_id"]],
    fold = task_splits_subset[["fold"]],
    key = "fold"
  )
  return(resampling)
}

# CV without repetition
convert_repeated_cv = function(task_split, task_splits, nfolds, repeats) {
  # instance: [row_id | rep | fold]
  resampling = ResamplingRepeatedCV$new()
  resampling$param_set$values$folds = nfolds
  resampling$param_set$values$repeats = repeats
  task_splits_subset = task_splits[get("type") == "TEST",
    list(
      row_id = as.integer(get("rowid")) + 1L,
      fold = as.integer(get("fold")) + 1L,
      rep = as.integer(get("rep")) + 1L
    )
  ]
  resampling$instance = data.table(
    row_id = task_splits_subset[["row_id"]],
    rep = task_splits_subset[["rep"]],
    fold = task_splits_subset[["fold"]]
  )
  return(resampling)
}

convert_loo = function(task_split, task_splits) {
  # instance: vector with the test ids
  resampling = ResamplingLOO$new()
  resampling$instance = task_splits[get("type") == "TEST", "rowid"][[1L]] + 1L
  return(resampling)
}

convert_holdout = function(task_split, task_splits) {
  resampling = ResamplingHoldout$new()
  train_ids = task_splits[get("type") == "TRAIN", "rowid"][[1L]] + 1L
  test_ids = task_splits[get("type") == "TEST", "rowid"][[1L]] + 1L
  # this needs to be done to ensure that instantiating a resampling with this ratio parameter
  # leads to the same train / test size (the problem is mlr3_ratio = 1 - oml_ratio + rounding)
  eps = 1 / nrow(task_splits)
  ratio = length(train_ids) / nrow(task_splits)
  ratios = c(ratio, ratio - eps, ratio + eps)
  n_trains = ratios * nrow(task_splits)
  valid_ratios = ratios[n_trains == length(train_ids)]
  ratio = valid_ratios[[1L]]
  resampling$param_set$values$ratio = ratio
  resampling$instance = list(
    train = train_ids,
    test = test_ids
  )
  return(resampling)
}
