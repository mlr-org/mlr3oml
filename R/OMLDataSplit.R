#' @title Interface to OpenML Splits
#'
#' @description
#' This is the class for OpenML splits that belong to task.
#' Note that the percentage in holdout is inverse to the ratio in mlr3.
#'
#' @section mlr3Intergration:
#' * Obtain a (instantiated) [mlr3::Resampling] object by calling `as_resampling()`.
#'
#' @examples
#' \donttest{
#' otask = OMLTask$new(31L)
#' data_split = otask$data_split
#' data_split$type
#' resampling = as_resampling(data_split)
#' }
#' @export
OMLDataSplit = R6Class("OMLDataSplit",
  public = list(
    #' @field task_id (`integer(1)`)
    #' The id of the task to which the evaluation procedure belongs.
    task_id = NULL,
    #' @field task (`OMLTask`) to which the evaluation procedure belongs.
    task = NULL,
    #' @template field_cache_dir
    cache_dir = NULL,
    #' @description
    #' Initializes an  instance of class OMLDataSplit Requires either
    #' @param task (`OMLTask) Either the OpenML Task or task_id have to be provided.
    #' @param task_id (`integer(1)`) The OpenML task ID (if no task is provided).
    #' @template param_cache
    initialize = function(task = NULL, task_id = NULL, cache = getOption("mlr3oml.cache", FALSE)) {
      assert(is.null(task) || is.null(task_id))
      assert_true(is.null(task) || inherits(task, "OMLTask"))
      if (is.null(task_id)) {
        assert_r6(task, "OMLTask")
        self$task = task
        self$task_id = task$id
      } else {
        self$task_id = assert_count(task_id, coerce = TRUE)
        self$task = OMLTask$new(self$task_id, cache = cache)
      }
      self$cache_dir = get_cache_dir(assert_flag(cache))
      initialize_cache(self$cache_dir)
    },

    #' @description
    #' Prints the object.
    print = function() {
      catf("<OMLDataSplit>")
      catf(" * Task: %i (%s)", self$task_id, self$task$data_name)
      if (self$type == "crossvalidation") {
        catf(" * Type: crossvalidation (repeats = %s, folds = %s)",
          self$parameters[name == "number_repeats", "value"][[1L]],
          self$parameters[name == "number_folds", "value"][[1L]]
        )
      } else if (self$type == "holdout") {
        catf(" * Type: holdout (test_percentage = %s)",
          self$parameters[name == "percentage", "value"][[1L]]
        )
      } else if (self$type == "leaveoneout") {
        catf(" * Type: leaveoneout")
      }
    }
  ),
  active = list(
    #' @field type (`character(1)`)\cr
    #' The type of the estimation procedure.
    type = function() self$task$desc$input$estimation_procedure$type,
    #' @field parameters (`list()`)\cr
    #' List of Parameters for the Estimation Procedure.
    parameters = function() self$task$desc$input$estimation_procedure$parameter
  ),
  private = list(
    .resampling = NULL,
    .url = NULL
  )
)

#' @importFrom mlr3 as_resampling
#' @export
as_resampling.OMLDataSplit = function(x, ...) {
  splits = cached(download_task_splits, "task_splits", x$task_id, x$task$desc,
    cache_dir = x$cache_dir
  )
  resampling = convert_data_split(x, splits)
  resampling$.__enclos_env__$private$oml_id = x$task_id
  resampling$.__enclos_env__$private$oml_hash = resampling$hash
  return(resampling)
}

convert_data_split = function(data_split, splits) {
  resampling = switch(data_split$type,
    crossvalidation = convert_cv(data_split, splits),
    leaveoneout = convert_loo(data_split, splits),
    holdout = convert_holdout(data_split, splits),
    stop("Estimation procedure not (yet) supported.")
  )
  resampling$task_hash = as_task(data_split$task)$hash
  resampling$task_nrow = data_split$task$nrow
  return(resampling)
}

convert_cv = function(data_split, splits) {
  nfolds = as.integer(data_split$parameters[name == "number_folds", "value"][[1]])
  repeats = as.integer(data_split$parameters[name == "number_repeats", "value"][[1]])

  if (repeats == 1) {
    resampling = convert_cv_simple(data_split, splits, nfolds)
  } else {
    resampling = convert_repeated_cv(data_split, splits, nfolds, repeats)
  }
  return(resampling)
}

convert_cv_simple = function(data_split, splits, nfolds) {
  # instance: [row_id | fold ]
  resampling = ResamplingCV$new()
  resampling$param_set$values$folds = nfolds
  splits_subset = splits[type == "TEST",
    list(
      row_id = as.integer(rowid) + 1L,
      fold = as.integer(fold) + 1L
    )
  ]
  resampling$instance = data.table(
    row_id = splits_subset[["row_id"]],
    fold = splits_subset[["fold"]],
    key = "fold"
  )
  return(resampling)
}

# CV without repetition
convert_repeated_cv = function(data_split, splits, nfolds, repeats) {
  # instance: [row_id | rep | fold]
  resampling = ResamplingRepeatedCV$new()
  resampling$param_set$values$folds = nfolds
  resampling$param_set$values$repeats = repeats
  splits_subset = splits[type == "TEST",
    list(
      row_id = as.integer(rowid) + 1L,
      fold = as.integer(fold) + 1L,
      rep = as.integer(rep) + 1L
    )
  ]
  resampling$instance = data.table(
    row_id = splits_subset[["row_id"]],
    rep = splits_subset[["rep"]],
    fold = splits_subset[["fold"]]
  )
  return(resampling)
}

convert_loo = function(data_split, splits) {
  # instance: vector with the test ids
  resampling = ResamplingLOO$new()
  resampling$instance = splits[type == "TEST", "rowid"][[1L]] + 1L
  return(resampling)
}

convert_holdout = function(data_split, splits) {
  resampling = ResamplingHoldout$new()
  train_ids = splits[type == "TRAIN", "rowid"][[1L]]
  test_ids = splits[type == "TEST", "rowid"][[1L]]
  # this needs to be done to ensure that instantiating a resampling with this ratio parameter
  # leads to the same train / test size (the problem is mlr3_ratio = 1 - oml_ratio + rounding)
  eps = 1 / nrow(splits)
  ratio = length(train_ids) / nrow(splits)
  ratios = c(ratio, ratio - eps, ratio + eps)
  n_trains = ratios * nrow(splits)
  valid_ratios = ratios[n_trains == length(train_ids)]
  ratio = valid_ratios[[1L]]
  resampling$param_set$values$ratio = ratio
  resampling$instance = list(
    train = train_ids,
    test = test_ids
  )
  return(resampling)
}
