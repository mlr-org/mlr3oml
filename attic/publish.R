#' @title Publish on OpenML
#'
#' @description
#' Publishes an object to OpenML.
#' Methods exist for:
#' * [mlr3::Learner] - Is published as a Flow
#' * [mlr3::ResampleResult] - Is published as a Run, including publishing the Learner.
#' * `"character"`  Depending on if it is `"collection"` or `"task"` a collection
#'    or task is being published
#'   [mlr3::Learner] (as a Flow) or [mlr3::ResampleResult] (as a Run) to OpenML.
#'   Currently only resample results with atomic parameters can be uploaded, as it has to be
#'   possible to represent the parameter values in json format.
#' @param x (any)\cr Object to publish to OpenML.
#'   A [Learner] is published as a flow.
#' @param confirm (`logical(1)`) Whether to confirm the upload.
#' @param ... Additional arguments.
#' @param main_entity_type (`character(1)`)\cr
#'   Only relevant if `x` is `"collection"`. Must be `"run"` or `"task"`.
#' @param name (`character(1)`)\cr
#'   Only relevant if `x` is `"collection"` or a `data.frame`. Name of the collection / dataset.
#' @param desc (`character(1)`)\cr
#'   Only relevant if `x` is `"collection"` or a `data.frame`. Description of the collection /
#'   dataset.
#' @param alias (`character(1)`)\cr
#'   Only relevant if `x` is `"collection"`. Alias for the study.
#' @param licence (`character(1)`)\cr
#'   Only relevant if `x` is a `data.frame`. The license for the dataset.
#' @param default_target (`character(1)`)\cr
#'   Only relevant if `x` is a `data.frame`. The default target for the dataset.
#' @param row_identifier (`character(1)`)\cr
#'   Only relevant if `x` is a `data.frame`. The row identifier for the dataset.
#' @param ignore (`character(1)`)\cr
#'   Only relevant if `x` is a `data.frame`. The ignore columns for the dataset.
#' @param citation (`character(1)`)\cr
#'   Only relevant if `x` is a `data.frame`. The citation for the dataset.
#' @param original_data_url (`character(1)`)\cr
#'   Only relevant if `x` is a `data.frame`. The citation for the dataset.
#' @param data_id (`integer(1)`)\cr
#'   Only relevant if `x` is `"task"`. Data id for the task.
#' @param type (`character(1)`)\cr
#'   Only relevant if `x` is `"task"`. Must be `"classif"` or `"regr"`.
#' @param estimation_procredure (`integer(1)`)\cr
#'   Only relevant if `x` is `"task"`. The estimation procedure.
#'   See here: https://www.openml.org/search?type=measure&measure_type=estimation_procedure.
#' @param target (`character(1)`)\cr
#'   Only relevant if `x` is `"task"`. The target variable.
#'
#' @details
#' This is a generic function.
#' @name publish
#' @export
publish = function(x, confirm = TRUE, api_key = get_api_key(), ...) {
  # if (is.na(api_key)) stop("API key required for upload.")
  # assert_character(api_key, any.missing = FALSE, len = 1)
  #
  if (confirm) {
    ask_confirmation()
  }
  id = UseMethod("publish")
  x$.__enclos_env__$private$oml$id = id
  return(id)
}

#' @export
publish.default = function(x, ...) { # nolint
  stopf("Objects of class %s cannot be published.", class(x)[[1]])
}

learner_is_publishable = function(learner) {
  is.null(get_private(learner)$oml$info)
}

#' @rdname publish
#' @export
publish.character = function(x, main_entity_type = NULL, name = NULL, desc = NULL, alias = NULL,
  data_id = NULL, type = NULL, estimation_procedure = NULL, target = NULL, api_key, ...) {
  if (x %in% c("collection", "study")) {
    invoke(publish_collection, .args = as.list(match.call())[-1L])
  } else if (x == "task") {
    invoke(publish_task, .args = as.list(match.call())[-1L])
  } else {
    stopf("Value for x must be 'collection' or 'task'.")
  }
}

ask_confirmation = function(action = "publish") {
  user_input = readline(sprintf("Are you sure you want to %s on OpenML? (y/n)  ", action))
  if (user_input != "y") stop("Exiting since you did not press y.")
}

